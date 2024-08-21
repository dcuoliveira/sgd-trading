rm(list=ls())
library("here")
library("dplyr")

source(file.path(here(), 'src', 'models', 'utils.R'))

window_size <- 2 * 52 
freq <- "weekly"

rolling_ols <- function(y, x, window_size, intercept = TRUE) {
    n <- dim(y)[1]
    betas <- matrix(NA, nrow = n, ncol = ifelse(intercept, ncol(x) + 1, ncol(x)))
    residuals <- numeric(n)
    
    for (i in window_size:n) {
        window_x <- x[(i - window_size + 1):i, , drop = FALSE] %>% as.matrix()
        window_y <- y[(i - window_size + 1):i, ]

        if (intercept) {
            model <- lm(window_y ~ window_x)
        } else {
            model <- lm(window_y ~ window_x + 0)
        }

        betas[i, ] <- coef(model) %>% as.matrix() %>% t()
        residuals[i] <- model$residuals[length(model$residuals)]
    }
    # browser()
    return(list(betas = betas, residuals = residuals))
}

data <- load_and_resample_currencies(freq=freq) %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")

x_data <- data %>% select(KRW, MYR, CNY, THB, IDR, TWD, INR, JPY, AUD, GBP, EUR)
y_data <- data %>% select(SGD)

rolling_results_with_intercept <- rolling_ols(y_data, x_data, window_size, intercept = TRUE)

rolling_results_no_intercept <- rolling_ols(y_data, x_data, window_size, intercept = FALSE)

ect_with_intercept <- scale(rolling_results_with_intercept$residuals)
ect_no_intercept <- scale(rolling_results_no_intercept$residuals)

write.csv(as.data.table(rolling_results_with_intercept$residuals) %>% as.data.table("cresid"="V1"), file = file.path(here(), 'ect_with_intercept.csv'))
write.csv(as.data.table(rolling_results_no_intercept$residuals) %>% as.data.table("cresid"="V1"), file = file.path(here(), 'ect_no_intercept.csv'))

df_no_intercept <- data.frame(Time = Date, ECT = ect_no_intercept, Model = "No Intercept")
df_with_intercept <- data.frame(Time = Date, ECT = ect_with_intercept, Model = "With Intercept")
df_combined_ect <- bind_rows(df_no_intercept, df_with_intercept)

ggplot(df_combined_ect, aes(x = Time, y = ECT, color = Model)) +
  geom_line(size = 1) +
  labs(title = "Standardized Error Correction Terms (ECT) Comparison",
       x = "Time",
       y = "Standardized ECT") +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("blue", "red"))