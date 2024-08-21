
rm(list=ls())
library("here")
library("dplyr")
library("roll")

source(file.path(here(), 'src', 'models', 'utils.R'))

window_size <- 2 * 52 
freq <- "weekly"

K <- 1
initial_cash <- 100
trade_size <- 10

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

    return(list(betas = betas, residuals = residuals))
}

data <- load_and_resample_currencies(freq=freq) %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")

x_data <- data %>% select(KRW, MYR, CNY, THB, IDR, TWD, INR, JPY, AUD, GBP, EUR)
y_data <- data %>% select(SGD)

rolling_results_with_intercept <- rolling_ols(y_data, x_data, window_size, intercept = TRUE)

rolling_results_no_intercept <- rolling_ols(y_data, x_data, window_size, intercept = FALSE)

# ect_with_intercept <- scale(rolling_results_with_intercept$residuals)
# ect_no_intercept <- scale(rolling_results_no_intercept$residuals)

ect_with_intercept <- rolling_results_with_intercept$residuals %>% as.data.table()
ect_no_intercept <- rolling_results_no_intercept$residuals %>% as.data.table()

n = dim(ect_with_intercept)[1]
mean_data_with_intercept <- apply(ect_with_intercept, 2, function(x) {roll_mean(x, width = window_size, min_obs = window_size)})
sd_data_with_intercept <- apply(ect_with_intercept, 2, function(x) {roll_sd(x, width = window_size, min_obs = window_size)})
ect_with_intercept <- ((ect_with_intercept - mean_data_with_intercept) / sd_data_with_intercept)
ect_with_intercept <- ect_with_intercept %>% drop_na() %>% as.matrix()

n = dim(ect_no_intercept)[1]
mean_data_with_no_intercept <- apply(ect_no_intercept, 2, function(x) {roll_mean(x, width = window_size, min_obs = window_size)})
sd_data_with_no_intercept <- apply(ect_no_intercept, 2, function(x) {roll_sd(x, width = window_size, min_obs = window_size)})
ect_no_intercept <- ((ect_no_intercept - mean_data_with_no_intercept) / sd_data_with_no_intercept)
ect_no_intercept <- ect_no_intercept %>% drop_na() %>% as.matrix()

# TRADING STRATEGY

trade_strategy <- function(ect, initial_cash, trade_size, K) {
    position <- 0
    cash <- initial_cash
    portfolio_value <- numeric(length(ect))
    positions <- numeric(length(ect))
    portfolio_value[1] <- initial_cash

    for (i in 2:length(ect)) {
        if (ect[i-1] <= -K && position == 0) {
            position <- trade_size
            cash <- cash - (position * ect[i-1])
        } else if (ect[i-1] >= K && position == 0) {
            position <- -trade_size
            cash <- cash - (position * ect[i-1] )
        } else if (ect[i-1] * sign(position) > 0 && abs(ect[i-1] ) < abs(K)) {
            cash <- cash + (position * ect[i-1])
            position <- 0
        }
        portfolio_value[i] <- cash + (position * ect[i-1])
        positions[i] <- position
    }
    
    list(portfolio_value = portfolio_value, cash = cash, position = position)
}

strategy_no_intercept <- trade_strategy(ect_no_intercept, initial_cash, trade_size, K)
strategy_with_intercept <- trade_strategy(ect_with_intercept, initial_cash, trade_size, K)

strategy_returns_no_intercept <- (strategy_no_intercept$portfolio_value - lag(strategy_no_intercept$portfolio_value)) / lag(strategy_no_intercept$portfolio_value) # diff(strategy_no_intercept$portfolio_value) / strategy_no_intercept$portfolio_value[-length(strategy_no_intercept$portfolio_value)]
annualized_return_no_intercept <- mean(strategy_returns_no_intercept, na.rm = TRUE) * 52
annualized_volatility_no_intercept <- sd(strategy_returns_no_intercept, na.rm = TRUE) * sqrt(52)
sharpe_ratio_no_intercept <- annualized_return_no_intercept / annualized_volatility_no_intercept

strategy_returns_with_intercept <- (strategy_with_intercept$portfolio_value - lag(strategy_with_intercept$portfolio_value)) / lag(strategy_with_intercept$portfolio_value) # diff(strategy_with_intercept$portfolio_value) / strategy_with_intercept$portfolio_value[-length(strategy_with_intercept$portfolio_value)]
annualized_return_with_intercept <- mean(strategy_returns_with_intercept, na.rm = TRUE) * 52
annualized_volatility_with_intercept <- sd(strategy_returns_with_intercept, na.rm = TRUE) * sqrt(52)
sharpe_ratio_with_intercept <- annualized_return_with_intercept / annualized_volatility_with_intercept

print(paste0("SharpeNoIntercept=", sharpe_ratio_no_intercept, "      SharpeIntercept=", sharpe_ratio_with_intercept))

# df_no_intercept_value <- data.frame(Time = Date, PortfolioValue = strategy_no_intercept$portfolio_value, Model = "No Intercept")
# df_with_intercept_value <- data.frame(Time = Date, PortfolioValue = strategy_with_intercept$portfolio_value, Model = "With Intercept")
# df_combined_value <- bind_rows(df_no_intercept_value, df_with_intercept_value)

# ggplot(df_combined_value, aes(x = Time, y = PortfolioValue, color = Model)) +
#   geom_line(size = 1) +
#   labs(title = "Mean Reversion Strategy Performance",
#        x = "Time",
#        y = "Portfolio Value (USD)") +
#   theme_minimal() +
#   theme(text = element_text(size = 12)) +
#   scale_color_manual(values = c("blue", "red")) +
#   annotate("text", x = max(df_combined_value$Time) - 10, y = min(df_combined_value$PortfolioValue), 
#            label = sprintf("Sharpe Ratio (No Intercept): %.2f", sharpe_ratio_no_intercept), 
#            color = "blue", hjust = 1) +
#   annotate("text", x = max(df_combined_value$Time) - 10, y = min(df_combined_value$PortfolioValue) + 50, 
#            label = sprintf("Sharpe Ratio (With Intercept): %.2f", sharpe_ratio_with_intercept), 
#            color = "red", hjust = 1)