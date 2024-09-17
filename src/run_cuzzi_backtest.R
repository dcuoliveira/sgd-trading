rm(list=ls())
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("reshape2")
library("dlm")
library("gridExtra")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))
source(file.path(getwd(), 'src', 'plots', 'plot_funcs.R'))
source(file.path(getwd(), 'src', 'func.R'))

MODEL <- "dlm"
OUTPUT_PATH <- file.path(getwd(), 'src', 'data', 'outputs', MODEL)
WINDOW_SIZE <- 52 * 10
INTERCEPT <- FALSE
TARGET <- "SGD"
SCALE_TYPE <- "rolling_scale"
FREQ <- "weekly"

data <- load_and_resample_currencies(freq=FREQ) %>% mutate(date=ymd(date)) # %>% filter(date >= "2006-01-01")
data_orig <- data
data <- data %>% select(-date)

if (INTERCEPT == T){
  model_formula <- paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + "), "+1")
}else{
  model_formula <- paste(paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + ")), " -1")
}

y <- data[,TARGET]
X <- data %>% select(-!!(TARGET))

lm_model <- lm(formula = model_formula,
               data = data)
betas_variaces <- (summary(lm_model)$coefficients[,2]) ** 2
residual_variance <- ((summary(lm_model)$sigma)) ** 2

m <- NCOL(X)
dlm_model <- dlmModReg(X, addInt = INTERCEPT)
dlm_model$FF <- dlm_model$FF
dlm_model$GG <- dlm_model$GG * 1
dlm_model$W <- diag(betas_variaces)
dlm_model$V <- residual_variance 
dlm_model$m0 <- rep(0,2 * m)
dlm_model$C0 <- diag(1e7, nr = 2 * m)

dlm_filter <- dlmFilter(y, dlm_model)
dlm_smooth <- dlmSmooth(dlm_filter)
dlm_filter_residual <- residuals(dlm_filter)
dlm_filter_residual$res <- as.data.frame(dlm_filter_residual$res)
colnames(dlm_filter_residual$res) <- "residual"
dlm_filter_residual$res$date <- ymd(rownames(X))
dlm_filter_residual$res <- dlm_filter_residual$res %>% select(date, everything())

dlm_smooth$s <- dlm_smooth$s %>% as.data.table()
if (INTERCEPT == T){
  colnames(dlm_smooth$s) <- append("intercept", colnames(X))
}else{
  colnames(dlm_smooth$s) <- colnames(X)
}
dlm_smooth$s <- dlm_smooth$s[2:dim(dlm_smooth$s)[1], ]
dlm_smooth$s$date <- ymd(rownames(X))
dlm_smooth$s <- dlm_smooth$s %>% select(date, everything())

dlm_filter$m <- dlm_filter$m %>% as.data.table()
if (INTERCEPT == T){
  colnames(dlm_filter$m) <- append("intercept", colnames(X))
}else{
  colnames(dlm_filter$m) <- colnames(X)
}
dlm_filter$m <- dlm_filter$m[2:dim(dlm_filter$m)[1], ]
dlm_filter$m$date <- ymd(rownames(X))
dlm_filter$m <- dlm_filter$m %>% select(date, everything())

dlmout <- list(filter=dlm_filter,
               smooth=dlm_smooth,
               residuals=dlm_filter_residual)

if (INTERCEPT == T){
  intercept_tag <- "intercept"
}else{
  intercept_tag <- "nointercept"
}

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
saveRDS(dlmout, file.path(OUTPUT_PATH, SCALE_TYPE, paste0("model_results_", FREQ, "_", WINDOW_SIZE, "_", intercept_tag, ".rds")))

# dlm residuals datatable
res <- dlmout$residuals$res %>% as.data.table() %>% mutate(date=ymd(date))

# # plot residuals
# ggplot(res, aes(x=date, y=residual)) +
#   geom_line(color="blue") +
#   labs(title="Residuals Over Time",
#        x="Date",
#        y="Residuals") +
#   theme_minimal(base_size=15)

n <- length(dlmout[["residuals"]][["res"]][,1])
dt <- dlmout[["residuals"]][["res"]][,1][378:n] # [400:1364]
ect <- dlmout[["residuals"]][["res"]][,2][378:n] # [400:1364]
k <- 2

# # plot residuals
# ggplot(res %>% filter(date <= "1999-07-02"), aes(x=date, y=residual)) +
#   geom_line(color="blue") +
#   labs(title="Residuals Over Time",
#        x="Date",
#        y="Residuals") +
#   theme_minimal(base_size=15)
# 
# # ggplot currencies
# ggplot(data_orig %>% filter(date <= "1999-07-02"), aes(x=date, y=KRW)) +
#   geom_line(color="blue") +
#   labs(title="SGD Over Time",
#        x="Date",
#        y="KRW") +
#   theme_minimal(base_size=15)

portfolio_value <- trade_strategy(ect=ect, k=k)
thresh <- sd(ect) * k

data_plot <- data.frame(Date = dt, ECT = ect, PortfolioValue = portfolio_value)

p1 <- ggplot(data_plot, aes(x = Date)) +
  geom_line(aes(y = ECT), color = "blue", size = 1) + 
  geom_hline(yintercept = thresh, color = "red", linetype = "dashed", size = 1) + 
  geom_hline(yintercept = -thresh, color = "red", linetype = "dashed", size = 1) +  
  labs(title = "ECT Over Time with Thresholds",
       x = "Date",
       y = "ECT") +
  theme_minimal(base_size = 15)

p2 <- ggplot(data_plot, aes(x = Date, y = PortfolioValue)) +
  geom_line(color = "black", size = 1) +  
  labs(title = "Portfolio Value Over Time",
       x = "Date",
       y = "Portfolio Value") +
  theme_minimal(base_size = 15)

grid.arrange(p1, p2, ncol = 1)

portfolio_returns <- diff(portfolio_value) / lag(portfolio_value, 1)
portfolio_returns <- na.omit(portfolio_returns)
trading_weeks_per_year <- 52
annualized_return <- prod(1 + portfolio_returns)^(trading_weeks_per_year / length(portfolio_returns)) - 1
annualized_volatility <- sd(portfolio_returns) * sqrt(trading_weeks_per_year)
risk_free_rate <- 0
sharpe_ratio <- (annualized_return - risk_free_rate) / annualized_volatility
cat("Annualized Return:", annualized_return, "\n")
cat("Annualized Volatility:", annualized_volatility, "\n")
cat("Sharpe Ratio:", sharpe_ratio, "\n")