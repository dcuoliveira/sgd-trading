rm(list=ls())
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("MTS")
library("lubridate")
library("optparse")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))
source(file.path(getwd(), 'src', 'plots', 'plot_funcs.R'))

# define command-line options
option_list <- list(
  make_option(c("--model_name"), type = "character", help = "Model name for output", default = "dlm"),
  make_option(c("--output_path"), type = "character", help = "Output path", default = file.path(here(), 'src', 'data', 'outputs')),
  make_option(c("--frequency"), type = "character", help = "Frequency to parse the data", default = "weekly"),
  make_option(c("--intercept"), type = "logical", help = "Intercept", default = FALSE),
  make_option(c("--scale_type"), type = "character", help = "Scale type", default = "rolling_scale"),
  make_option(c("--strategy_type"), type = "character", help = "Strategy type", default = "scale"),
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 52 * 4),
  make_option(c("--target"), type = "character", help = "Target variable name", default = "SGD"),
  make_option(c("--betas_type"), type = "character", help = "Betas type", default = NULL),
  make_option(c("--threshold"), help = "Threshold to the used on the strategy", default = 2)
)

# create a parser object
parser <- OptionParser(option_list = option_list)

# parse the arguments
args <- parse_args(parser)

MODEL <- args$model_name
STRATEGY_TYPE <- args$strategy_type
OUTPUT_PATH <- file.path(args$output_path, MODEL)
TARGET <- args$target
SCALE_TYPE <- args$scale_type
BETAS_TYPE <- "filter"
FREQ = args$frequency
WINDOW_SIZE <- args$window_size
THRESHOLD <- as.numeric(args$threshold)
INTERCEPT <- args$intercept
STRATEGY_TYPE <- args$strategy_type

if (FREQ == "monthly"){
  FREQ_INT <- 12
}else if (FREQ == "weekly"){
  FREQ_INT <- 52
}

if (INTERCEPT == T){
  intercept_tag <- "intercept"
}else{
  intercept_tag <- "nointercept"
}

# prices data
prices_df <- load_and_resample_currencies(freq=FREQ) %>% mutate(date=ymd(date)) # %>% filter(date >= "2006-01-01")

if (MODEL == "dlm"){
  
  # load model output
  model_out <- readRDS(file = file.path(OUTPUT_PATH, SCALE_TYPE, paste0("model_results_", FREQ, "_", WINDOW_SIZE, "_", intercept_tag, ".rds")))
  
  residuals_df = model_out$residuals$res %>% as.data.table()
  n = dim(residuals_df)[1]
  # mean <- rollapply(residuals_df$residual, width = WINDOW_SIZE, FUN = mean, align = "right", partial = TRUE)
  std <- rollapply(residuals_df$residual, width = WINDOW_SIZE, FUN = sd, align = "right", partial = TRUE)
  # residuals <- ((residuals_df$residual - mean) / sd)
  # residuals <- residuals %>% as.matrix()
  # cointegration_error_df = data.table(date=residuals_df$date, residual=residuals_df$residual) %>%
  #   rename(residual=`residual.V1`) %>%
  #   mutate(ub=THRESHOLD, lb=-THRESHOLD)
  cointegration_error_df = residuals_df %>% mutate(std=std) %>% mutate(ub=THRESHOLD*std, lb=-THRESHOLD*std)
  
  # betas
  if (BETAS_TYPE == "smooth"){
    betas_df <- model_out$smooth$s
  }else{
    betas_df <- model_out$filter$m
  }
  
}else if (MODEL == "rolling-ols"){
  
  # load model output
  model_out <- readRDS(file = file.path(OUTPUT_PATH, SCALE_TYPE, paste0("model_results_", FREQ, "_", WINDOW_SIZE, "_", intercept_tag, ".rds")))
  
  # cointegration error
  residuals = model_out$residuals$residual %>% as.data.table()
  n = dim(residuals)[1]
  mean <- apply(residuals, 2, function(x) {roll_mean(x, width = WINDOW_SIZE, min_obs = WINDOW_SIZE)})
  sd <- apply(residuals, 2, function(x) {roll_sd(x, width = WINDOW_SIZE, min_obs = WINDOW_SIZE)})
  residuals <- ((residuals - mean) / sd)
  residuals <- residuals %>% as.matrix()
  cointegration_error_df = data.table(date=model_out$residuals$date, residual=residuals) %>%
    rename(residual=`residual..`) %>%
    mutate(ub=THRESHOLD, lb=-THRESHOLD)
  
  # betas
  betas_df <- model_out$model$coefs %>% as.data.table() %>%
    mutate(date=cointegration_error_df$date) %>% select(date, everything()) %>% as.data.table()
  
}

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

strategy <- trade_strategy(ect = cointegration_error_df$residual,
                           initial_cash = 100,
                           trade_size = 10,
                           K = 1)

strategy_returns <- (strategy$portfolio_value - lag(strategy$portfolio_value)) / lag(strategy$portfolio_value)
annualized_return <- mean(strategy_returns, na.rm = TRUE) * 52
annualized_volatility <- sd(strategy_returns, na.rm = TRUE) * sqrt(52)
sharpe_ratio <- annualized_return / annualized_volatility
print(sharpe_ratio)




