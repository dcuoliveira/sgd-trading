rm(list=ls())
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("MTS")
library("lubridate")
library("optparse")
library("gridExtra")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))
source(file.path(getwd(), 'src', 'models', 'backtest.R'))
source(file.path(getwd(), 'src', 'plots', 'plot_funcs.R'))

# define command-line options
option_list <- list(
  make_option(c("--model_name"), type = "character", help = "Model name for output", default = "rolling-ols"),
  make_option(c("--output_path"), type = "character", help = "Output path", default = file.path(here(), 'src', 'data', 'outputs')),
  make_option(c("--frequency"), type = "character", help = "Frequency to parse the data", default = "weekly"),
  make_option(c("--intercept"), type = "logical", help = "Intercept", default = FALSE),
  make_option(c("--scale_type"), type = "character", help = "Scale type", default = "rolling_scale"),
  make_option(c("--strategy_type"), type = "character", help = "Strategy type", default = "ewma"),
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 52 * 2),
  make_option(c("--target"), type = "character", help = "Target variable name", default = "SGD"),
  make_option(c("--betas_type"), type = "character", help = "Betas type", default = "filter"),
  make_option(c("--threshold"), help = "Threshold to the used on the strategy", default = 1)
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
prices_df <- load_and_resample_currencies(freq=FREQ, invert_quotes=FALSE) %>% mutate(date=ymd(date))

# load model output
model_out <- readRDS(file = file.path(OUTPUT_PATH, SCALE_TYPE, paste0("model_results_", FREQ, "_", WINDOW_SIZE, "_", intercept_tag, ".rds")))

# betas
if (BETAS_TYPE == "smooth"){
  betas_df <- model_out$smooth$s %>% tidyr::drop_na()
}else{
  betas_df <- model_out$filter$m %>% tidyr::drop_na()
}

if (INTERCEPT == T){
  betas_df <- betas_df %>% select(-intercept)
}

residuals_df = model_out$residuals$res %>% as.data.table() %>% tidyr::drop_na()

start_date <- min(min(residuals_df$date), min(betas_df$date))
end_date <- min(max(residuals_df$date), max(betas_df$date))
cointegration_error_df <- residuals_df %>% filter(date >= start_date & date <= end_date) %>% as.data.table()
betas_df <- betas_df %>% filter(date >= start_date & date <= end_date) %>% as.data.table()

cointegration_error_df <- cointegration_error_df %>%
  drop_na() %>%
  mutate(residual_vol=EWMAvol(residual,lambda = 0.8)$Sigma.t) %>%
  mutate(ub=THRESHOLD*sqrt(residual_vol), lb=-THRESHOLD*sqrt(residual_vol)) %>%
  select(-residual_vol) %>%
  as.data.table() %>%
  tidyr::drop_na()

start_date <- min(min(cointegration_error_df$date), min(cointegration_error_df$date))

prices_df <- prices_df %>% filter(date >= start_date & date <= end_date) %>% as.data.table()
betas_df <- betas_df %>% filter(date >= start_date & date <= end_date) %>% as.data.table()

# prepare signal
# signal <- build_signal_from_bounds(cointegration_error_df)
signal <- data.table(
  date = cointegration_error_df$date
)
signal[, "signal" := ifelse(
  cointegration_error_df$residual >= cointegration_error_df$ub, -1,
  ifelse(cointegration_error_df$residual < cointegration_error_df$lb, 1, 0)
)]

# run backtest
output <- run_backtest(signal=signal,
                       betas=betas_df,
                       prices=prices_df,
                       target_name="SGD",
                       ret_type="simple")
pnl_df <- output$pnl
cum_pnl_df <- output$cum_pnl
returns_df <- output$returns
cum_returns_df <- output$cum_returns
vol_adj_returns_df <- output$vol_adj_portfolio_returns
cum_vol_adj_returns_df <- output$cum_vol_adj_portfolio_returns
positions_df <- output$positions
prices <- output$prices

outputs <- list(
  cointegration_error=cointegration_error_df,
  positions=positions_df,
  pnl=pnl_df,
  cum_pnl=cum_pnl_df,
  returns=returns_df,
  cum_returns=cum_returns_df,
  vol_adj_returns=vol_adj_returns_df,
  cum_vol_adj_returns=cum_vol_adj_returns_df,
  prices=prices
)

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, SCALE_TYPE), showWarnings = FALSE)
saveRDS(outputs, file.path(OUTPUT_PATH, SCALE_TYPE, paste0("backtest_results_freq=", FREQ,
                                                           "_window-size=", WINDOW_SIZE,
                                                           "_strategy-type=", STRATEGY_TYPE,
                                                           "_", intercept_tag,
                                                           "_threshold=", THRESHOLD, ".rds")))

