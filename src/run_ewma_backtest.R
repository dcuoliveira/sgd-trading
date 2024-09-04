rm(list=ls())
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("MTS")
library("lubridate")

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
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 52 * 4),
  make_option(c("--strategy_type"), type = "character", help = "Strategy type", default = "ewma"),
  make_option(c("--target"), type = "character", help = "Target variable name", default = "SGD"),
  make_option(c("--betas_type"), type = "character", help = "Betas type", default = NULL),
  make_option(c("--threshold"), help = "Threshold to the used on the strategy", default = 1.5)
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
BETAS_TYPE <- args$betas_type
FREQ = args$frequency
WINDOW_SIZE <- args$window_size
THRESHOLD <- as.numeric(args$threshold)
INTERCEPT <- args$intercept
BETAS_TYPE <- "filter"

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
prices_df <- load_and_resample_currencies(freq=FREQ) %>% mutate(date=ymd(date))

if (MODEL == "dlm"){

  # load model output
  model_out <- readRDS(file = file.path(OUTPUT_PATH, SCALE_TYPE, paste0("model_results_", FREQ, "_", WINDOW_SIZE, "_", intercept_tag, ".rds")))
  
  # cointegration error
  cointegration_error_df <- model_out$residuals$res %>%
    drop_na() %>%
    mutate(ewma_vol=EWMAvol(residual, lambda = 0.8)$Sigma.t) %>%
    mutate(ub=THRESHOLD*sqrt(ewma_vol), lb=-THRESHOLD*sqrt(ewma_vol)) %>%
    as.data.table()
  
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
  cointegration_error_df <- model_out$residuals %>% drop_na() %>% mutate(ewma_vol=EWMAvol(residual,lambda = 0.8)$Sigma.t) %>%
    mutate(ub=THRESHOLD*sqrt(ewma_vol), lb=-THRESHOLD*sqrt(ewma_vol))

  # betas
  betas_df <- model_out$model$coefs %>% as.data.table() %>% drop_na() %>%
   mutate(date=cointegration_error_df$date) %>% select(date, everything()) %>% as.data.table()

}

# positions for the target (y) variable
positions_df <- data.table(
  date = cointegration_error_df$date
)

# Add the dynamically named column using := inside a data.table expression
# positions_df[, (TARGET) := ifelse(
#   lag(cointegration_error_df$residual, n = 1) >= cointegration_error_df$ub, -1,
#   ifelse(cointegration_error_df$residual < cointegration_error_df$lb, 1, 0)
# )]
positions_df[, (TARGET) := ifelse(
  cointegration_error_df$residual >= cointegration_error_df$ub, -1,
  ifelse(cointegration_error_df$residual < cointegration_error_df$lb, 1, 0)
)]

# fillna with zero values
positions_df <- positions_df %>% replace(is.na(.), 0)

# positions for the regressors (X)
positions_betas_df <- merge(x = positions_df, y = betas_df, by = "date") %>% select(-date, -sym(TARGET))

out_positions_betas_list <- list()
for (i in 1:nrow(positions_betas_df)){
  betas_row <- positions_betas_df[i,]
  coint_error_row <- positions_df[i,]
  
  if (coint_error_row[[TARGET]] > 0){
    out_positions_betas_list[[i]] <- betas_row * 1 
  }else if (coint_error_row[[TARGET]] < 0){
    out_positions_betas_list[[i]] <- betas_row * -1 
  }else{
    out_positions_betas_list[[i]] <- betas_row * 0 
  }
  
}
out_positions_betas_df <- do.call("rbind", out_positions_betas_list) %>% as.data.table()

# put all positions together
out_positions_df <- cbind(positions_df, out_positions_betas_df) %>% as.data.table()
if (INTERCEPT == T){
   out_positions_df <- out_positions_df %>% select(-intercept)
}

# returns data
prices_dtref <- prices_df$date
returns_df <- prices_df %>% select(-date)
returns_df <- (returns_df - lag(returns_df)) / lag(returns_df)
returns_df$date <- prices_dtref
returns_df <- returns_df %>% select(date, everything()) %>% drop_na()

# merge positions and returns 
returns_df <- merge(out_positions_df %>% select(date), returns_df, by = "date")

# check if columns are sorted correctly
returns_list = list()
for (colname in colnames(out_positions_df)){
  if (colname == "date"){
    next
  }else{
    returns_list[[colname]] <- returns_df[[colname]]
  }
}
returns_df <- do.call("cbind", returns_list) %>% as.data.table() %>% 
  mutate(date=out_positions_df$date[1:length(out_positions_df$date)]) %>%
  select(date, everything())
l <- 1
lead_returns_df <- cbind(data.frame(date=returns_df$date[1:(dim(returns_df)[1]-l)]), lead(returns_df %>% select(-date), l) %>% drop_na()) %>% as.data.table()

# generate stretegy returns
out_positions_df <- merge(out_positions_df, lead_returns_df %>% select(date), by = "date")
strategy_returns_df <- (out_positions_df %>% select(-date)) * (lead_returns_df %>% select(-date))

ts.plot(cumprod(1+rowSums(strategy_returns_df)))

strategy_returns_df <- strategy_returns_df %>% mutate(date=out_positions_df$date) %>%
  select(date, everything()) %>% drop_na()

all_cumret_df <- cumprod(1+(strategy_returns_df %>% select(-date))) %>%
  mutate(date=strategy_returns_df$date) %>% select(date, everything())
ret_df <- data.frame(date=strategy_returns_df$date,
                     portfolio=rowSums(strategy_returns_df %>% select(-date))) %>% as.data.table()

outputs <- list(signal=cointegration_error_df,
                positions=out_positions_df,
                returns=strategy_returns_df,
                bars=prices_df,
                bars_ret=returns_df)

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, SCALE_TYPE), showWarnings = FALSE)
saveRDS(outputs, file.path(OUTPUT_PATH, SCALE_TYPE, paste0("backtest_results_", FREQ, "_", WINDOW_SIZE, "_", STRATEGY_TYPE, "_", intercept_tag, ".rds")))


