rm(list=ls())
library('readxl')
library('here')
library('dplyr')
library('lubridate')
library('urca')
library('tsDyn')
library('data.table')
library('purrr')
library('tidyr')
library('ggplot2')
library('Metrics')

## Define Parameters of the Study

SPOTS = c(
  
  # https://www.uobgroup.com/web-resources/uobgroup/pdf/research/MN_190321.pdf
  
  'USDCNY', 'USDEUR', 'USDMYR', 'USDJPY',
  'USDIDR', 'USDKRW', 'USDINR', 'USDTHB',
  'USDAUD', 'USDGBP', 'USDSGD'
  
  # 'USDSGD', 'USDKRW', 'USDMYR', 'USDCNY', 'USDTHB',
  # 'USDIDR', 'USDTWD', 'USDINR', 'USDTWD', 'USDJPY'
  # 'USDAUD', 'USDEUR', 'USDGBP'
)

FWD_NAMES = c(
  '1M', '2M', '3M', '6M', '12M', '2Y'
)

FWD_NAMES_MAPPINGS = c(
  '1M' = 1, '2M' = 2, '3M' = 3, '6M' = 6, '12M' = 12, '2Y' = 24
)

INPUT_PATH = here('src', 'data', 'inputs')
CCYS_FILE_NAME = 'daily-currencies-forwards.xlsx'
TRAIN_END_DATE = '2008-01-01'
MODEL_NAME <- 'rolling_ols'

## Load Data

ccy_list = list()
for (ccy in SPOTS) {
  daily_currencies_forwards <- read_excel(
    file.path(INPUT_PATH, CCYS_FILE_NAME),
    sheet = ccy
  )
  selected_ccy <- daily_currencies_forwards %>% select(date, paste0(ccy, ' Curncy'))
  ccy_list[[ccy]] <- selected_ccy
}

# merge all the data into one dataframe using the date column
ccy_df <- Reduce(function(x, y) merge(x, y, by = 'date', all = TRUE), ccy_list)

# resample prices to monthly frequency by the last observation of the month
ccy_df$date <- ymd(ccy_df$date)
ccy_df <- ccy_df %>% group_by(year(date), month(date)) %>% slice(n()) %>% ungroup() %>%
  arrange(date) %>% select(-`year(date)`, -`month(date)`) %>% tidyr::drop_na()

monthly_currencies_forwards <- daily_currencies_forwards %>%
  group_by(year = year(date), month = month(date)) %>%
  slice_max(order_by = date, n = 1) %>%
  ungroup() %>%
  arrange(date) %>%
  select(-year, -month)

# select train and test data
train_ccy_df <- ccy_df %>% filter(date <= TRAIN_END_DATE)
train_dates <- train_ccy_df$date
train_ccy_df <- train_ccy_df %>% select(-date)
test_ccy_df <- ccy_df %>% filter(date > TRAIN_END_DATE)
test_dates <- test_ccy_df$date
test_ccy_df <- test_ccy_df %>% select(-date)

train_curve_list = list()
test_curve_list = list()
for (fwd in FWD_NAMES) {
  fwd_list = list()
  for (ccy in SPOTS) {
    
    col_types <-  c('date', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
    daily_currencies_forwards <- read_excel(
      file.path(INPUT_PATH, CCYS_FILE_NAME),
      sheet = ccy,
      col_types = col_types
    )
    
    selected_ccy <- daily_currencies_forwards %>% select(date, paste0(ccy, fwd, ' Curncy'))
    fwd_list[[ccy]] <- selected_ccy
  }
  # merge all the data into one dataframe using the date column
  fwd_df <- Reduce(function(x, y) merge(x, y, by = 'date', all = TRUE), fwd_list)
  
  # resample prices to monthly frequency by the last observation of the month
  fwd_df$date <- ymd(fwd_df$date)
  fwd_df <- fwd_df %>% group_by(year(date), month(date)) %>% slice(n()) %>% ungroup() %>%
    arrange(date) %>% select(-`year(date)`, -`month(date)`) %>% tidyr::drop_na()
  
  # select train and test data
  train_fwd_df <- fwd_df %>% filter(date <= TRAIN_END_DATE)
  train_fwd_dates <- train_fwd_df$date
  train_fwd_df <- train_fwd_df %>% select(-date)
  test_fwd_df <- fwd_df %>% filter(date > TRAIN_END_DATE)
  test_fwd_dates <- test_fwd_df$date
  test_fwd_df <- test_fwd_df %>% select(-date)
  
  if ((dim(train_fwd_df)[1] == 0) | (dim(test_fwd_df)[1] == 0)){
    stop(paste0("Error: No data in train or test for forward ", fwd))
  }
  
  train_curve_list[[fwd]] <- train_fwd_df
  test_curve_list[[fwd]] <- test_fwd_df
}

## Recursive Forecasting of the Test Data

# setup
n_test <- nrow(test_ccy_df)
selected_lag <- 1
h <- 24

# initialize empty lists of data.tables per horizon
FWD_NAMES <- names(FWD_NAMES_MAPPINGS)

forecasts_list <- setNames(vector("list", length(FWD_NAMES)), FWD_NAMES)
actuals_list <- setNames(vector("list", length(FWD_NAMES)), FWD_NAMES)
forecasts_rw_list <- setNames(vector("list", length(FWD_NAMES)), FWD_NAMES)
forecasts_fwd_list <- setNames(vector("list", length(FWD_NAMES)), FWD_NAMES)

for (fwd in FWD_NAMES) {
  forecasts_list[[fwd]] <- data.table()
  actuals_list[[fwd]] <- data.table()
  forecasts_rw_list[[fwd]] <- data.table()
  forecasts_fwd_list[[fwd]] <- data.table()
}

# loop over the test set
ccys_to_forecast <- SPOTS
betas_sgd <- list()
for (i in seq_len(n_test)) {
  
  # append relevant portion of the test set to the training set starting from i=0 (no test data)
  current_data <- rbind(train_ccy_df, test_ccy_df[1:i, ])
  
  # compute log-prices
  current_data <- log(current_data)
  
  # equations to be estimated
  pred_ols <- list()
  betas <- list()
  for (ccy in ccys_to_forecast) {
    ols_model <- lm(
      as.formula(paste0('`', ccy, ' Curncy` ~ . -1')),
      data = current_data
    )
    
    tmp_betas <- ols_model$coefficients
    tmp_X <- current_data[nrow(current_data), ]
    tmp_X[[ccy]] <- NULL
    
    betas_names <- gsub('`', '', names(tmp_betas))
    tmp_X <- tmp_X[match(betas_names, names(tmp_X))]
    
    tmp_pred_ols_log <- sum(tmp_X * tmp_betas)
    tmp_pred_ols <- exp(tmp_pred_ols_log)
    
    tmp_pred_ols <- data.frame(ccy = rep(tmp_pred_ols, h)) %>%
      rename(!!paste0(ccy, ' Curncy') := ccy)
    row.names(tmp_pred_ols) <- nrow(current_data) + seq(1, h)
    
    pred_ols[[ccy]] <- tmp_pred_ols %>% as.data.table()
    betas[[ccy]] <- tmp_betas
    if (ccy == 'USDSGD') {
      betas_sgd[[i]] <- tmp_betas
    }
  }
  
  pred_ols <- do.call(cbind, pred_ols) %>% as.data.table()
  colnames(pred_ols) <- paste0(ccys_to_forecast, ' Curncy')
  for (fwd in FWD_NAMES) {
    horizon_idx <- FWD_NAMES_MAPPINGS[[fwd]]
    
    # OLS forecast
    tmp_pred_ols <- pred_ols[horizon_idx, ] %>% select(all_of(paste0(ccys_to_forecast, " Curncy"))) %>% as.data.table()
    forecasts_list[[fwd]] <- rbind(forecasts_list[[fwd]], as.data.table(as.list(tmp_pred_ols)))
    
    # actuals
    actual_row <- i + horizon_idx
    tmp_true <- test_ccy_df[actual_row, ] %>% select(all_of(paste0(ccys_to_forecast, " Curncy"))) %>% as.data.table()
    if (dim(actuals_list[[fwd]])[1] == 0){
      tmp_actuals <- actuals_list[[fwd]] %>% as.data.table()
    }
    else{
      tmp_actuals <- actuals_list[[fwd]] %>% select(all_of(paste0(ccys_to_forecast, " Curncy"))) %>% as.data.table()
    }
    actuals_list[[fwd]] <- rbind(tmp_true, tmp_actuals, fill = TRUE)
    
    # forward forecast
    tmp_fwd <- test_curve_list[[fwd]][i, ] %>% select(all_of(paste0(ccys_to_forecast, fwd, " Curncy"))) %>% as.data.table()
    colnames(tmp_fwd) <- gsub(fwd, '', colnames(tmp_fwd))
    forecasts_fwd_list[[fwd]] <- rbind(forecasts_fwd_list[[fwd]], as.data.table(as.list(tmp_fwd)))
    
    # random walk
    tmp_rw <- exp(current_data[dim(current_data)[1], ]) %>% select(all_of(paste0(ccys_to_forecast, " Curncy"))) %>% as.data.table()
    forecasts_rw_list[[fwd]] <- rbind(forecasts_rw_list[[fwd]], as.data.table(as.list(tmp_rw)))
  }
}

fwd_rmse <- list()
for (fwd in FWD_NAMES) {
  model_pred <- forecasts_list[[fwd]]
  fwd_pred <- forecasts_fwd_list[[fwd]]
  rw_pred <- forecasts_rw_list[[fwd]]
  
  ccy_model_rmse <- list()
  for (ccy in ccys_to_forecast){
    ccy_model_rmse[[ccy]] <- rmse(
      model_pred[[paste0(ccy, ' Curncy')]],
      fwd_pred[[paste0(ccy, ' Curncy')]]
    )
  }
  ccyy_model_rmse_df <- as.data.table(ccy_model_rmse)
  
  ccy_fwd_rmse <- list()
  for (ccy in ccys_to_forecast){
    ccy_fwd_rmse[[ccy]] <- rmse(
      fwd_pred[[paste0(ccy, ' Curncy')]],
      rw_pred[[paste0(ccy, ' Curncy')]]
    )
  }
  ccy_fwd_rmse_df <- as.data.table(ccy_fwd_rmse)
  
  ccy_rw_rmse <- list()
  for (ccy in ccys_to_forecast){
    ccy_rw_rmse[[ccy]] <- rmse(
      rw_pred[[paste0(ccy, ' Curncy')]],
      test_ccy_df[[paste0(ccy, ' Curncy')]]
    )
  }
  ccy_rw_rmse_df <- as.data.table(ccy_rw_rmse)
  
  ccy_rmse_df <- rbind(
    ccyy_model_rmse_df %>% mutate(type = 'model') %>% select(type, everything()),
    ccy_fwd_rmse_df %>% mutate(type = 'forward') %>% select(type, everything()),
    ccy_rw_rmse_df %>% mutate(type = 'random_walk') %>% select(type, everything())
  )
  
  fwd_rmse[[fwd]] <- ccy_rmse_df %>% 
    mutate(fwd = fwd) %>% 
    select(fwd, type, everything()) %>% 
    pivot_longer(-c(fwd, type), names_to = 'ccy', values_to = 'rmse') %>%
    mutate(ccy = gsub(' Curncy', '', ccy))
}
fwd_rmse_df <- do.call(rbind, fwd_rmse) %>% as.data.table()

# check if dir exists
target_path <- here('src', 'data', 'outputs', MODEL_NAME)
if (!dir.exists(target_path)) {
  dir.create(target_path, recursive = TRUE)
}

# save outputs
saveRDS(fwd_rmse_df, file = here(target_path, 'rmse_table.rds'))
saveRDS(test_dates, file = here(target_path, 'test_dates.rds'))
saveRDS(forecasts_list, file = here(target_path, 'model_forecasts.rds'))
saveRDS(forecasts_rw_list, file = here(target_path, 'model_forecasts_rw.rds'))
saveRDS(forecasts_fwd_list, file = here(target_path, 'model_forecasts_fwd.rds'))





