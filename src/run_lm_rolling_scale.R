rm(list=ls())
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("reshape2")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))
source(file.path(getwd(), 'src', 'plots', 'plot_funcs.R'))

MODEL <- "lm"
OUTPUT_PATH <- file.path(getwd(), 'src', 'data', 'outputs', MODEL)
WINDOW_SIZE <- 52 * 2
MEAN_WINDOW_SIZE <- 52 * 1
INTERCEPT <- TRUE
TARGET <- "SGD"
SCALE_TYPE <- "rolling_scale"

data <- merge_fx_sneer_data() %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")
data_orig <- data
data <- data %>% select(-date) # %>% apply(2, function(x) scale(x)) %>% as.data.frame()

if (INTERCEPT == T){
  model_formula <- paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + "), "+1")
}else{
  model_formula <- paste(paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + ")), " -1")
}

if (SCALE_TYPE == "scale"){
  data <- data %>% lapply(scale) %>% as.data.table()
  row.names(data) <- data_orig$date
}else if (SCALE_TYPE == "rolling_scale"){
  n <- dim(data)[1]
  mean_data <- apply(data, 2, function(x) {roll_mean(x, width = n, min_obs = WINDOW_SIZE)})
  sd_data <- apply(data, 2, function(x) {roll_sd(x, width = n, min_obs = WINDOW_SIZE)})
  
  data <- ((data - mean_data) / sd_data)
  row.names(data) <- data_orig$date
  data <- data %>% drop_na()
}

reg <- lm(model_formula, data)

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
saveRDS(reg, file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, ".rds")))






