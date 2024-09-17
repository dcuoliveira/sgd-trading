rm(list=ls())
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("reshape2")
library("dlm")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))
source(file.path(getwd(), 'src', 'plots', 'plot_funcs.R'))

MODEL <- "dlm"
OUTPUT_PATH <- file.path(getwd(), 'src', 'data', 'outputs', MODEL)
WINDOW_SIZE <- 52 * 10
INTERCEPT <- FALSE
TARGET <- "SGD"
SCALE_TYPE <- "noscale"
FREQ <- "weekly"

data <- load_and_resample_currencies(freq=FREQ) %>% mutate(date=ymd(date)) # %>% filter(date >= "2006-01-01")
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
  dates <- ymd(row.names(data))
}else if (SCALE_TYPE == "rolling_scale"){
  n <- dim(data)[1]
  mean_data <- apply(data, 2, function(x) {roll_mean(x, width = n, min_obs = WINDOW_SIZE)})
  sd_data <- apply(data, 2, function(x) {roll_sd(x, width = n, min_obs = WINDOW_SIZE)})
  
  data <- ((data - mean_data) / sd_data)
  row.names(data) <- data_orig$date
  data <- data %>% drop_na()
  dates <- ymd(row.names(data))
}else if (SCALE_TYPE == "noscale"){
  dates <- ymd(row.names(data))
}

y <- data[,TARGET]
X <- data %>% select(-!!(TARGET))

# MLE estimates of the variance
lm_model <- lm(formula = model_formula,
               data = data)
betas_variaces <- (summary(lm_model)$coefficients[,2]) ** 2
residual_variance <- ((summary(lm_model)$sigma)) ** 2

# DLM specs
#########################################################
# y_t = (F_t x I_m)\theta_t + v_t,  v_t ~ N(0, V)
# \theta_t = (G_t x I_m)\theta_{t-1} + w_t, w_t ~ N(\boldsymbol{0}, W)
#
# m: number of currencies
# theta_t: time-varying alphas and betas 
#########################################################
m <- NCOL(X)
dlm_model <- dlmModReg(X, addInt = INTERCEPT)
dlm_model$FF <- dlm_model$FF
dlm_model$GG <- dlm_model$GG * 1
dlm_model$W <- diag(betas_variaces)
dlm_model$V <- residual_variance 
dlm_model$m0 <- rep(0,2 * m)
dlm_model$C0 <- diag(1e7, nr = 2 * m)

# DLM estimate
dlm_filter <- dlmFilter(y, dlm_model)
dlm_smooth <- dlmSmooth(dlm_filter)
dlm_filter_residual <- residuals(dlm_filter)
dlm_filter_residual$res <- as.data.table(dlm_filter_residual$res)
colnames(dlm_filter_residual$res) <- "residual"
dlm_filter_residual$res$date <- ymd(rownames(X))
dlm_filter_residual$res <- dlm_filter_residual$res %>% select(date, everything())

# Build residuals from scratch
if (INTERCEPT == F){
  final_colnames <- colnames(data_orig %>% select(-date, -SGD)) 
}else{
  final_colnames <- colnames(data_orig %>% mutate(intercept=1) %>% select(-date, -SGD) %>% select(intercept, everything())) 
}
prices_df <- data_orig %>% as.data.table()
betas_df <- dlm_filter$m %>%
  as.data.table() %>%
  slice(2:nrow(.)) %>%
  setnames(final_colnames) %>%
  mutate(date=ymd(dates)) %>%
  select(date, everything())

residuals <- list()
for (i in 1:nrow(betas_df)){
  betas_tmp <- betas_df[i,] %>% select(-date)
  prices_tmp <- prices_df[i,] %>% select(-date, -SGD)
  resid_tmp <- prices_df[i, SGD] - sum(betas_tmp * prices_tmp)
  resid_tmp <- data.table(date=betas_df[i,]$date, residual=resid_tmp)
  residuals[[i]] <- resid_tmp
}
residuals_df <- do.call(rbind, residuals) %>% as.data.table()
dlm_filter_residual$res <- residuals_df

# rename columns
## smooth parameters
dlm_smooth$s <- dlm_smooth$s %>% as.data.table()
if (INTERCEPT == T){
  colnames(dlm_smooth$s) <- append("intercept", colnames(X))
}else{
  colnames(dlm_smooth$s) <- colnames(X)
}
dlm_smooth$s <- dlm_smooth$s[2:dim(dlm_smooth$s)[1], ]
dlm_smooth$s$date <- ymd(rownames(X))
dlm_smooth$s <- dlm_smooth$s %>% select(date, everything())

## filter parameters
dlm_filter$m <- dlm_filter$m %>% as.data.table()
if (INTERCEPT == T){
  colnames(dlm_filter$m) <- append("intercept", colnames(X))
}else{
  colnames(dlm_filter$m) <- colnames(X)
}
dlm_filter$m <- dlm_filter$m[2:dim(dlm_filter$m)[1], ]
dlm_filter$m$date <- ymd(rownames(X))
dlm_filter$m <- dlm_filter$m %>% select(date, everything())

# outputs
dlmout <- list(filter=dlm_filter,
               smooth=dlm_smooth,
               residuals=dlm_filter_residual,
               prices=data_orig)

if (INTERCEPT == T){
  intercept_tag <- "intercept"
}else{
  intercept_tag <- "nointercept"
}

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, SCALE_TYPE), showWarnings = FALSE)
saveRDS(dlmout, file.path(OUTPUT_PATH, SCALE_TYPE, paste0("model_results_", FREQ, "_", WINDOW_SIZE, "_", intercept_tag, ".rds")))






