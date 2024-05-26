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
WINDOW_SIZE <- 52 * 2
MEAN_WINDOW_SIZE <- 52 * 1
INTERCEPT <- TRUE
TARGET <- "SGD"
SCALE_TYPE <- "rolling_scale"

data <- load_and_resample_currencies() %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")
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
dlm_model <- dlmModReg(X)
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
dlm_filter_residual$res <- as.data.frame(dlm_filter_residual$res)
colnames(dlm_filter_residual$res) <- "residual"
dlm_filter_residual$res$date <- ymd(rownames(X))
dlm_filter_residual$res <- dlm_filter_residual$res %>% select(date, everything())

# rename columns
## smooth parameters
dlm_smooth$s <- dlm_smooth$s %>% as.data.table()
colnames(dlm_smooth$s) <- append("intercept", colnames(X))
dlm_smooth$s <- dlm_smooth$s[2:dim(dlm_smooth$s)[1], ]
dlm_smooth$s$date <- ymd(rownames(X))
dlm_smooth$s <- dlm_smooth$s %>% select(date, everything())

## filter parameters
dlm_filter$m <- dlm_filter$m %>% as.data.table()
colnames(dlm_filter$m) <- append("intercept", colnames(X))
dlm_filter$m <- dlm_filter$m[2:dim(dlm_filter$m)[1], ]
dlm_filter$m$date <- ymd(rownames(X))
dlm_filter$m <- dlm_filter$m %>% select(date, everything())

# outputs
dlmout <- list(filter=dlm_filter,
               smooth=dlm_smooth,
               residuals=dlm_filter_residual)

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
saveRDS(dlmout, file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, ".rds")))






