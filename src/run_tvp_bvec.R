rm(list=ls())
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("reshape2")
library("dlm")
library("bvartools")
library("here")
library("parallel")

source(file.path(here(), 'src', 'models', 'utils.R'))
source(file.path(here(), 'src', 'models', 'models.R'))
source(file.path(here(), 'src', 'plots', 'plot_funcs.R'))

MODEL <- "tvp-bvec"
OUTPUT_PATH <- file.path(here(), 'src', 'data', 'outputs', MODEL)
WINDOW_SIZE <- 52 * 2
MEAN_WINDOW_SIZE <- 52 * 1
INTERCEPT <- TRUE
TARGET <- "SGD"
SCALE_TYPE <- "rolling_scale"
num_cores <- detectCores() - 1
print(paste0("Number of cores: ", num_cores))

data <- load_and_resample_currencies() %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")
data_orig <- data
data <- data %>% select(-date) # %>% apply(2, function(x) scale(x)) %>% as.data.frame()
tsdata <- ts(data, start = 1, end = nrow(data), frequency = 52)

# generate BVEC model
temp <- gen_vec(
  data = tsdata, # endogenous variables
  p = 1, # endogenous variables lag order (var model)
  exogen = NULL, # exogenous variables
  s = NULL, # exogenous variables lag order
  r = 1:2, # cointegration rank. It tests r = (0, 1, 2).
  const = "unrestricted", # if a constant should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  trend = NULL, # if a trend should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  seasonal = NULL, # if seasonal dummies should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  structural = FALSE, # if the "data" should be prepared for the estimation of a svar
  tvp = TRUE, # if the model parameters are time varying
  sv = FALSE, # if time varying error variances should be added (stoch vol)
  fcst = NULL, # number of observations saved for forecasting
  iterations = 100, # mcmc draws excluding burn-in
  burnin = 100 # number of mcmc draws to initialize the sampler
                )

# start timer
start_time <- Sys.time()

# Add priors to the "empty" models
temp <- add_priors(
  temp,
  coef = list(v_i = 0,
              v_i_det = 1 / 100, # ??
              shape = 0, # a numeric or character specifying the prior shape parameter of the error term. default is k.s
              rate = .0001,
              rate_det = .0001),
  coint = list(shape = 0, # mean of the gaussian prior on the cointegration matrix
               rho = 0.9999, # var(\b_t) = I_{nr}\frac{1}{1-\rho^2} - variance of the prior on the model betas
               rate = 1e-04), # var(\nu_t) - state error variance priors
  sigma = list(df = "k", scale = .0001) # measurement error variance priors
  )

# Run Gibbs sampler
tvp_bvec_out <- draw_posterior(temp, mc.cores = num_cores, verbose = TRUE)

# end timer
end_time <- Sys.time()
print(paste0("Elapsed time: ", end_time - start_time))

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
saveRDS(tvp_bvec_out, file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, ".rds")))