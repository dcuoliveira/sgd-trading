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
library("optparse")

source(file.path(here(), 'src', 'models', 'utils.R'))
source(file.path(here(), 'src', 'models', 'models.R'))
source(file.path(here(), 'src', 'plots', 'plot_funcs.R'))

# define command-line options
option_list <- list(
  make_option(c("--model_name"), type = "character", help = "Model name for output", default = "tvp-bvec"),
  make_option(c("--output_path"), type = "character", help = "Output path", default = file.path(here(), 'src', 'data', 'outputs')),
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 52 * 2),
  make_option(c("--mean_window_size"), type = "integer", help = "Mean window size", default = 52 * 1),
  make_option(c("--intercept"), type = "logical", help = "Intercept", default = TRUE),
  make_option(c("--scale_type"), type = "character", help = "Scale type", default = "rolling_scale"),
  make_option(c("--num_cores"), type = "integer", help = "Number of cores", default = detectCores() - 1),
  make_option(c("--iterations"), type = "integer", help = "Number of iterations", default = 100),
  make_option(c("--burnin"), type = "integer", help = "Burnin", default = 100)
)

# create a parser object
parser <- OptionParser(option_list = option_list)

# parse the arguments
args <- parse_args(parser)

MODEL <- args$model_name
OUTPUT_PATH <- file.path(args$output_path, MODEL)
WINDOW_SIZE <- args$window_size
MEAN_WINDOW_SIZE <- args$mean_window_size
SCALE_TYPE <- args$scale_type
num_cores <- args$num_cores
ITERATIONS <- args$iterations
BURNIN <- args$burnin
RANK <- 1:2

# load data
data <- load_and_resample_currencies() %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")
data_orig <- data
data <- data %>% select(-date) # %>% apply(2, function(x) scale(x)) %>% as.data.frame()

# scale data
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

# change to ts format
tsdata <- ts(data, start = 1, end = nrow(data), frequency = 52)

# generate BVEC model
temp <- gen_vec(
  data = tsdata, # endogenous variables
  p = 1, # endogenous variables lag order (var model)
  exogen = NULL, # exogenous variables
  s = NULL, # exogenous variables lag order
  r = RANK, # cointegration rank. It tests r = (0, 1, 2).
  const = "unrestricted", # if a constant should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  trend = NULL, # if a trend should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  seasonal = NULL, # if seasonal dummies should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  structural = FALSE, # if the "data" should be prepared for the estimation of a svar
  tvp = TRUE, # if the model parameters are time varying
  sv = FALSE, # if time varying error variances should be added (stoch vol)
  fcst = NULL, # number of observations saved for forecasting
  iterations = ITERATIONS, # mcmc draws excluding burn-in
  burnin = BURNIN # number of mcmc draws to initialize the sampler
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
tvp_bvec_out <- draw_posterior(temp, verbose = TRUE)

# end timer
end_time <- Sys.time()
print(paste0("Elapsed time: ", end_time - start_time))

summary(tvp_bvec_out)
tvp_bvec_out$runtime <- end_time - start_time

FINAL_RANK <- paste0(RANK, collapse = "-")

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
saveRDS(tvp_bvec_out, file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds")))