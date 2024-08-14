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
library("optparse")

source(file.path(here(), 'src', 'models', 'utils.R'))
source(file.path(here(), 'src', 'models', 'models.R'))
source(file.path(here(), 'src', 'plots', 'plot_funcs.R'))

# define command-line options
option_list <- list(
  make_option(c("--model_name"), type = "character", help = "Model name for output", default = "tvp-bvar"),
  make_option(c("--output_path"), type = "character", help = "Output path", default = file.path(here(), 'src', 'data', 'outputs')),
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 52 * 2),
  make_option(c("--mean_window_size"), type = "integer", help = "Mean window size", default = 52 * 1),
  make_option(c("--intercept"), type = "logical", help = "Intercept", default = TRUE),
  make_option(c("--scale_type"), type = "character", help = "Scale type", default = "rolling_scale"),
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
ITERATIONS <- args$iterations
BURNIN <- args$burnin

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
tsdata <- as.ts(data, start = 1, end = nrow(data), frequency = 52)

# generate VAR model
model <- gen_var(tsdata,
                 p = 1, 
                 deterministic = "const",
                 iterations = ITERATIONS, 
                 burnin = BURNIN)

# start timer
start_time <- Sys.time()

# Define the priors over the VAR model parameters
## coef = list(a_mu = prior means, v_i = prior precisisions) => prior: P(coef) = N(a_mu, diag(v_i))
## Recall that the prior precision is the inverse of the prior variance
## sigma = list(df = prior degrees of freedom, scale = prior scale) =>  P(sigma) = W(df, scale)
##
## References on Koop (2008) - Bayesian Econometrics, Section 6.6
model_with_priors <- add_priors(model,
                                coef = list(mu = 0, v_i = 0),
                                sigma = list(df = 1, scale = .0001))

# Draw posteriors
bvar_est <- draw_posterior(model_with_priors)

# end timer
end_time <- Sys.time()
print(paste0("Elapsed time: ", end_time - start_time))

summary(bvar_est)
tvp_bvec_out$runtime <- end_time - start_time

FINAL_RANK <- paste0(RANK, collapse = "-")

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
saveRDS(bvar_est, file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds")))