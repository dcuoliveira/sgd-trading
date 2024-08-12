rm(list = ls())
library("optparse")
library("here")
library("dplyr")
library("ggplot2")
library("data.table")

source(file.path(here(), 'src', 'models', 'utils.R'))
source(file.path(here(), 'src', 'models', 'models.R'))
source(file.path(here(), 'src', 'plots', 'plot_funcs.R'))

# define command-line options
option_list <- list(
  make_option(c("--model_name"), type = "character", help = "Model name for output", default = "tvp-bvec"),
  make_option(c("--output_path"), type = "character", help = "Output path", default = file.path(here(), 'src', 'data', 'outputs')),
  make_option(c("--scale_type"), type = "character", help = "Scale type", default = "rolling_scale"),
  make_option(c("--iterations"), type = "integer", help = "Number of iterations", default = 100),
  make_option(c("--burnin"), type = "integer", help = "Burnin", default = 100),
  make_option(c("--rank"), type = "character", help = "Rank", default = 1:3),
  make_option(c("--freq"), type = "character", help = "Frequency", default = "weekly"),
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 108)
)

# create a parser object
parser <- OptionParser(option_list = option_list)

# parse the arguments
args <- parse_args(parser)

MODEL <- args$model_name
OUTPUT_PATH <- file.path(args$output_path, MODEL)
SCALE_TYPE <- args$scale_type
ITERATIONS <- args$iterations
BURNIN <- args$burnin
RANK <- args$rank
p <- 1
FREQ <- args$freq
WINDOW_SIZE <- args$window_size

FINAL_RANK <- paste0(RANK, collapse = "-")

output_reference <- paste0(SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN)
file_name <- paste0("model_results_", FREQ, "_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds")

# read currencies data
currencies <- load_and_resample_currencies(freq=FREQ) %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")
date <- currencies$date[(WINDOW_SIZE + 1):length(currencies$date)]
currencies <- currencies %>% select(-date) # %>% apply(2, function(x) scale(x)) %>% as.data.frame()

# list files in dir
results = readRDS(file.path(OUTPUT_PATH, output_reference, file_name))
print(results[[length(results)]])
results[[length(results)]] <- NULL

# summary of models
print(bvartools::summary.bvarlist(results))

# if error, delete the last element and re-run
# results[[length(results)]] <- NULL
# print(bvartools::summary.bvarlist(results))

# if error, delete the last element and re-run
# results[[length(results)]] <- NULL
# print(bvartools::summary.bvarlist(results))

# chosen model
r = 3
object <- results[[r]]
level_model <- object
difference_model = bvartools::bvec_to_bvar(object)

# summary of the final model
level_model_summary = summary(level_model)
difference_model_summary = summary(difference_model)

# extract tim-varying betas
center = 0.5
upper = 0.83
lower = 0.17
tvp_betas_center = list()
tvp_betas_lower = list()
tvp_betas_upper = list()
for (k in 1:ncol(object$beta[[1]])) {
  draws <- lapply(object$beta, function(x) {quantile(x[, k], probs = c(lower, center, upper))})
  draws <- matrix(unlist(draws), ncol = 3, byrow = TRUE)
  tvp_betas_center[[k]] <- draws[, 2]
  tvp_betas_lower[[k]] <- draws[, 1]
  tvp_betas_upper[[k]] <- draws[, 3]
}
tvp_betas_center_df = do.call(cbind, tvp_betas_center) %>% as.data.table() 
tvp_betas_lower_df = do.call(cbind, tvp_betas_lower) %>% as.data.table()
tvp_betas_upper_df = do.call(cbind, tvp_betas_upper) %>% as.data.table()

# create beta names
tvp_betas_center_df = rename_tvp_params(df = tvp_betas_center_df, names = colnames(currencies), r = r)
tvp_betas_lower_df = rename_tvp_params(df = tvp_betas_lower_df, names = colnames(currencies), r = r)
tvp_betas_upper_df = rename_tvp_params(df = tvp_betas_upper_df, names = colnames(currencies), r = r)

# extract tim-varying alphas
tvp_alphas_center = list()
tvp_alphas_lower = list()
tvp_alphas_upper = list()
for (k in 1:ncol(object$alpha[[1]])) {
  draws <- lapply(object$alpha, function(x) {quantile(x[, k], probs = c(lower, center, upper))})
  draws <- matrix(unlist(draws), ncol = 3, byrow = TRUE)
  tvp_alphas_center[[k]] <- draws[, 2]
  tvp_alphas_lower[[k]] <- draws[, 1]
  tvp_alphas_upper[[k]] <- draws[, 3]
}
tvp_alphas_center_df = do.call(cbind, tvp_alphas_center) %>% as.data.table()
tvp_alphas_lower_df = do.call(cbind, tvp_alphas_lower) %>% as.data.table()
tvp_alphas_upper_df = do.call(cbind, tvp_alphas_upper) %>% as.data.table()

# create alpha names
tvp_alphas_center_df = rename_tvp_params(df = tvp_alphas_center_df, names = colnames(currencies), r = r)
tvp_alphas_lower_df = rename_tvp_params(df = tvp_alphas_lower_df, names = colnames(currencies), r = r)
tvp_alphas_upper_df = rename_tvp_params(df = tvp_alphas_upper_df, names = colnames(currencies), r = r)

betas = list(center = tvp_betas_center_df, lower = tvp_betas_lower_df, upper = tvp_betas_upper_df)
alphas = list(center = tvp_alphas_center_df, lower = tvp_alphas_lower_df, upper = tvp_alphas_upper_df)
output = list(betas = betas, alphas = alphas)

dir.create(file.path(OUTPUT_PATH, output_reference), showWarnings = FALSE)
file_name = paste0("betas_alphas_", FREQ, "_", SCALE_TYPE, "_r=", r, "_", ITERATIONS, "_", BURNIN, ".rds")
saveRDS(output, file.path(OUTPUT_PATH, output_reference, file_name))
  






