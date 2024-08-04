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
  make_option(c("--iterations"), type = "integer", help = "Number of iterations", default = 10000),
  make_option(c("--burnin"), type = "integer", help = "Burnin", default = 5000),
  make_option(c("--rank"), type = "character", help = "Rank", default = 1:3),
  make_option(c("--freq"), type = "character", help = "Frequency", default = "monthly"),
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 12)
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
results[[length(results)]] <- NULL
print(bvartools::summary.bvarlist(results))

# if error, delete the last element and re-run
results[[length(results)]] <- NULL
print(bvartools::summary.bvarlist(results))

# # chosen model
# r = 2
# object <- results[[r]]
# level_model <- object
# difference_model = bvartools::bvec_to_bvar(results[[r]])

# # summary of the final model
# level_model_summary = summary(level_model)
# difference_model_summary = summary(difference_model)

# # plot tim-varying betas
# tvp_betas = list()
# for (k in 1:ncol(object$beta[[1]])) {
#   draws <- lapply(object$beta, function(x) {quantile(x[, k], probs = c(0.05, .5, 0.95))})
#   draws <- matrix(unlist(draws), ncol = 1, byrow = TRUE)
#   tvp_betas[[k]] <- draws
# }
# # tvp_betas_df = do.call(cbind, tvp_betas) %>% as.data.table()

# # create beta names
# idx = 1
# tmp_df = tvp_betas[[idx]] %>% as.data.table() %>% mutate(date = date)

# # plot with confidence interval add red line on zero
# ggplot(tmp_df, aes(x=date)) + 
#   geom_line(aes(y=V2)) +
#   geom_ribbon(aes(ymin=V1, ymax=V3), alpha=0.2) +
#   geom_hline(yintercept = 0, linetype="dashed", color = "red", size = 2)

# # create beta names
# # tvp_betas_df = rename_tvp_params(names = names, p = p, df = tvp_alphas_df)

# # plot tim-varying alphas
# tvp_alphas = list()
# for (k in 1:ncol(object$alpha[[1]])) {
#   draws <- lapply(object$alpha, function(x) {quantile(x[, k], probs = c(0.05, .5, 0.95))})
#   draws <- matrix(unlist(draws), ncol = 3, byrow = TRUE)
#   tvp_alphas[[k]] <- draws
# }
# # tvp_alphas_df = do.call(cbind, tvp_alphas) %>% as.data.table()

# # create beta names
# idx = 3 + 12
# tmp_df = tvp_alphas[[idx]] %>% as.data.table() %>% mutate(date = date)

# # plot with confidence interval add red line on zero
# ggplot(tmp_df, aes(x=date)) + 
#   geom_line(aes(y=V2)) +
#   geom_ribbon(aes(ymin=V1, ymax=V3), alpha=0.2) +
#   geom_hline(yintercept = 0, linetype="dashed", color = "red", size = 2)

# pp.test(x = tmp_df$V2, type = "Z(alpha)")

# hist(tmp_df$V2, breaks = 75, col = "lightblue")

# #create historgram with density and 95% confidence interval highlighted
# ggplot(tmp_df, aes(x=V2)) + 
#   geom_histogram(aes(y=..density..), bins = 75, fill = "lightblue") +
#   geom_density(alpha = 0.2) +
#   geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed", size = 1) 
  






