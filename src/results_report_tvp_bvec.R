rm(list = ls())
library("optparse")
library("here")
library("dplyr")
library("ggplot2")
library("data.table")

# define command-line options
option_list <- list(
  make_option(c("--model_name"), type = "character", help = "Model name for output", default = "tvp-bvec"),
  make_option(c("--output_path"), type = "character", help = "Output path", default = file.path(here(), 'src', 'data', 'outputs')),
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
SCALE_TYPE <- args$scale_type
ITERATIONS <- args$iterations
BURNIN <- args$burnin
RANK <- 1:2

FINAL_RANK <- paste0(RANK, collapse = "-")

output_reference <- paste0(SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN)

# list files in dir
results = readRDS(file.path(OUTPUT_PATH, output_reference, paste0("model_results_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds")))
results[[length(results)]] <- NULL

# summary of models
bvartools::summary.bvarlist(results)

# chosen model
i = 2
object <- results[[i]]
level_model <- object
difference_model = bvartools::bvec_to_bvar(results[[i]])

# summary of the final model
level_model_summary = summary(level_model)
difference_model_summary = summary(difference_model)

# plot tim-varying betas
par(mfrow = c(1, 1))
tvp_betas = list()
for (k in 1:ncol(object$beta[[1]])) {
  draws <- lapply(object$beta, function(x) {quantile(x[, k], probs = c(.5))})
  draws <- matrix(unlist(draws), ncol = 1, byrow = TRUE)
  tvp_betas[[k]] <- draws
}
tvp_betas_df = do.call(cbind, tvp_betas)

plot(tvp_betas_df[,2], type = "l", col = 1, lwd = 2, xlab = "Time", ylab = "Beta", main = "Time-varying betas")

# plot tim-varying alphas
par(mfrow = c(1, 1))
tvp_alphas = list()
for (k in 1:ncol(object$alpha[[1]])) {
  draws <- lapply(object$alpha, function(x) {quantile(x[, k], probs = c(.5))})
  draws <- matrix(unlist(draws), ncol = 1, byrow = TRUE)
  tvp_alphas[[k]] <- draws
}
tvp_alphas_df = do.call(cbind, tvp_alphas)

ref = 8
plot.ts(tvp_alphas_df[,c(ref, ref+12)], type = "l", col = 1:2, lwd = 2, xlab = "Time", ylab = "Beta", main = "Time-varying alphas")
