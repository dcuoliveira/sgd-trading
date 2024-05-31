library("optparse")
library("here")

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

# load the results
results = readRDS(file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds")))

# break and save the results
# Break and save the outputs
for (i in 1:length(results)) {
  # break list
  tmp = results[[i]]
  tmp_names = names(tmp)

  if (!is.null(tmp_names)) {
    for (n in tmp_names) {
      # parse tmp obj
      output = tmp[[n]]

      # file name
      file = file.path(OUTPUT_PATH, paste0("model_results_", i, "_", name, "_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds"))

      # save results
      saveRDS(output, file = file)
    }
  }
  else {
    file = file.path(OUTPUT_PATH, paste0("model_results_", i, "_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds"))
    saveRDS(tmp, file = file)
  }
}

# delete the original file
file.remove(file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds")))