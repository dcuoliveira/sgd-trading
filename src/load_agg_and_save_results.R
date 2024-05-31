rm(list=ls())
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

output_reference <- paste0(SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN)

# list files in directory
files <- list.files(file.path(OUTPUT_PATH, output_reference))

# break and save the results
out1 = list()
out2 = list()
out3 = list()
for (f in files) {
  tmp_param_name = strsplit(f, ".rds")[[1]]
  tmp_params = strsplit(tmp_param_name, "_")[[1]][3:length(strsplit(tmp_param_name, "_")[[1]])]

  if (tmp_params[1] == 1){
    tmp = readRDS(file.path(OUTPUT_PATH, output_reference, f))
    if (tmp_params[2] == "x"){
      out_param_name = "x_d"
    }
    else if ((tmp_params[2] == "C")&(tmp_params[3] == "sigma")){
      out_param_name = "C_sigma"
    }
    else{
      out_param_name = tmp_params[2]
    }
    out1[[out_param_name]] = tmp
  }
  else if (tmp_params[1] == 2) {
    tmp = readRDS(file.path(OUTPUT_PATH, output_reference, f))
    if (tmp_params[2] == "x"){
      out_param_name = "x_d"
    }
    else if ((tmp_params[2] == "C")&(tmp_params[3] == "sigma")){
      out_param_name = "C_sigma"
    }
    else{
      out_param_name = tmp_params[2]
    }
    out2[[out_param_name]] = tmp
  }
  else if (tmp_params[1] == 3){
    tmp = readRDS(file.path(OUTPUT_PATH, output_reference, f))
    out3 = tmp
  
  }

}

# reorder lists
desired_order <- c("y", "w", "x_d", "alpha", "beta", "Pi", "C", "C_sigma", "Sigma", "specifications")

# 1
order <- desired_order[desired_order %in% names(out1)]
out1 = out1[order]
attr(out1, "class") <- "bvec"

# 2
order <- desired_order[desired_order %in% names(out1)]
out2 = out2[order]
attr(out2, "class") <- "bvec"

out = list(out1, out2, out3)

# save the results
saveRDS(out, file.path(OUTPUT_PATH, output_reference, paste0("model_results_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds")))


# # delete the original file
# file.remove(file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, "_", FINAL_RANK, "_", ITERATIONS, "_", BURNIN, ".rds")))