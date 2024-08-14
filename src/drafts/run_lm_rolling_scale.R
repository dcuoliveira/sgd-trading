rm(list=ls())
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("reshape2")
library("optparse")
library("rollRegres")

source(file.path(here(), 'src', 'models', 'utils.R'))
source(file.path(here(), 'src', 'models', 'models.R'))
source(file.path(here(), 'src', 'plots', 'plot_funcs.R'))

# define command-line options
option_list <- list(
  make_option(c("--model_name"), type = "character", help = "Model name for output", default = "rolling-ols"),
  make_option(c("--output_path"), type = "character", help = "Output path", default = file.path(here(), 'src', 'data', 'outputs')),
  make_option(c("--frequency"), type = "character", help = "Frequency to parse the data", default = "weekly"),
  make_option(c("--intercept"), type = "logical", help = "Intercept", default = TRUE),
  make_option(c("--scale_type"), type = "character", help = "Scale type", default = "rolling_scale"),
  make_option(c("--num_cores"), type = "integer", help = "Number of cores", default = 1),
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 48),
  make_option(c("--intercept"), type = "logical", help = "Intercept", default = TRUE)
)

# create a parser object
parser <- OptionParser(option_list = option_list)

# parse the arguments
args <- parse_args(parser)

MODEL <- args$model_name
OUTPUT_PATH <- file.path(args$output_path, MODEL)
FREQ = args$frequency
WINDOW_SIZE <- args$window_size
SCALE_TYPE <- args$scale_type
num_cores <- args$num_cores
INTERCEPT <- args$intercept

output_reference <- paste0(SCALE_TYPE)

if (FREQ == "monthly"){
  FREQ_INT <- 12
}else if (FREQ == "weekly"){
  FREQ_INT <- 52
}

# load data
data <- load_and_resample_currencies(freq=FREQ) %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")
data_orig <- data
data <- data %>% select(-date) # %>% apply(2, function(x) scale(x)) %>% as.data.frame()

if (INTERCEPT == T){
  model_formula <- paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + "), "+1")
}else{
  model_formula <- paste(paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + ")), " -1")
}

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

rolling_ols_out <- roll_regres(model_formula, data, min_obs = WINDOW_SIZE, do_downdates = FALSE, width = WINDOW_SIZE) # width only used when do_downdates=T

dir.create(file.path(OUTPUT_PATH, output_reference), showWarnings = FALSE)
saveRDS(rolling_ols_out, file.path(OUTPUT_PATH, output_reference, paste0("model_results_", FREQ, "_", SCALE_TYPE, "_", WINDOW_SIZE, ".rds")))






