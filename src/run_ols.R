rm(list=ls())
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("reshape2")
library("optparse")

source(file.path(here(), 'src', 'models', 'utils.R'))
source(file.path(here(), 'src', 'models', 'models.R'))
source(file.path(here(), 'src', 'plots', 'plot_funcs.R'))

# define command-line options
option_list <- list(
  make_option(c("--model_name"), type = "character", help = "Model name for output", default = "ols"),
  make_option(c("--output_path"), type = "character", help = "Output path", default = file.path(here(), 'src', 'data', 'outputs')),
  make_option(c("--frequency"), type = "character", help = "Frequency to parse the data", default = "weekly"),
  make_option(c("--intercept"), type = "logical", help = "Intercept", default = FALSE),
  make_option(c("--scale_type"), type = "character", help = "Scale type", default = "scale"),
  make_option(c("--num_cores"), type = "integer", help = "Number of cores", default = 1),
  make_option(c("--window_size"), type = "integer", help = "Window size", default = 52 * 2),
  make_option(c("--target"), type = "character", help = "Target of the model", default = "SGD")
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
TARGET <- args$target

data <- load_and_resample_currencies(freq=FREQ, invert_quotes=FALSE) %>% mutate(date=ymd(date)) %>% filter(date >= "2005-01-01")
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
  dates <- ymd(row.names(data))
}else if (SCALE_TYPE == "rolling_scale"){
  n <- dim(data)[1]
  mean_data <- apply(data, 2, function(x) {roll_mean(x, width = n, min_obs = WINDOW_SIZE)})
  sd_data <- apply(data, 2, function(x) {roll_sd(x, width = n, min_obs = WINDOW_SIZE)})
  
  data <- ((data - mean_data) / sd_data)
  row.names(data) <- data_orig$date
  data <- data %>% drop_na()
  dates <- ymd(row.names(data))
}else if (SCALE_TYPE == "noscale"){
  dates <- ymd(row.names(data))
}

ols <- lm(formula = model_formula,
          data = data)

# residuals
residuals <- data.table(date=dates, residual=ols$residuals)
residuals$date <- ymd(residuals$date)
ts.plot(residuals$residual)

# expand coef vector to matrix
n_rows <- dim(residuals)[1]
coefs_df <- matrix(rep(ols$coefficients, n_rows), nrow = n_rows, byrow = TRUE) %>%
  as.data.table()
colnames(coefs_df) = names(ols$coefficients)
coefs_df$date <- ymd(residuals$date) 
coefs_df <- coefs_df %>% select(date, everything())

if (INTERCEPT == T){
  coefs_df <- coefs_df %>% rename("intercept" = `(Intercept)`)
}

# outputs
rolling_ols_out <- list(model=ols,
                        residuals=list(res=residuals),
                        prices=data_orig,
                        filter=list(m=coefs_df))

if (INTERCEPT == T){
  intercept_tag <- "intercept"
}else{
  intercept_tag <- "nointercept"
}

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, SCALE_TYPE), showWarnings = FALSE)
saveRDS(rolling_ols_out, file.path(OUTPUT_PATH, SCALE_TYPE, paste0("model_results_", FREQ, "_", WINDOW_SIZE, "_", intercept_tag, ".rds")))






