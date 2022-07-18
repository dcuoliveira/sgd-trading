rm(list=ls())
library("rollRegres")
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("hrbrthemes")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))
source(file.path(getwd(), 'src', 'plots', 'plot_funcs.R'))

MODEL <- "rolling_lm"
OUTPUT_PATH <- file.path(getwd(), 'src', 'data', 'outputs', MODEL)
WINDOW_SIZE <- 52 * 2
MEAN_WINDOW_SIZE <- 52 * 1
INTERCEPT <- TRUE
TARGET <- "SGD"

data <- merge_fx_sneer_data() %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")

if (INTERCEPT == T){
  model_formula <- paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + "), "+1")
}else{
  model_formula <- paste(paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + ")), " -1")
}

rollingreg <- roll_reg_prop(formula = model_formula,
                            target_name=TARGET,
                            data = data %>% select(-SNEER),
                            reg_window_size = WINDOW_SIZE,
                            mean_window_size = MEAN_WINDOW_SIZE,
                            do_compute=c("sigmas", "r.squareds", "1_step_forecasts"))

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
saveRDS(rollingreg, file.path(OUTPUT_PATH, "model_results.rds"))

