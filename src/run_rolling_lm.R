rm(list=ls())
library("rollRegres")
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("hrbrthemes")

source(here('src', 'utils.R'))
source(here('src', 'models.R'))
source(here('src', 'plot_funcs.R'))

MODEL <- "rolling_reg"
OUTPUT_PATH <- here("src", "data", "outputs", "models", MODEL)
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
