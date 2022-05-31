rm(list=ls())
library("rollRegres")
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("roll")

source(here('src', 'utils.R'))
source(here('src', 'models.R'))
source(here('src', 'plot_funcs.R'))

MODEL = "rolling_reg"
OUTPUT_PATH = here("src", "data", "outputs", "models", MODEL)
WINDOW_SIZE = 52 * 4
MEAN_WINDOW_SIZE = 52 * 1
INTERCEPT = FALSE
TARGET = "SGD"

data = merge_fx_sneer_data()
data_orig = data %>% select(-date)
date = data$date
data =  data %>% select(-date)
# data =  data %>% select(-date) %>% apply(2, function(x) scale(x)) %>% as.data.frame()

if (INTERCEPT == T){
  model_formula = paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER", names(data))], collapse=" + "))
}else{
  model_formula = paste(paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER", names(data))], collapse=" + ")), " -1")
}

rollingreg = roll_reg_prop(formula = model_formula,
                           data = data %>% select(-SNEER),
                           window_size = WINDOW_SIZE,
                           do_compute=c("sigmas", "r.squareds", "1_step_forecasts"))



