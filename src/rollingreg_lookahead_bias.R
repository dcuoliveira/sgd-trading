rm(list=ls())
library("rollRegres")
library('here')
library('dplyr')
library("data.table")

source(here('src', 'utils.R'))
source(here('src', 'plot_funcs.R'))

MODEL = "rolling_reg"
MODEL_TYPE = "lookahead_bias"
OUTPUT_PATH = here("src", "data", "outputs", "models", MODEL, MODEL_TYPE)
WINDOW_SIZE = 52 * 4
INTERCEPT = FALSE

data = merge_fx_sneer_data()
data_orig = data %>% select(-date)
date = data$date
data =  data %>% select(-date) %>% apply(2, function(x) scale(x)) %>% as.data.frame()

if (INTERCEPT == T){
  model_formula = paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER", names(data))], collapse=" + "))
}else{
  model_formula = paste(paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER", names(data))], collapse=" + ")), " -1")
}

rollingreg = roll_regres(formula = model_formula,
                         data = data,
                         width = WINDOW_SIZE,
                         do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))
rollingreg$dates = date
rollingreg$coefs = rollingreg$coefs %>% as.data.table() %>% mutate(date=date) %>% select(date, everything())


