rm(list=ls())
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")
library("reshape2")

source(here('src', 'utils.R'))
source(here('src', 'models.R'))
source(here('src', 'plot_funcs.R'))

MODEL <- "reg"
OUTPUT_PATH <- here("src", "data", "outputs", "models", MODEL)
WINDOW_SIZE <- 52 * 4
MEAN_WINDOW_SIZE <- 52 * 1
INTERCEPT <- TRUE
TARGET <- "SGD"

data <- merge_fx_sneer_data() %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")
data_orig <- data
data <- data %>% select(-date) # %>% apply(2, function(x) scale(x)) %>% as.data.frame()

if (INTERCEPT == T){
  model_formula <- paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + "), "+1")
}else{
  model_formula <- paste(paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + ")), " -1")
}

reg <- lm(model_formula, data)
summary(reg)

resid <- reg$residuals %>% as.data.table() %>%
  rename(resid := .) %>%
  mutate(date=data_orig$date, resid_zscore=scale(resid)) %>%
  select(date, everything())
ggplot(resid, aes(x = date, y = resid_zscore)) + geom_line()

diff_df = data_orig %>% select(date, SGD) %>%
  mutate(SGD_log_diff=c(NA, diff(log(SGD))), resid=resid$resid, resid_diff=c(NA, diff(resid)))
ggplot(diff_df, aes(x=resid_diff, y=lag(SGD_log_diff, n = 1))) + geom_point() + geom_smooth(method=lm)





