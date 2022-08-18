rm(list=ls())
library('dplyr')
library("tidyr")
library("data.table")
library("MTS")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))

MODEL <- "dlm"
OUTPUT_PATH <- file.path(getwd(), 'src', 'data', 'outputs', MODEL)
TARGET <- "SGD"
SCALE_TYPE <- "rolling_scale"

dlmout <- readRDS(file = file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, ".rds")))
cointegration_error <- dlmout$residuals$res %>% mutate(ewma_vol=EWMAvol(residual,lambda = 0.8)$Sigma.t) %>%
  mutate(ub=1.4*sqrt(ewma_vol), lb=-1.4*sqrt(ewma_vol))

ggplot(data = cointegration_error, mapping = aes(x=date)) +
  geom_line(mapping = aes(x=date, y=residual, colour="conit error")) + 
  geom_line(mapping = aes(x=date, y=ub, colour="bounds")) + 
  geom_line(mapping = aes(x=date, y=lb, colour="bounds"))
