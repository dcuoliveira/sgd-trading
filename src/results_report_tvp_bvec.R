rm(list=ls())
library("here")

OUTPUT_PATH = file.path(here(), 'src', 'data', 'outputs')
MODEL_NAME = "tvp-bvec"

# list files in dir
files = list.files(file.path(OUTPUT_PATH, MODEL_NAME))

# get file name
file = files[2]

# load results
results = readRDS(file.path(OUTPUT_PATH, MODEL_NAME, file))

# print model name
print(paste0("Model: ", file))

# print model results
summary(results)

# print time to run model
print(paste0("Time to run: ", round(results$runtime, 1), " hours"))

results
