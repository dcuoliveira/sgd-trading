rm(list=ls())
library("readxl")
library("lubridate")

INPUT_PATH <- file.path(getwd(), 'src', 'data', 'inputs')
TARGET_FILE_NAME <- "Currencies_pxlast.xlsx"
SHEET_NAME <- "Sheet2"

df <- read_excel(file.path(INPUT_PATH, TARGET_FILE_NAME), sheet = SHEET_NAME) %>%
  rename(date=Dates) %>% mutate(date=ymd(date))
write.csv(df,
           file = file.path(INPUT_PATH, "currencies.csv"))
