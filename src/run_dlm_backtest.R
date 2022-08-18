rm(list=ls())
library('dplyr')
library("tidyr")
library("data.table")
library("MTS")
library("lubridate")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))

MODEL <- "dlm"
OUTPUT_PATH <- file.path(getwd(), 'src', 'data', 'outputs', MODEL)
TARGET <- "SGD"
SCALE_TYPE <- "rolling_scale"

# prices data
prices_df <- load_and_resample_currencies() %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")

# dlm output data  
dlmout <- readRDS(file = file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, ".rds")))

# cointegration error
cointegration_error_df <- dlmout$residuals$res %>% mutate(ewma_vol=EWMAvol(residual,lambda = 0.8)$Sigma.t) %>%
  mutate(ub=1.4*sqrt(ewma_vol), lb=-1.4*sqrt(ewma_vol))

# dlm betas - pode melhorar aqui colocando o filter ao invés do smooth
betas_df <- dlmout$smooth$s

# positions for the target (y) variable
positions_df <- data.frame(date=cointegration_error_df$date,
                           USDSGD=ifelse(cointegration_error_df["residual"] >= cointegration_error_df["ub"],
                                        -1,
                                        ifelse(cointegration_error_df["residual"] < cointegration_error_df["lb"],
                                               1,
                                               0)))
colnames(positions_df) <- c("date", TARGET)

# positions for the regressors (X)
positions_betas_df <- merge(x = positions_df, y = betas_df, by = "date") %>% select(-date, -sym(TARGET))

out_positions_betas_list <- list()
for (i in 1:nrow(positions_betas_df)){
  betas_row <- positions_betas_df[i,]
  coint_error_row <- positions_df[i,]
  
  if (coint_error_row[[TARGET]] > 0){
    out_positions_betas_list[[i]] <- betas_row * -1 
  }else if (coint_error_row[[TARGET]] < 0){
    out_positions_betas_list[[i]] <- betas_row * 1 
  }else{
    out_positions_betas_list[[i]] <- betas_row * 0 
  }
  
}
out_positions_betas_df <- do.call("rbind", out_positions_betas_list) %>% as.data.table()

# put all positions together
out_positions_df <- cbind(positions_df, out_positions_betas_df) %>% as.data.table()  %>% select(-intercept)

# returns data
prices_dtref <- prices_df$date
returns_df <- prices_df %>% select(-date)
returns_df <- (returns_df - lag(returns_df)) / lag(returns_df)
returns_df$date <- prices_dtref
returns_df <- returns_df %>% select(date, everything()) %>% drop_na()

# merge positions and returns 
returns_df <- merge(out_positions_df %>% select(date), returns_df, by = "date")

# check if columns are sorted correctly
returns_list = list()
for (colname in colnames(out_positions_df)){
  if (colname == "date"){
    next
  }else{
    returns_list[[colname]] <- returns_df[[colname]]
  }
}
returns_df <- do.call("cbind", returns_list) %>% as.data.table() %>% 
  mutate(date=out_positions_df$date) %>% select(date, everything())
lead_returns_df <- cbind(data.frame(date=returns_df$date[1:(dim(returns_df)[1]-1)]),
                         lead(returns_df %>% select(-date)) %>% drop_na())%>% as.data.table()

# generate stretegy returns
out_positions_df <- merge(out_positions_df, lead_returns_df %>% select(date), by = "date")
strategy_returns_df <- (out_positions_df %>% select(-date)) * (lead_returns_df %>% select(-date))
cum_strategy_returns_df <- cumprod(1 + strategy_returns_df)

# add dates
strategy_returns_df$date <- out_positions_df$date
strategy_returns_df <- strategy_returns_df %>% select(date, everything())
cum_strategy_returns_df$date <- out_positions_df$date
cum_strategy_returns_df <- cum_strategy_returns_df %>% select(date, everything())

# plots
## cummulative return of the portfolio
ggplot(data = data.frame(date=strategy_returns_df$date, rets=cumprod(1+rowSums(strategy_returns_df %>% select(-date)))),
       mapping = aes(x = date, y = rets)) + 
  geom_line() + 
  ggtitle("Cummulative returns os $1 invested in the portfolio")

## cummulative return of the portfolio
melt_cum_strategy_returns_df <- cum_strategy_returns_df %>% melt("date")
ggplot(data = melt_cum_strategy_returns_df, 
       mapping = aes(x = date, y = value, colour = variable, group = variable)) +
  geom_line() +
  ggtitle("Cummulative returns os $1 invested in each asset")

## cointegration error
ggplot(data = cointegration_error_df, mapping = aes(x=date)) +
  geom_line(mapping = aes(x=date, y=residual, colour="cointegration error")) +
  geom_line(mapping = aes(x=date, y=ub, colour="bounds")) +
  geom_line(mapping = aes(x=date, y=lb, colour="bounds"))



