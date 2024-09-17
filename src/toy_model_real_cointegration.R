rm(list=ls())
threshold <- 2 
MA_WINDOW_SIZE <- 52 * 1

# load residuals
path <- "/Users/danieloliveira/Documents/Daniel/codes/sgd-trading/src/data/outputs/dlm/noscale/model_results_weekly_520_nointercept.rds"
obj <- readRDS(path)
resid <- obj$residuals$res$residual

# scale residuals
mean <- rollapply(resid, width = MA_WINDOW_SIZE, FUN = mean, align = "right", partial = TRUE)
std <- rollapply(resid, width = MA_WINDOW_SIZE, FUN = sd, align = "right", partial = TRUE)
scaled_resid <- na.exclude((resid - mean) / std)

# plot resid
ts.plot(resid)
ts.plot(scaled_resid)

# plot cumulative resid
ts.plot(cumsum(resid))
ts.plot(cumsum(scaled_resid))

# build signals from residuals
signal <- ifelse(scaled_resid > threshold,
                 -1,
                 ifelse(scaled_resid < -threshold,
                        1,
                        0))
signal_df <- as.data.table(signal) %>% 
  mutate(date=obj$residuals$res$date[2:length(obj$residuals$res$date)]) %>%
  select(date, everything())

# build portfolio
portfolio_returns <- signal * na.fill(dplyr::lead(diff(resid), n = 1), 0)

# plot cumulative sum of returns
ts.plot(cumsum(portfolio_returns))



