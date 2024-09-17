rm(list=ls())
threshold <- 2 
MA_WINDOW_SIZE <- 52 * 1

# load residuals
path <- "/Users/danieloliveira/Documents/Daniel/codes/sgd-trading/src/data/outputs/dlm/noscale/model_results_weekly_520_nointercept.rds"
obj <- readRDS(path)
resid <- obj$residuals$res
betas <- obj$filter$m
betas_matrix <- betas %>% select(-date) %>% as.matrix()
prices <- obj$prices %>% filter(date>=betas$date[1])
prices_matrix <- prices %>% select(-date) %>% as.matrix()

write.csv(resid, "resid.csv")
write.csv(betas, "betas.csv")
write.csv(prices, "prices.csv")

resid <- obj$residuals$res$residual

# scale residuals
mean <- rollapply(resid, width = MA_WINDOW_SIZE, FUN = mean, align = "right", partial = TRUE)
std <- rollapply(resid, width = MA_WINDOW_SIZE, FUN = sd, align = "right", partial = TRUE)
scaled_resid <- na.fill((resid - mean) / std, 0)

scaled_resid_df <- as.data.table(scaled_resid) %>%
  mutate(date=obj$residuals$res$date) %>%
  select(date, everything())

write.csv(scaled_resid_df, "scaled_resid_df.csv")

# plot resid
ts.plot(resid)
ts.plot(scaled_resid)

# plot cumulative resid
ts.plot(cumsum(resid))
ts.plot(cumsum(scaled_resid))

# build signals from residuals
signal <- ifelse(scaled_resid > threshold,
                 betas_matrix,
                 ifelse(scaled_resid < -threshold,
                        -1 * betas_matrix,
                        0))
signal <- matrix(NA, nrow = length(scaled_resid), ncol = dim(betas_matrix)[2]+1)
for (i in 1:length(scaled_resid)){
  scale_resid_t <- scaled_resid[i]
  if (scale_resid_t > threshold){
    signal[i,] <- c(-1, betas_matrix[i,])
  }else if (scale_resid_t < -threshold){
    signal[i,] <- c(1, betas_matrix[i,] * -1)
  }else{
    signal[i,] <- c(0, betas_matrix[i,] * 0)
  }
}

# build portfolio
lead_diff_prices_matrix <- na.fill(dplyr::lead(diff(prices_matrix), n = 1), 0)
asset_returns <- matrix(NA, nrow = dim(signal)[1]-1, ncol = dim(signal)[2])
for (i in 1:(dim(signal)[1]-1)){
  asset_returns[i,] <- signal[i,] * lead_diff_prices_matrix[i,]
}

# portfolio returns
portfolio_returns <- rowSums(asset_returns)
cum_portfolio_returns <- cumsum(portfolio_returns)
assets_cum_returns <- cumsum(asset_returns %>% as.data.table())

# plot cumulative sum of returns
ts.plot(cum_portfolio_returns[1:950])
ts.plot(assets_cum_returns$V2)


