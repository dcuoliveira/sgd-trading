rm(list=ls())
n <- 52*30
mean <- 0
sd <- 1
threshold <- 1

# sample residuals from clearly stationary process
resid <- rnorm(n = n, mean = mean, sd = sd)

# plot resid
ts.plot(resid)

# plot cumulative resid
ts.plot(cumsum(resid))

# build signals from residuals
sample_sd <- sd(resid)
signal <- ifelse(resid >= threshold * sample_sd,
                 -1,
                 ifelse(resid < threshold * -sample_sd,
                        1,
                        0))

# build portfolio
portfolio_returns <- signal * diff(resid)

# plot cumulative sum of returns
ts.plot(cumsum(portfolio_returns))



