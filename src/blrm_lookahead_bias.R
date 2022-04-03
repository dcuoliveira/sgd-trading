rm(list=ls())
library('rstanarm')
library('bayestestR')
library('bayesplot')
library('purrr')
library('insight')
library('here')
library('dplyr')
source(here('src', 'utils.R'))

data = merge_fx_sneer_data()
data_orig = data %>% select(-date)
date = data$date
data =  data %>% select(-date) %>% apply(2, function(x) scale(x)) %>% as.data.frame()

# Bayesian normal linear regression with normal priors
bnlr = stan_glm(SGD ~ KRW + MYR + CNY + THB + IDR + TWD +
                      INR + JPY + EUR + AUD + GBP, data=data_orig, seed=2294)
print(bnlr, digits = 3)

# Posterior of the parameters
mcmc_dens(bnlr, pars = c("KRW"))
mcmc_dens(bnlr, pars = c("MYR"))
mcmc_dens(bnlr, pars = c("CNY"))
mcmc_dens(bnlr, pars = c("THB"))
mcmc_dens(bnlr, pars = c("IDR"))
mcmc_dens(bnlr, pars = c("TWD"))
mcmc_dens(bnlr, pars = c("INR"))
mcmc_dens(bnlr, pars = c("JPY"))
mcmc_dens(bnlr, pars = c("EUR"))
mcmc_dens(bnlr, pars = c("AUD"))
mcmc_dens(bnlr, pars = c("GBP"))

# Full description of the model parameters
describe_posterior(bnlr)

# MaximuM a posterior (MAP) estimate 
post = get_parameters(bnlr)
print(map_dbl(post, map_estimate),digits = 3)

fitted_df = data.frame(date=date,
                       realsgd=data_orig$SGD,
                       fittedsgd=bnlr$fitted.values,
                       rollingmean=rollapply(bnlr$fitted.values, 30, mean, fill=NA),
                       rollingstd=rollapply(bnlr$fitted.values, 30, sd, fill=NA),
                       cointerror=bnlr$residuals)

plot(fitted_df$date, fitted_df$cointerror, type = 'l', col = 'red')
par(new = 'T')
plot(fitted_df$date, fitted_df$fittedsgd, type = 'l', col = 'black')
par(new = 'T')
plot(fitted_df$date, fitted_df$realsgd, type = 'l', col = 'blue')
par(new = 'T')
plot(fitted_df$date, fitted_df$rollingmean + 2*fitted_df$rollingstd, type = 'l', col = 'green')
par(new = 'T')
plot(fitted_df$date, fitted_df$rollingmean - 2*fitted_df$rollingstd, type = 'l', col = 'green')


PP.test(bnlr$residuals)

# Highest density interval
hdi(bnlr)

