
# TVP-VEC ----

rm(list = ls())

# Load package
library("bvartools")

# Load data
data("us_macrodata")

# Plot the data
plot(us_macrodata)

# Generate the basic model
temp <- gen_vec(
  data = us_macrodata, # endogenous variables
  p = 1, # endogenous variables lag order (var model)
  exogen = NULL, # exogenous variables
  s = NULL, # exogenous variables lag order
  r = 0:2, # cointegration rank. It tests r = (0, 1, 2).
  const = "unrestricted", # if a constant should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  trend = NULL, # if a trend should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  seasonal = NULL, # if seasonal dummies should be added to the error correction term (restricted) or the non-cointegration term (restricted)
  structural = FALSE, # if the "data" should be prepared for the estimation of a svar
  tvp = TRUE, # if the model parameters are time varying
  sv = FALSE, # if time varying error variances should be added (stoch vol)
  fcst = NULL, # number of observations saved for forecasting
  iterations = 100, # mcmc draws excluding burn-in
  burnin = 5000 # number of mcmc draws to initialize the sampler
                )

start_time <- Sys.time()

# Add priors to the "empty" models
temp <- add_priors(
  temp,
  coef = list(v_i = 1 / 9,
              v_i_det = 1 / 100,
              shape = 3, # a numeric or character specifying the prior shape parameter of the error term. default is k.s
              rate = .0001,
              rate_det = .0001),
  coint = list(shape = 3, # ??
               rate = 1e-04, # var(\nu_t) - state error variance priors
               rho = 0.9999), # var(\b_t) = I_{nr}\frac{1}{1-\rho^2} - variance of the prior on the model betas
  sigma = list(df = "k", scale = .0001) # measurement error variance priors
  )

# Run Gibbs sampler
result <- draw_posterior(temp)

end_time <- Sys.time()
difference <- difftime(end_time, start_time, units='mins')
print(difference)

# Get test statistics for the models to select the final one
tests <- summary(result)
tests

# Plot the posterior draws of each model
plot(result)

# Translate the final model into VAR
res_var <- bvec_to_bvar(result[[3]])

# General summary stats the model
summary(res_var)

# Plot the VAR draws
plot(res_var)

# Forecasting
prd <- predict(res_var, n.ahead = 10, new_d = rep(1, 10))
plot(prd)

# Impulse response
ir <- irf(res_var, impulse = "r", response = "Dp", type = "gir")
plot(ir)
