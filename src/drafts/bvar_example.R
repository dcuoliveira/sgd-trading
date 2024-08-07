library("bvartools")

# Load data
data("e1")
e1 <- diff(log(e1)) * 100

# Reduce number of oberservations
e1 <- window(e1, end = c(1978, 4))
plot(e1)

# Define the VAR model
model <- gen_var(e1,
                 p = 1, 
                 deterministic = "const",
                 iterations = 5000, 
                 burnin = 1000)

# Define the priors over the VAR model parameters
## coef = list(a_mu = prior means, v_i = prior precisisions) => prior: P(coef) = N(a_mu, diag(v_i))
## Recall that the prior precision is the inverse of the prior variance
## sigma = list(df = prior degrees of freedom, scale = prior scale) =>  P(sigma) = W(df, scale)
##
## References on Koop (2008) - Bayesian Econometrics, Section 6.6
model_with_priors <- add_priors(model,
                                coef = list(mu = 0, v_i = 0),
                                sigma = list(df = 1, scale = .0001))

# Draw posteriors
bvar_est <- draw_posterior(model_with_priors)

# Inspect posteriors draws
# plot(bvar_est)
# plot(bvar_est, type = "trace")

# Model summary
summary(bvar_est)

# Plot estimates
plot(bvar_est)

# Model predictions
# bvar_pred <- predict(bvar_est, n.ahead = 10, new_d = rep(1, 10))
# plot(bvar_pred)



