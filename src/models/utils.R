library('padr')
library('lubridate')
library('zoo')
library("timeDate")

calculate_holding_periods <- function(positions) {
  # Initialize variables
  holding_periods <- numeric()
  current_period <- 0
  in_position <- FALSE
  
  # Loop through positions
  for(i in 1:length(positions)) {
    if(positions[i] != 0) {  # If there's a position (either long or short)
      if(!in_position) {  # Starting a new position
        in_position <- TRUE
        current_period <- 1
      } else {  # Continuing existing position
        # Check if it's the same type of position (long or short)
        if(i > 1 && positions[i] == positions[i-1]) {
          current_period <- current_period + 1
        } else {  # Different type of position
          if(current_period > 0) {
            holding_periods <- c(holding_periods, current_period)
          }
          current_period <- 1
        }
      }
    } else {  # No position
      if(in_position && current_period > 0) {
        holding_periods <- c(holding_periods, current_period)
        current_period <- 0
      }
      in_position <- FALSE
    }
    
    # Handle the last position if we're at the end of the series
    if(i == length(positions) && current_period > 0) {
      holding_periods <- c(holding_periods, current_period)
    }
  }
  
  # Calculate statistics
  results <- list(
    average_holding_period = mean(holding_periods),
    median_holding_period = median(holding_periods),
    min_holding_period = min(holding_periods),
    max_holding_period = max(holding_periods),
    std_holding_period = sd(holding_periods),
    number_of_trades = length(holding_periods),
    all_holding_periods = holding_periods
  )
  
  return(results)
}

rename_tvp_params = function(df, names, r) {
  k = 1
  j = 1
  for (i in 1:dim(df)[2]){
    if (k > length(names)){
      j = j + 1
      k = 1
    }
    colnames(df)[i] = paste0(names[k], "_r", j)
    k = k + 1
  }
  return(df)
}

load_package <- function(package) {
  tryCatch(
    {
      library(package, character.only = TRUE)
      message(paste("Package", package, "loaded successfully."))
    },
    error = function(e) {
      message(paste("Error: Unable to load package", package, "- ", e$message))
    }
  )
}

gen_Y_rho_psi = function(rho_xi_post,
                         Yt,
                         delta_Yt,
                         T,
                         k){
  y_beta2 <- matrix(0, nrow = 1, ncol = (T-k))
  for(t in 1:(T-k)){
    aux <- 0
    aux <- rho_xi_post[1]*Yt[(t+k-1)]
    for(j in 1:(k-1)){
      aux <-  aux + rho_xi_post[(j+1)]*delta_Yt[(t+k-j)]
    }
    
    y_beta2[,t] <- aux
  }
  return(y_beta2)
}

gen_X_beta2 = function(rho_xi_post,
                       Xt,
                       delta_Xt,
                       T,
                       k,
                       n) {
  x_beta2 <- matrix(0, nrow = n, ncol = (T-k))
  for(t in 1:(T-k)){
    aux <- 0
    aux <- rho_xi_post[1]*Xt[,(t+k-1)]
    for(j in 1:(k-1)){
      aux <-  aux + rho_xi_post[(j+1)]*delta_Xt[,(t+k-j)]
    }
    x_beta2[,t] <- aux
  }
  return(x_beta2)
}

gen_X_rho_xi = function(Rt,
                        delta_Rt,
                        T,
                        k){
  #Creating X matrix
  x_vec <- c(Rt[k:(T-1)])
  for(i in k:2){
    aux <- delta_Rt[i:(T+1-i)]
    x_vec <- c(x_vec,aux)
  }
  return(matrix(x_vec, nrow = k, byrow = TRUE))
}

polycreate = function(x,
                      order){
  y = 0
  for (p in 1:order){
    y = y + x^p
  }
  return(y)
}

h_epsilon_poly = function(epsilon,
                          p){
  return(polycreate(x=epsilon,
                    p=p))

}

data_frame_to_ts_list = function(df,
                                 freq_vec){
  
}

resample_data = function(df, freq){
  # create missing days and delete weekends
  df = df %>% mutate(date=ymd(date)) %>% pad()
  
  if (freq == 'weekly'){
    df = df %>% mutate(weekday=weekdays(date, abbreviate = TRUE)) %>% filter(weekday=='sex'|weekday=='Sex'|weekday=='Fri'|weekday=='fri') %>% select(-weekday)
  }else {
     
  }
  # fill na's forward
  dtref <- df$date
  df <- df %>% select(-date) %>% lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% do.call(cbind, .) %>% as.data.table() %>%
    mutate(date=dtref) %>% select(date, everything())
  
  return(df)
}

# Define the function to resample weekly data to monthly
resample_to_monthly <- function(data) {
  return(data %>%
    # Ensure the date column is of Date type
    mutate(date = as.Date(date)) %>%
    # Group by year and month
    group_by(year = year(date), month = month(date)) %>%
    # Summarize by taking the last entry of each group
    summarise(across(everything(), last)) %>%
    ungroup() %>%
    # Create a proper date column for the first day of the month
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    # Select the columns in the desired order
    select(date, everything(), -year, -month))
}

load_and_resample_currencies = function(freq, invert_quotes=TRUE){
  
  orig_df = read.csv(here('src', 'data', 'inputs', 'daily-currencies-new.csv'), sep = ";") %>%
    mutate(date=ymd(date)) %>%
    pad() %>%
    na.locf(., fromLast = TRUE)
    
  if (freq == 'weekly'){
    out_df = orig_df %>%
      mutate(weekday=weekdays(date, abbreviate = TRUE)) %>%
      filter(weekday=='sex'|weekday=='Sex'|weekday=='Fri'|weekday=='fri') %>%
      select(-weekday)
  }else if (freq == 'monthly'){
    out_df <- orig_df %>%
      mutate(date = as.Date(date))

    # Create a timeDate object for business day calculations
    data_timeDate <- as.timeDate(out_df$date)
    out_df$isBizday <- isBizday(data_timeDate)
    
    # Group by year and month
    grouped_data <- out_df %>%
      group_by(year = year(date), month = month(date)) %>%
      # Summarize to find the last business day of each month
      summarise(last_biz_day = max(date[isBizday])) %>%
      ungroup()

    # Join with the original data to get the rows corresponding to the last business day
    out_df <- grouped_data %>%
      left_join(out_df, by = c("last_biz_day" = "date")) %>%
      select(last_biz_day, everything(), -year, -month, -USD.Curncy, -isBizday) %>%
      rename(date = last_biz_day) %>% as.data.table()
  }
  
  out_df = na.locf(na.locf(out_df), fromLast = TRUE)
  colnames(out_df) = unlist(lapply(colnames(out_df), function(x){strsplit(x, '.', fixed = TRUE)[[1]][1]}))
  
  if (invert_quotes){
    dtref <- out_df$date
    out_df <- out_df %>% select(-date)
    out_df <- 1 / out_df
    out_df$date <- dtref
    out_df <- out_df %>% select(date, everything())
  }
  
  rownames(out_df) = out_df$date

  return(out_df)
}

merge_fx_sneer_data = function(){

  fx_data = read.csv(here('src', 'data', 'inputs', 'currencies.csv')) %>% mutate(date=ymd(date)) %>% select(-X)
  sneer_data = read.csv(here('src', 'data', 'inputs', 'sneer.csv')) %>% mutate(date=ymd(date)) %>%
    pad() %>% mutate(weekday=weekdays(date, abbreviate = TRUE)) %>% filter(weekday=='Sex'|weekday=='Fri') %>% select(-weekday, -X)
  
  merge_data = merge(sneer_data, fx_data) 
  merge_data = na.locf(na.locf(merge_data), fromLast = TRUE)
  colnames(merge_data) = unlist(lapply(colnames(merge_data), function(x){strsplit(x, '.', fixed = TRUE)[[1]][1]}))
  rownames(merge_data) = merge_data$date
  
  return(merge_data)
}

posteriors_draws_koopetal2010 = function(data){
  # Reset random number generator for reproducibility
  set.seed(221994)
  
  # Obtain data matrices
  y <- t(data$data$Y)
  w <- t(data$data$W)
  x <- t(data$data$X)
  
  r <- data$model$rank # Set rank
  
  tt <- ncol(y) # Number of observations
  k <- nrow(y) # Number of endogenous variables
  k_w <- nrow(w) # Number of regressors in error correction term
  k_x <- nrow(x) # Number of differenced regressors and unrestrictec deterministic terms
  k_gamma <- k * k_x # Total number of non-cointegration coefficients
  
  k_alpha <- k * r # Number of elements in alpha
  k_beta <- k_w * r # Number of elements in beta
  
  # Priors
  a_mu_prior <- data$priors$noncointegration$mu # Prior means
  a_v_i_prior <- data$priors$noncointegration$v_i # Inverse of the prior covariance matrix
  
  v_i <- data$priors$cointegration$v_i
  p_tau_i <- data$priors$cointegration$p_tau_i
  
  sigma_df_prior <- data$priors$sigma$df # Prior degrees of freedom
  sigma_scale_prior <- data$priors$sigma$scale # Prior covariance matrix
  sigma_df_post <- tt + sigma_df_prior # Posterior degrees of freedom
  
  # Initial values
  beta <- matrix(0, k_w, r)
  beta[1:r, 1:r] <- diag(1, r)
  
  sigma_i <- diag(1 / .0001, k)
  
  g_i <- sigma_i
  
  iterations <- data$model$iterations # Number of iterations of the Gibbs sampler
  burnin <- data$model$burnin # Number of burn-in draws
  draws <- iterations + burnin # Total number of draws
  
  # Data containers
  draws_alpha <- matrix(NA, k_alpha, iterations)
  draws_beta <- matrix(NA, k_beta, iterations)
  draws_pi <- matrix(NA, k * k_w, iterations)
  draws_gamma <- matrix(NA, k_gamma, iterations)
  draws_sigma <- matrix(NA, k^2, iterations)
  
  # Start Gibbs sampler
  for (draw in 1:draws) {
    
    # Draw conditional mean parameters
    temp <- post_coint_kls(y = y, beta = beta, w = w, x = x, sigma_i = sigma_i,
                           v_i = v_i, p_tau_i = p_tau_i, g_i = g_i,
                           gamma_mu_prior = a_mu_prior,
                           gamma_v_i_prior = a_v_i_prior)
    alpha <- temp$alpha
    beta <- temp$beta
    Pi <- temp$Pi
    gamma <- temp$Gamma
    
    # Draw variance-covariance matrix
    u <- y - Pi %*% w - matrix(gamma, k) %*% x
    sigma_scale_post <- solve(tcrossprod(u) + v_i * alpha %*% tcrossprod(crossprod(beta, p_tau_i) %*% beta, alpha))
    sigma_i <- matrix(rWishart(1, sigma_df_post, sigma_scale_post)[,, 1], k)
    sigma <- solve(sigma_i)
    
    # Update g_i
    g_i <- sigma_i
    
    # Store draws
    if (draw > burnin) {
      draws_alpha[, draw - burnin] <- alpha
      draws_beta[, draw - burnin] <- beta
      draws_pi[, draw - burnin] <- Pi
      draws_gamma[, draw - burnin] <- gamma
      draws_sigma[, draw - burnin] <- sigma
    }
  }
  beta <- apply(t(draws_beta) / t(draws_beta)[, 1], 2, mean) # Obtain means for every row
  beta <- matrix(beta, k_w) # Transform mean vector into a matrix
  beta <- round(beta, 3) # Round values
  dimnames(beta) <- list(dimnames(w)[[1]], NULL) # Rename matrix dimensions
  
  # Number of non-deterministic coefficients
  k_nondet <- (k_x - 4) * k
  return(list(draws_pi=draws_pi,
              draws_gamma=draws_gamma,
              draws_sigma=draws_sigma,
              beta=beta,
              k_nondet=k_nondet))
}



