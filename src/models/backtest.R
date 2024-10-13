build_signal_from_bounds <- function(data){
  
  dates <- ymd(data$date) 
  signal <- list()
  position_tm1 <- 0
  for (i in 1:length(dates)){
    
    dtref <- dates[i]
    signal_t <- data %>% filter(date==dtref) %>% select(residual) %>% as.numeric()
    ub_t <- data %>% filter(date==dtref) %>% select(ub) %>% as.numeric()
    lb_t <- data %>% filter(date==dtref) %>% select(lb) %>% as.numeric()
    
    if ((position_tm1 == 0)&(signal_t > ub_t)){
      position_t <- -1
    }else if((position_tm1 == 0)&(signal_t < lb_t)){
      position_t <- 1
    }else if ((position_tm1 == 1)&(signal_t < ub_t)){
      position_t <- position_tm1
    }else if((position_tm1 == 1)&(signal_t >= ub_t)){
      position_t <- position_tm1 * -1
    }else if ((position_tm1 == -1)&(signal_t > lb_t)){
      position_t <- position_tm1
    }else if(((position_tm1 == -1)&(signal_t <= lb_t))){
      position_t <- position_tm1 * -1
    }else{
      position_t <- 0
    }
    signal[[i]] <- data.table(date=dtref, signal=position_t) 
    position_tm1 <- position_t
  }
  signal_out <- do.call(rbind, signal) %>% as.data.table()
  return(signal_out)
}

roll_sd_excluding_zeros <- function(x, window_size) {
  if (is.vector(x)) {
    # Apply rolling standard deviation to a vector
    result <- rollapply(
      data = x,
      width = window_size,
      FUN = function(y) {
        y_non_zero <- y[y != 0]
        if (length(y_non_zero) < 2) {
          return(0)
        } else {
          return(sd(y_non_zero))
        }
      },
      by.column = FALSE,
      align = "right",
      fill = NA
    )
    result[is.na(result)] <- 0
    return(result)
  }else{
    # Apply rolling standard deviation to each column of the dataframe
    result <- as.data.frame(lapply(x, function(col) {
      rollapply(
        data = col,
        width = window_size,
        FUN = function(y) {
          y_non_zero <- y[y != 0]
          if (length(y_non_zero) < 2) {
            return(0)
          } else {
            return(sd(y_non_zero))
          }
        },
        by.column = FALSE,
        align = "right",
        fill = NA
      )
    }))
    result[is.na(result)] <- 0
    return(result)
  }
}

run_backtest_cuzzi <- function(signal, prices, betas, target_name){
  
  browser()
  
  # build relevant lists
  positions <- list()
  dtref <- signal$date[1]
  positions[[1]] <- betas %>%
    filter(date==dtref) %>%
    select(-date) %>%
    mutate_all(~ . * 0) %>%
    mutate(!!paste(target_name):=0, date=dtref) %>%
    select(date, sym(target_name), everything())
  position_open <- FALSE
  position_type <- NA
  
  # run backtest
  for (i in 2:dim(signal)[1]) {
    
    # date reference
    dtref <- signal$date[i]
    
    # signal_{t} and signal_{t-1}
    signal_tm1 <- signal[i-1,][["signal"]]
    ub_tm1 <- signal[i-1,][["ub"]]
    lb_tm1 <- signal[i-1,][["lb"]]
    
    signal_t <- signal[i,][["signal"]]
    ub_t <- signal[i,][["ub"]]
    lb_t <- signal[i,][["lb"]]
    
    if (!position_open) { # decision when no position held
      if (signal_t < lb_t) {
        position_open <- TRUE
        sign <- 1
        positions[[i]] <- betas %>%
          filter(date==dtref) %>%
          select(-date) %>%
          mutate_all(~ . * sign) %>%
          mutate(!!paste(target_name):=sign, date=dtref) %>%
          select(date, sym(target_name), everything())      
      } else if (signal_t > ub_t) {
        
        position_open <- TRUE
        position_type <- "sell"
        sign <- -1
        positions[[i]] <- betas %>%
          filter(date==dtref) %>%
          select(-date) %>%
          mutate_all(~ . * sign) %>%
          mutate(!!paste(target_name):=sign, date=dtref) %>%
          select(date, sym(target_name), everything())
      }else{
        positions[[i]] <- positions[[i-1]]
      }
    }else if (position_open) { # decision when a position is held
      if ((position_type == "buy")&(signal_t > 0)&(signal_t < ub_t)) {
        position_open <- FALSE
        position_type <- NA
        positions[[i]] <- betas %>%
          filter(date==dtref) %>%
          select(-date) %>%
          mutate_all(~ . * 0) %>%
          mutate(!!paste(target_name):=0, date=dtref) %>%
          select(date, sym(target_name), everything())
      }else if((position_type == "buy")&(signal_t > 0)&(signal_t > ub_t)){
        position_open <- TRUE
        position_type <- NA
        sign <- -1
        positions[[i]] <- betas %>%
          filter(date==dtref) %>%
          select(-date) %>%
          mutate_all(~ . * sign) %>%
          mutate(!!paste(target_name):=sign, date=dtref) %>%
          select(date, sym(target_name), everything())
      }else if ((position_type == "sell")&(signal_t < 0)&(signal_t > lb_t)) {
        position_open <- FALSE
        position_type <- NA
        positions[[i]] <- betas %>%
          filter(date==dtref) %>%
          select(-date) %>%
          mutate_all(~ . * 0) %>%
          mutate(!!paste(target_name):=0, date=dtref) %>%
          select(date, sym(target_name), everything())
      }else if ((position_type == "buy")&(signal_t < 0)&(signal_t < lb_t)){
        position_open <- TRUE
        position_type <- NA
        sign <- 1
        positions[[i]] <- betas %>%
          filter(date==dtref) %>%
          select(-date) %>%
          mutate_all(~ . * sign) %>%
          mutate(!!paste(target_name):=sign, date=dtref) %>%
          select(date, sym(target_name), everything())
      }else{
        positions[[i]] <- positions[[i-1]]
      }
    }
  }
  return(portfolio_value)
}

run_backtest <- function(signal, prices, betas, target_name, ret_type="log"){

  # compute price difference
  prices_diff <- (prices %>% select(-date)) - lag(prices %>% select(-date), 1)
  prices_diff$date <- prices$date
  prices_diff <- prices_diff %>%
    drop_na() %>%
    select(date, everything())
  
  # compute returns
  if (ret_type == "log"){
    returns <- log(prices %>% select(-date)) - log(lag(prices %>% select(-date), 1))
  }else if (ret_type == "simple"){
    returns <- (prices %>% select(-date)) / lag(prices %>% select(-date), 1) - 1
  }
  returns$date <- prices$date
  returns <- returns %>%
    drop_na() %>%
    select(date, everything())
  
  # start backtest
  poisitions_out <- list()
  prices_diff_out <- list()
  prices_out <- list()
  returns_out <- list()
  for (i in 2:dim(signal)[1]){
    d_t <- signal$date[i]
    d_tp1 <- signal$date[i+1]
    signal_t <- signal[i][["signal"]]
    
    if (is.na(sign)){
      signal_t <- 0
    }
    
    if (signal_t > 0){
      betas_signal_t <- -1
    }else if (signal_t < 0){
      betas_signal_t <- 1
    }else{
      betas_signal_t <- 0
    }
    
    # positions
    beta_names <- colnames(betas[i,] %>% select(-date))
    betas_tmp <- betas %>%
      filter(date==d_t) %>%
      select(-date) %>%
      mutate_all(~ . * betas_signal_t) %>%
      mutate(!!paste(target_name):=signal_t, date=d_tp1) %>%
      select(date, sym(target_name), everything())
    
    # price difference
    prices_diff_tmp <- prices_diff %>%
      filter(date==d_tp1)
    
    # prices to convert pnl
    prices_tmp <- prices %>%
      filter(date==d_tp1)
    
    # returns
    returns_tmp <- returns %>%
      filter(date==d_tp1)
    
    poisitions_out[[i]] <- betas_tmp
    prices_diff_out[[i]] <- prices_diff_tmp
    prices_out[[i]] <- prices_tmp
    returns_out[[i]] <- returns_tmp
  }
  
  positions_out_df <- do.call(rbind, poisitions_out) %>% as.data.table() %>% drop_na()
  prices_diff_out_df <- do.call(rbind, prices_diff_out) %>% as.data.table()
  prices_out_df <- do.call(rbind, prices_out) %>% as.data.table()
  returns_out_df <- do.call(rbind, returns_out) %>% as.data.table()
  
  # currencies pnl
  pnl_df <- (positions_out_df %>% select(-date)) * (prices_diff_out_df %>% select(-date))
  ## convert pnl to usd (assumes prices are in usdxxx and not the contrary)
  pnl_df <- pnl_df * 1 / (prices_out_df %>% select(-date))
  ## compute portfolio pnl
  pnl_df$portfolio <- rowSums(pnl_df)
  ## fix format
  pnl_df$date <- positions_out_df$date
  pnl_df <- pnl_df %>% select(date, everything())
  
  # currencies portfolio returns
  portfolio_returns_df <- (positions_out_df %>% select(-date)) * (returns_out_df %>% select(-date))
  ## compute portfolio returns
  portfolio_returns_df$portfolio <- rowSums(portfolio_returns_df)
  ## vol adjusted portfolio returns
  target_vol <- 35
  portfolio_vol <- roll_sd_excluding_zeros(portfolio_returns_df, 24) * sqrt(52) * 100
  portfolio_scaling <- target_vol / portfolio_vol
  portfolio_scaling[as.matrix(portfolio_scaling) == Inf] <- 0
  portfolio_scaling[as.matrix(portfolio_scaling) > 1000] <- 1000
  portfolio_scaling <- as.data.table(portfolio_scaling)
  vol_adj_portfolio_returns_df <- portfolio_returns_df * portfolio_scaling
  vol_adj_portfolio_returns_df <- vol_adj_portfolio_returns_df %>%
    mutate(date=positions_out_df$date) %>%
    select(date, everything())
  ## fix format
  portfolio_returns_df$date <- positions_out_df$date
  portfolio_returns_df <- portfolio_returns_df %>% select(date, everything())
  
  # currencies cumulative pnl
  cum_pnl_df <- cumsum((pnl_df %>% select(-date)))
  cum_pnl_df$date <- pnl_df$date
  cum_pnl_df <- cum_pnl_df %>% select(date, everything())
  
  # currencies cumulative returns
  if (ret_type == "log"){
    cum_portfolio_returns_df <- cumsum((portfolio_returns_df %>% select(-date)))
  }else if (ret_type == "simple"){
    cum_portfolio_returns_df <- cumprod(1+(portfolio_returns_df %>% select(-date))) - 1
  }
  cum_portfolio_returns_df$date <- portfolio_returns_df$date
  cum_portfolio_returns_df <- cum_portfolio_returns_df %>% select(date, everything())
  
  # currencies cumulative returns - vol adj
  if (ret_type == "log"){
    cum_vol_adj_portfolio_returns_df <- cumsum((vol_adj_portfolio_returns_df %>% select(-date)))
  }else if (ret_type == "simple"){
    cum_vol_adj_portfolio_returns_df <- cumprod(1+(vol_adj_portfolio_returns_df %>% select(-date))) - 1
  }
  cum_vol_adj_portfolio_returns_df$date <- vol_adj_portfolio_returns_df$date
  cum_vol_adj_portfolio_returns_df <- cum_vol_adj_portfolio_returns_df %>% select(date, everything())
  
  browser()
  
  output <- list(
    positions=positions_out_df,
    prices_diff=prices_diff_out_df,
    prices=prices_out_df,
    pnl=pnl_df,
    returns=portfolio_returns_df,
    cum_pnl=cum_pnl_df,
    cum_returns=cum_portfolio_returns_df,
    vol_adj_portfolio_returns=vol_adj_portfolio_returns_df,
    cum_vol_adj_portfolio_returns=cum_vol_adj_portfolio_returns_df
    )
  
  return(output)
}

run_backtest_fixed <- function(signal, prices, betas, target_name){
  
  # compute price difference
  prices_diff <- (prices %>% select(-date)) - lag(prices %>% select(-date), 1)
  prices_diff$date <- prices$date
  prices_diff <- prices_diff %>%
    drop_na() %>%
    select(date, everything())
  
  # compute returns
  # returns <- log(prices %>% select(-date)) - log(lag(prices %>% select(-date), 1))
  returns <- (prices %>% select(-date)) / lag(prices %>% select(-date), 1) - 1
  returns$date <- prices$date
  returns <- returns %>%
    drop_na() %>%
    select(date, everything())
  
  # start backtest
  poisitions_out <- list()
  prices_diff_out <- list()
  prices_out <- list()
  returns_out <- list()
  for (i in 2:dim(signal)[1]){
    d_t <- signal$date[i]
    d_tp1 <- signal$date[i+1]
    signal_t <- signal[i][["signal"]]
    
    if (is.na(sign)){
      signal_t <- 0
    }
    
    if (signal_t > 0){
      betas_signal_t <- -1
    }else if (signal_t < 0){
      betas_signal_t <- 1
    }else{
      betas_signal_t <- 0
    }
    
    # positions
    beta_names <- colnames(betas[i,] %>% select(-date))
    betas_tmp <- betas %>%
      filter(date==d_t) %>%
      select(-date) %>%
      mutate_all(~ . * betas_signal_t) %>%
      mutate(!!paste(target_name):=signal_t, date=d_tp1) %>%
      select(date, sym(target_name), everything())
    
    # price difference
    prices_diff_tmp <- prices_diff %>%
      filter(date==d_tp1)
    
    # prices to convert pnl
    prices_tmp <- prices %>%
      filter(date==d_tp1)
    
    # returns
    returns_tmp <- returns %>%
      filter(date==d_tp1)
    
    poisitions_out[[i]] <- betas_tmp
    prices_diff_out[[i]] <- prices_diff_tmp
    prices_out[[i]] <- prices_tmp
    returns_out[[i]] <- returns_tmp
  }
  
  positions_out_df <- do.call(rbind, poisitions_out) %>% as.data.table() %>% drop_na()
  prices_diff_out_df <- do.call(rbind, prices_diff_out) %>% as.data.table()
  prices_out_df <- do.call(rbind, prices_out) %>% as.data.table()
  returns_out_df <- do.call(rbind, returns_out) %>% as.data.table()
  
  # currencies pnl
  ## convert pnl to usd (assumes prices are in usdxxx and not the contrary)
  pnl_df <- (prices_diff_out_df %>% select(-date)) * 1 / (prices_out_df %>% select(-date))
  ## fix format
  pnl_df$date <- positions_out_df$date
  pnl_df <- pnl_df %>% select(date, everything())
  
  # currencies portfolio returns
  portfolio_returns_df <- returns_out_df %>% select(-date)
  ## cumulative returns
  cum_portfolio_returns_df <- cumprod(1+portfolio_returns_df) - 1
  cum_portfolio_returns_df <- cum_portfolio_returns_df * (positions_out_df %>% select(-date))
  cum_portfolio_returns_df$portfolio <- rowSums(cum_portfolio_returns_df)
  
  # currencies portfolio returns - vol adj
  portfolio_returns_df <- returns %>% select(-date)
  ## vol adjusted portfolio returns
  target_vol <- 35
  portfolio_vol <- roll_sd_excluding_zeros(portfolio_returns_df, 24) * sqrt(52) * 100
  portfolio_scaling <- target_vol / portfolio_vol
  portfolio_scaling[as.matrix(portfolio_scaling) == Inf] <- 0
  portfolio_scaling[as.matrix(portfolio_scaling) > 1000] <- 1000
  portfolio_scaling <- as.data.table(portfolio_scaling)
  vol_adj_portfolio_returns_df <- portfolio_returns_df * portfolio_scaling
  vol_adj_portfolio_returns_df <- vol_adj_portfolio_returns_df %>%
    mutate(date=positions_out_df$date) %>%
    select(date, everything())
  ## fix format
  portfolio_returns_df$date <- positions_out_df$date
  portfolio_returns_df <- portfolio_returns_df %>% select(date, everything())
  
  # currencies cumulative pnl
  cum_pnl_df <- cumsum((pnl_df %>% select(-date)))
  cum_pnl_df$date <- pnl_df$date
  cum_pnl_df <- cum_pnl_df %>% select(date, everything())
  
  browser()
  
  # currencies cumulative returns
  # cum_portfolio_returns_df <- cumsum((portfolio_returns_df %>% select(-date)))
  cum_portfolio_returns_df <- cumprod((portfolio_returns_df %>% select(-date))) - 1
  cum_portfolio_returns_df$date <- portfolio_returns_df$date
  cum_portfolio_returns_df <- cum_portfolio_returns_df %>% select(date, everything())
  
  # currencies cumulative returns - vol adj
  # cum_vol_adj_portfolio_returns_df <- cumsum((vol_adj_portfolio_returns_df %>% select(-date)))
  cum_vol_adj_portfolio_returns_df <- cumprod((1+vol_adj_portfolio_returns_df %>% select(-date))) - 1
  cum_vol_adj_portfolio_returns_df$date <- vol_adj_portfolio_returns_df$date
  cum_vol_adj_portfolio_returns_df <- cum_vol_adj_portfolio_returns_df %>% select(date, everything())
  
  output <- list(
    positions=positions_out_df,
    prices_diff=prices_diff_out_df,
    prices=prices_out_df,
    pnl=pnl_df,
    returns=portfolio_returns_df,
    cum_pnl=cum_pnl_df,
    cum_returns=cum_portfolio_returns_df,
    vol_adj_portfolio_returns=vol_adj_portfolio_returns_df,
    cum_vol_adj_portfolio_returns=cum_vol_adj_portfolio_returns_df
  )
  
  return(output)
}

run_backtest_extremes <- function(signal, prices, betas, target_name){
  
  # compute price difference
  prices_diff <- (prices %>% select(-date)) - lag(prices %>% select(-date), 1)
  prices_diff$date <- prices$date
  prices_diff <- prices_diff %>%
    drop_na() %>%
    select(date, everything())
  
  # compute returns
  returns <- log(prices %>% select(-date)) - log(lag(prices %>% select(-date), 1))
  returns$date <- prices$date
  returns <- returns %>%
    drop_na() %>%
    select(date, everything())
  
  # start backtest
  poisitions_out <- list()
  prices_diff_out <- list()
  prices_out <- list()
  returns_out <- list()
  for (i in 2:dim(signal)[1]){
    d_t <- signal$date[i]
    d_tp1 <- signal$date[i+1]
    signal_t <- signal[i][["signal"]]
    
    if (is.na(sign)){
      signal_t <- 0
    }
    
    if (signal_t > 0){
      betas_signal_t <- -1
    }else if (signal_t < 0){
      betas_signal_t <- 1
    }else{
      betas_signal_t <- 0
    }
    
    # positions
    beta_names <- colnames(betas[i,] %>% select(-date))
    betas_tmp <- betas %>%
      filter(date==d_t) %>%
      select(-date) %>%
      mutate_all(~ . * betas_signal_t) %>%
      mutate(!!paste(target_name):=signal_t, date=d_tp1) %>%
      select(date, sym(target_name), everything())
    
    # price difference
    prices_diff_tmp <- prices_diff %>%
      filter(date==d_tp1)
    
    # prices to convert pnl
    prices_tmp <- prices %>%
      filter(date==d_tp1)
    
    # returns
    returns_tmp <- returns %>%
      filter(date==d_tp1)
    
    poisitions_out[[i]] <- betas_tmp
    prices_diff_out[[i]] <- prices_diff_tmp
    prices_out[[i]] <- prices_tmp
    returns_out[[i]] <- returns_tmp
  }
  
  positions_out_df <- do.call(rbind, poisitions_out) %>% as.data.table() %>% drop_na()
  prices_diff_out_df <- do.call(rbind, prices_diff_out) %>% as.data.table()
  prices_out_df <- do.call(rbind, prices_out) %>% as.data.table()
  returns_out_df <- do.call(rbind, returns_out) %>% as.data.table()
  
  # currencies pnl
  pnl_df <- (positions_out_df %>% select(-date)) * (prices_diff_out_df %>% select(-date))
  ## convert pnl to usd (assumes prices are in usdxxx and not the contrary)
  pnl_df <- pnl_df * 1 / (prices_out_df %>% select(-date))
  ## compute portfolio pnl
  pnl_df$portfolio <- rowSums(pnl_df)
  ## fix format
  pnl_df$date <- positions_out_df$date
  pnl_df <- pnl_df %>% select(date, everything())
  
  # currencies portfolio log-returns
  portfolio_returns_df <- (positions_out_df %>% select(-date)) * (returns_out_df %>% select(-date))
  ## compute portfolio log-returns
  portfolio_returns_df$portfolio <- rowSums(portfolio_returns_df)
  ## vol adjusted portfolio log-returns
  target_vol <- 20
  portfolio_vol <- roll_sd_excluding_zeros(portfolio_returns_df, 24) * sqrt(52) * 100
  portfolio_scaling <- target_vol / portfolio_vol
  portfolio_scaling[as.matrix(portfolio_scaling) == Inf] <- 0
  portfolio_scaling[as.matrix(portfolio_scaling) > 1000] <- 1000
  portfolio_scaling <- as.data.table(portfolio_scaling)
  vol_adj_portfolio_returns_df <- portfolio_returns_df * portfolio_scaling
  vol_adj_portfolio_returns_df <- vol_adj_portfolio_returns_df %>%
    mutate(date=positions_out_df$date) %>%
    select(date, everything())
  ## fix format
  portfolio_returns_df$date <- positions_out_df$date
  portfolio_returns_df <- portfolio_returns_df %>% select(date, everything())
  
  # currencies cumulative pnl
  cum_pnl_df <- cumsum((pnl_df %>% select(-date)))
  cum_pnl_df$date <- pnl_df$date
  cum_pnl_df <- cum_pnl_df %>% select(date, everything())
  
  # currencies cumulative log-returns
  cum_portfolio_returns_df <- cumsum((portfolio_returns_df %>% select(-date)))
  cum_portfolio_returns_df$date <- portfolio_returns_df$date
  cum_portfolio_returns_df <- cum_portfolio_returns_df %>% select(date, everything())
  
  # currencies cumulative log-returns
  browser()
  cum_vol_adj_portfolio_returns_df <- cumsum((vol_adj_portfolio_returns_df %>% select(-date)))
  cum_vol_adj_portfolio_returns_df$date <- vol_adj_portfolio_returns_df$date
  cum_vol_adj_portfolio_returns_df <- cum_vol_adj_portfolio_returns_df %>% select(date, everything())
  
  output <- list(
    positions=positions_out_df,
    prices_diff=prices_diff_out_df,
    prices=prices_out_df,
    pnl=pnl_df,
    returns=portfolio_returns_df,
    cum_pnl=cum_pnl_df,
    cum_returns=cum_portfolio_returns_df,
    vol_adj_portfolio_returns=vol_adj_portfolio_returns_df,
    cum_vol_adj_portfolio_returns=cum_vol_adj_portfolio_returns_df
  )
  
  return(output)
}



