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

run_backtest <- function(signal, prices, betas, target_name){
  
  # generate positions
  betas_positions <- list()
  for (i in 2:dim(signal)[1]){
    dtref <- signal$date[i]
    sign <- signal[i][["signal"]]
    
    if (is.na(sign)){
      sign <- 0
    }
    
    beta_names <- colnames(betas[i,] %>% select(-date))
    betas_tmp <- betas %>%
      filter(date==dtref) %>%
      select(-date) %>%
      mutate_all(~ . * sign) %>%
      mutate(!!paste(target_name):=sign, date=dtref) %>%
      select(date, sym(target_name), everything())
    
    betas_positions[[i]] <- betas_tmp
  }
  positions_df <- do.call(rbind, betas_positions) %>% as.data.table()
  
  # computer currency returns
  returns <- (((prices %>% select(-date)) / lag(prices %>% select(-date))) - 1) %>%
    as.data.table() %>%
    drop_na() %>%
    mutate(date=prices$date[2:length(prices$date)[1]]) %>%
    select(date, everything())
  
  # lead returns
  l <- 1
  lead_returns <- returns %>%
    shift(l, n=-l) %>%
    as.data.table() %>%
    setnames(colnames(returns)) %>%
    mutate(date=returns$date) %>%
    slice(1:(nrow(.)-l))
  
  # portfolio returns
  start_date <-  min(min(lead_returns$date), min(positions_df$date))
  end_date <- min(max(lead_returns$date), max(positions_df$date))
  positions_df <- positions_df %>% filter(date>=start_date&date<=end_date)
  lead_returns <- lead_returns %>% filter(date>=start_date&date<=end_date)
  portfolio_returns_df <- (positions_df %>% select(-date)) * (lead_returns %>% select(-date))
  portfolio_returns_df$date <- positions_df$date
  portfolio_returns_df <- portfolio_returns_df %>%  select(date, everything())
  portfolio_returns_df$portfolio <- rowSums(portfolio_returns_df %>% select(-date))
  
  output <- list(portfolio_returns=portfolio_returns_df,
                 positions=positions_df)
  return(output)
}


