trade_strategy <- function(ect, k){
  
  ect_returns <- diff(lag(ect))
  portfolio_value <- numeric(length(ect))
  portfolio_value[1] <- 1  
  position_open <- FALSE
  position_type <- NA
  for (i in 3:length(ect)) {

    thresh <- sd(ect[1:i - 1]) * k

    if (!position_open) {
      if (ect[i - 1] < -thresh) {
        position_open <- TRUE
        position_type <- "buy"
      } else if (ect[i - 1] > thresh) {
        
        position_open <- TRUE
        position_type <- "sell"
      }
    }
    if (position_open) {
      if (position_type == "buy") {
        portfolio_value[i] <- portfolio_value[i - 1] + ect_returns[i]
        if (ect[i-1] >= 0) {  
          position_open <- FALSE
          position_type <- NA
        }
      } else if (position_type == "sell") {
        portfolio_value[i] <- portfolio_value[i - 1] - ect_returns[i]
        if (ect[i-1] <= 0) {  
          position_open <- FALSE
          position_type <- NA
        }
      }
    } else {
      portfolio_value[i] <- portfolio_value[i - 1]
    }
  }
  return(portfolio_value)
}

trade_strategy_new <- function(ect, betas, prices, k){
  
  if (dim(ect)[1] != dim(betas)[1]){
    stop("ect and betas must be the same length")
  }
  
  positions <- list()
  pnl <- list()
  returns <- list()
  residuals <- list()
  position_open <- FALSE
  position_type <- NA
  
  dtref <- ect$date[2]
  positions[[2]] <- betas_df %>%
    select(-date) %>%
    mutate_all(~ . * 0) %>%
    mutate(date=betas_df$date) %>%
    filter(date==dtref) %>%
    mutate(SGD=0) %>%
    select(date, SGD, everything())
  
  count = 0
  for (i in 3:dim(ect)[1]) {
    dtref <- ect$date[i]
    thresh <- sd(ect[["residual"]]) * k
    residuals[[i]] <- data.table(date=dtref, residual=ect[i-1][["residual"]], ub=thresh, lb=-thresh)
    
    if (!position_open) {
      if (ect[i - 1][["residual"]] < -thresh) {
        position_open <- TRUE
        position_type <- "buy"
      } else if (ect[i - 1][["residual"]] > thresh) {
        position_open <- TRUE
        position_type <- "sell"
      }
    }
    if (position_open) {
      if (position_type == "buy") {
        positions[[i]] <- betas_df %>%
          select(-date) %>%
          mutate_all(~ . * 1) %>%
          mutate(date=betas_df$date) %>%
          filter(date==dtref) %>%
          mutate(SGD=1) %>%
          select(date, SGD, everything())
        
        pnl[[i]] <- (positions[[i]] %>% select(-date)) * (prices %>% filter(date==positions[[i]]$date) %>% select(-date))
        pnl[[i]] <- pnl[[i]] %>% mutate(date=positions[[i]]$date) %>% select(date, everything())
        
        returns_dtref <- prices %>% filter(date>=dtref) %>% slice(2) %>% select(date)
        returns_ip1 <- prices %>% filter(date>=dtref) %>% select(-date) %>% slice(1:2) %>% as.matrix() %>% log() %>% diff()
        returns[[i]] <- (positions[[i]] %>% select(-date)) * returns_ip1
        returns[[i]] <- returns[[i]] %>% mutate(date=returns_dtref$date) %>% select(date, everything())
        if (ect[i-1][["residual"]] >= 0) {  
          position_open <- FALSE
          position_type <- NA
          positions[[i]] <- betas_df %>%
            select(-date) %>%
            mutate_all(~ . * 0) %>%
            mutate(date=betas_df$date) %>%
            filter(date==dtref) %>%
            mutate(SGD=0) %>%
            select(date, SGD, everything())
          
          pnl[[i]] <- (positions[[i]] %>% select(-date)) * (prices %>% filter(date==positions[[i]]$date) %>% select(-date))
          pnl[[i]] <- pnl[[i]] %>% mutate(date=positions[[i]]$date) %>% select(date, everything())
          
          returns[[i]] <- (positions[[i]] %>% select(-date)) * returns_ip1
          returns[[i]] <- returns[[i]] %>% mutate(date=returns_dtref$date) %>% select(date, everything())
        }
      } else if (position_type == "sell") {
        positions[[i]] <- betas_df %>%
          select(-date) %>%
          mutate_all(~ . * -1) %>%
          mutate(date=betas_df$date) %>%
          filter(date==dtref) %>%
          mutate(SGD=-1) %>%
          select(date, SGD, everything())
        
        pnl[[i]] <- (positions[[i]] %>% select(-date)) * (prices %>% filter(date==positions[[i]]$date) %>% select(-date))
        pnl[[i]] <- pnl[[i]] %>% mutate(date=positions[[i]]$date) %>% select(date, everything())
        
        returns_dtref <- prices %>% filter(date>=dtref) %>% slice(2) %>% select(date)
        returns_ip1 <- prices %>% filter(date>=dtref) %>% select(-date) %>% slice(1:2) %>% as.matrix() %>% log() %>% diff()
        returns[[i]] <- (positions[[i]] %>% select(-date)) * returns_ip1
        returns[[i]] <- returns[[i]] %>% mutate(date=returns_dtref$date) %>% select(date, everything())
        if (ect[i-1][["residual"]] <= 0) {  
          position_open <- FALSE
          position_type <- NA
          positions[[i]] <- betas_df %>%
            select(-date) %>%
            mutate_all(~ . * 0) %>%
            mutate(date=betas_df$date) %>%
            filter(date==dtref) %>%
            mutate(SGD=0) %>%
            select(date, SGD, everything())
          
          pnl[[i]] <- (positions[[i]] %>% select(-date)) * (prices %>% filter(date==positions[[i]]$date) %>% select(-date))
          pnl[[i]] <- pnl[[i]] %>% mutate(date=positions[[i]]$date) %>% select(date, everything())
          
          returns[[i]] <- (positions[[i]] %>% select(-date)) * returns_ip1
          returns[[i]] <- returns[[i]] %>% mutate(date=returns_dtref$date) %>% select(date, everything())
        }
      }
    } else {
      
      positions[[i]] <- positions[[i-1]] %>% mutate(date=dtref)
      
      pnl[[i]] <- (positions[[i]] %>% select(-date)) * (prices %>% filter(date==positions[[i]]$date) %>% select(-date))
      pnl[[i]] <- pnl[[i]] %>% mutate(date=positions[[i]]$date) %>% select(date, everything())
      
      returns_dtref <- prices %>% filter(date>=dtref) %>% slice(2) %>% select(date)
      returns_ip1 <- prices %>% filter(date>=dtref) %>% select(-date) %>% slice(1:2) %>% as.matrix() %>% log() %>% diff()
      returns[[i]] <- (positions[[i]] %>% select(-date)) * returns_ip1
      returns[[i]] <- returns[[i]] %>% mutate(date=returns_dtref$date) %>% select(date, everything())
    }
  }
  out <- list(positions=do.call(rbind, positions) %>% as.data.table() %>% mutate(date=ymd(date)),
              residuals=do.call(rbind, residuals) %>% as.data.table() %>% mutate(date=ymd(date)),
              pnl=do.call(rbind, pnl) %>% as.data.table() %>% mutate(date=ymd(date)),
              returns=do.call(rbind, returns) %>% as.data.table() %>% mutate(date=ymd(date)))
  return(out)
}


        