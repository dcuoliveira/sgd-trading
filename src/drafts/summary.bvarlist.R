summary.bvarlist <- function(object, ...){
  browser()
  n_models <- length(object)
  
  teststats <- data.frame(p = rep(NA, n_models),
                          s = rep(NA, n_models),
                          r = rep(NA, n_models),
                          LL = rep(NA, n_models),
                          AIC = rep(NA, n_models),
                          BIC = rep(NA, n_models),
                          HQ = rep(NA, n_models),
                          stringsAsFactors = FALSE)
  
  endo_vars <- NULL
  ect_vars <- NULL
  exog_vars <- NULL
  
  for (i in 1:n_models) {
    
    if (!is.null(object[[i]][["error"]])) {
      if (object[[i]][["error"]]) {
        next
      }
    }
    
    tvp <- any(unlist(object[[i]][["specifications"]][["tvp"]]))
    sv <- object[[i]][["specifications"]][["tvp"]][["Sigma"]]
    tt <- NROW(object[[i]][["y"]])
    k <- object[[i]][["specifications"]][["dims"]][["K"]]
    p <- object[[i]][["specifications"]][["lags"]][["p"]]
    endo_vars <- c(endo_vars, dimnames(object[[i]][["y"]])[[2]])
    
    teststats[i, "p"] <- p
    teststats[i, "s"] <- object[[i]][["specifications"]][["lags"]]["s"]
    
    if (tvp) {
      temp_pars <- list()
      length(temp_pars) <- tt
    } else {
      temp_pars <- NULL
    }
    x <- NULL
    
    if ("bvar" %in% class(object[[i]])) {
      type <- "VAR"
      x <- t(object[[i]][["x"]])
      tot_pars <- NCOL(object[[i]][["x"]])
      exog_vars <- c(exog_vars, dimnames(object[[i]][["x"]])[[2]])
      
      vars <- c("A", "B", "C")
      for (j in vars) {
        if (!is.null(object[[i]][[j]])) {
          if (is.list(object[[i]][[j]])) {
            for (period in 1:tt) {
              temp_pars[[period]] <- cbind(temp_pars[[period]], object[[i]][[j]][[period]]) 
            }
          } else {
            temp_pars <- cbind(temp_pars, object[[i]][[j]]) 
          }
        }
      }
    }
    
    if ("bvec" %in% class(object[[i]])) {
      
      type <- "VEC"
      r <- 0
      if (is.null(object[[i]][["specifications"]][["rank"]])) {
        if (!is.null(object[[i]][["alpha"]])) {
          if (tvp) {
            r <- NCOL(object[[i]][["alpha"]][[1]]) / k
          } else {
            r <- NCOL(object[[i]][["alpha"]]) / k 
          }
        }
      } else {
        r <- object[[i]][["specifications"]][["rank"]]
      }
      teststats[i, "r"] <- r
      tot_pars <- r
      
      vars <- c("Pi", "Pi_x", "Pi_d", "Gamma", "Upsilon", "C")
      for (j in vars) {
        if (!is.null(object[[i]][[j]])) {
          
          if (is.list(object[[i]][[j]])) {
            for (period in 1:tt) {
              temp_pars[[period]] <- cbind(temp_pars[[period]], object[[i]][[j]][[period]]) 
            }
          } else {
            temp_pars <- cbind(temp_pars, object[[i]][[j]]) 
          }
          
          if (j == "Pi") {
            x <- cbind(x, object[[i]][["w"]])
            ect_vars <- c(ect_vars, dimnames(object[[i]][["w"]])[[2]])
          }
          if (j == "Pi_x") {
            x <- cbind(x, object[[i]][["w_x"]])
            ect_vars <- c(ect_vars, dimnames(object[[i]][["w_x"]])[[2]])
          }
          if (j == "Pi_d") {
            x <- cbind(x, object[[i]][["w_d"]])
            ect_vars <- c(ect_vars, dimnames(object[[i]][["w_d"]])[[2]])
          }
          if (j == "Gamma") {
            x <- cbind(x, object[[i]][["x"]])
            exog_vars <- c(exog_vars, dimnames(object[[i]][["x"]])[[2]])
          }
          if (j == "Upsilon") {
            x <- cbind(x, object[[i]][["x_x"]])
            exog_vars <- c(exog_vars, dimnames(object[[i]][["x_x"]])[[2]])
          }
          if (j == "C") {
            x <- cbind(x, object[[i]][["x_d"]])
            exog_vars <- c(exog_vars, dimnames(object[[i]][["x_d"]])[[2]])
          }
        }        
      }
      
      tot_pars <- tot_pars + NCOL(x)
      x <- t(x)
    }
    
    if (tvp) {
      draws <- nrow(temp_pars[[1]])
    } else {
      draws <- nrow(temp_pars) 
    }
    LL <- matrix(NA, tt, draws) # Get LogLik
    y <- t(object[[i]][["y"]])
    u <- y * 0
    if (sv) {
      sigma <- matrix(NA_real_, k * tt, k)
    } else {
      sigma <- matrix(NA_real_, k, k)
    }
    
    for (j in 1:draws) {
      # Residuals
      if (tvp) {
        for (period in 1:tt) {
          u[, period] <- y[, period] - matrix(temp_pars[[period]][j, ], k) %*% x[, period] 
        }
      } else {
        u <- y - matrix(temp_pars[j, ], k) %*% x
      }
      
      if (sv) {
        for (period in 1:tt) {
          sigma[(period - 1) * k + 1:k,] <- matrix(object[[i]][["Sigma"]][[period]][j,], k)
        }
      } else {
        sigma <- matrix(object[[i]][["Sigma"]][j,], k)
      }
      
      # LogLik
      LL[, j] <- loglik_normal(u, sigma)
    }
    
    ll <- sum(rowMeans(LL))
    teststats[i, "LL"] <- ll
    teststats[i, "AIC"] <- 2 * tot_pars - 2 * ll
    teststats[i, "BIC"] <- log(tt) * tot_pars - 2 * ll
    teststats[i, "HQ"] <- 2 * log(log(tt)) * tot_pars - 2 * ll
  }
  
  endo_vars <- unique(endo_vars)
  if (!is.null(ect_vars)) {
    ect_vars <- unique(ect_vars)
  }
  if (!is.null(exog_vars)) {
    exog_vars <- unique(exog_vars) 
  }
  
  # Omit unnecessary columns
  teststats <- teststats[, which(!apply(teststats, 2, function(x) {all(is.na(x))}))]
  
  return(teststats)
}
