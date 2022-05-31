roll_reg_prop = function(formula,
                         data,
                         window_size,
                         do_compute=c("sigmas", "r.squareds", "1_step_forecasts")){
  date = row.names(data)
  
  rollingreg = roll_regres(formula = formula,
                           data = data,
                           width = window_size,
                           do_compute = do_compute)
  rollingreg$dates = date
  rollingreg$coefs = rollingreg$coefs %>% as.data.table() %>% mutate(date=date) %>% select(date, everything())
  
  fits = list()
  resids = list()
  for (i in 1:dim(rollingreg$coefs %>% drop_na())[1]){
    dtref = rollingreg$coefs %>% drop_na() %>% slice(i) %>% select(date)
    tmp_coef = rollingreg$coefs %>% drop_na() %>% slice(i) %>% select(-date) %>% as.numeric()
    tmp_X = data %>% filter(row.names(data) == dtref[[1]]) %>% select(-contains(TARGET)) %>% as.numeric()
    tmp_fit = t(tmp_coef) %*% tmp_X
    tmp_resid = data %>% filter(row.names(data) == dtref[[1]]) %>% select(contains(TARGET)) - tmp_fit
    
    tmp_resid$date = rownames(tmp_resid)
    tmp_resid = tmp_resid %>% select(date, everything())
    tmp_fit = as.data.table(tmp_fit)
    colnames(tmp_fit) = 'fit'
    tmp_fit$date = rownames(tmp_resid)
    tmp_fit = tmp_fit %>% select(date, everything())
    
    fits[[i]] = tmp_fit %>% as.data.table()
    resids[[i]] = tmp_resid %>% as.data.table()
  }
  
  fits_df = do.call("rbind", fits) %>%  as.data.table()
  resids_df = do.call("rbind", resids) %>%  as.data.table()
  
  resids_df[,paste0(TARGET, "_zscore")] = (resids_df$SGD - roll_mean(resids_df$SGD, width = MEAN_WINDOW_SIZE)) / roll_sd(resids_df$SGD, width = MEAN_WINDOW_SIZE)
  
  rollingreg$fits = fits_df
  rollingreg$resids = resids_df
  
  return(rollingreg)
}