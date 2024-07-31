# load_package("rollRegres")
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")


roll_reg_prop = function(formula,
                         target_name,
                         data,
                         reg_window_size,
                         mean_window_size,
                         scale_type,
                         do_compute=c("sigmas", "r.squareds", "1_step_forecasts")){
  date <- data %>% select(date)
  data <- data %>% select(-date)
  
  if (scale_type == "scale"){
    data <- data %>% lapply(scale) %>% as.data.table()
    row.names(data) <- date$date
  }
  else if (scale_type == "rolling_scale"){
    n <- dim(data)[1]
    mean_data <- apply(data, 2, function(x) {roll_mean(x, width = n, min_obs = WINDOW_SIZE)})
    sd_data <- apply(data, 2, function(x) {roll_sd(x, width = n, min_obs = WINDOW_SIZE)})
    
    data <- ((data - mean_data) / sd_data)
    row.names(data) <- date$date
    data <- data %>% drop_na()
  }
  
  rollingreg <- roll_regres(formula = formula,
                            data = data,
                            width = reg_window_size,
                            do_compute = do_compute)
  rollingreg$date <- date$date
  tmp_coefs <- rollingreg$coefs %>% as.data.table() %>%
    mutate(date=ymd(row.names(rollingreg$coefs))) %>%
    select(date, everything())
  rollingreg$coefs = tmp_coefs
  
  if (grep("+1", formula) > 0){
    data <- data %>% mutate(Intercept=1) %>% select(Intercept, everything())
  }
  
  fits = list()
  resids = list()
  for (i in 1:dim(rollingreg$coefs %>% drop_na())[1]){
    dtref <- rollingreg$coefs %>% drop_na() %>% slice(i) %>% select(date)
    tmp_coef <- rollingreg$coefs %>% drop_na() %>% slice(i) %>% select(-date) %>% as.numeric()
    tmp_X <- data %>% filter(row.names(data) == dtref[[1]]) %>% select(-contains(target_name)) %>% as.numeric()
    tmp_fit <- t(tmp_coef) %*% tmp_X
    tmp_resid <- data %>% filter(row.names(data) == dtref[[1]]) %>% select(contains(target_name)) - tmp_fit
    
    tmp_resid$date <- rownames(tmp_resid)
    tmp_resid <- tmp_resid %>% select(date, everything())
    tmp_fit <- as.data.table(tmp_fit)
    colnames(tmp_fit) <- 'fit'
    tmp_fit$date <- rownames(tmp_resid)
    tmp_fit <- tmp_fit %>% select(date, everything())
    
    fits[[i]] <- tmp_fit %>% as.data.table()
    resids[[i]] <- tmp_resid %>% as.data.table()
  }

  fits_df <- do.call("rbind", fits) %>%  as.data.table() %>% rename(!!paste0(TARGET, "_fit") := "fit") %>%
    mutate(date=ymd(date))
  
  resids_df <- do.call("rbind", resids) %>%  as.data.table() %>%
    mutate(date=ymd(date),
           !!paste0(target_name, "_roll_mean") := roll_mean(!!sym(target_name), width = mean_window_size),
           !!paste0(target_name, "_roll_sd") := roll_sd(!!sym(target_name), width = mean_window_size)) %>%
    mutate(!!paste0(target_name, "_resid_zscore") := (!!sym(paste0(target_name)) - !!sym(paste0(target_name, "_roll_mean"))) / !!sym(paste0(target_name, "_roll_sd"))) %>%
    rename(!!paste0(TARGET, "_resid") := !!paste0(TARGET)) %>% 
    select(date, !!paste0(target_name, "_resid"), !!paste0(target_name, "_resid_zscore")) %>%
    mutate(!!paste0(target_name, "_resid_zscore") := coalesce(!!sym(paste0(target_name, "_resid_zscore")), 0))
 
  rollingreg$fits <- fits_df
  rollingreg$resids <- resids_df
  
  return(rollingreg)
}