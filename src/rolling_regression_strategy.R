rm(list=ls())
library("rollRegres")
library('here')
library('dplyr')
library("tidyr")
library("data.table")
library("roll")
library("rlang")

source(here('src', 'utils.R'))
source(here('src', 'models.R'))
source(here('src', 'plot_funcs.R'))

MODEL <- "rolling_reg"
OUTPUT_PATH <- here("src", "data", "outputs", "models", MODEL)
WINDOW_SIZE <- 52 * 4
MEAN_WINDOW_SIZE <- 52 * 1
INTERCEPT <- TRUE
TARGET <- "SGD"

data <- merge_fx_sneer_data() data %>% filter(date >= "2006-01-01")
data <-  data %>% select(-date) %>% apply(2, function(x) scale(x)) %>% as.data.frame()

if (INTERCEPT == T){
  model_formula <- paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + "), "+1")
}else{
  model_formula <- paste(paste("SGD", "~", paste(names(data)[-grep("SGD|SNEER|date", names(data))], collapse=" + ")), " -1")
}

rollingreg <- roll_reg_prop(formula = model_formula,
                            target_name=TARGET,
                            data = data %>% select(-SNEER),
                            reg_window_size = WINDOW_SIZE,
                            mean_window_size = MEAN_WINDOW_SIZE,
                            do_compute=c("sigmas", "r.squareds", "1_step_forecasts"))

threshold <- 1.5
positions_df <- rollingreg$resids
coefs_df <- rollingreg$coefs
coef_names <- colnames(coefs_df %>% select(-date))

# target positions
positions_df <- positions_df %>% mutate(target_position := ifelse(!!sym(paste0(TARGET, "_zscore")) > threshold,
                                                                  -1,
                                                                  ifelse(!!sym(paste0(TARGET, "_zscore")) < -threshold,
                                                                         1,
                                                                         0)))

# covariates positions
merge_coefs_df <- merge(positions_df, coefs_df, by = "date")
expanded_target_positions <- replicate(length(coef_names), (merge_coefs_df %>% select(target_position)) * -1)
coefs_matrix <- merge_coefs_df %>% select(all_of(coef_names))
new_coefs_df <- -1 * coefs_matrix
new_coefs_df$date <- merge_coefs_df$date
new_coefs_df <- new_coefs_df %>% select(date, everything())

# prepare lead returns
positions_df <- merge(positions_df, new_coefs_df, by = "date")
ret_df <- data %>% select(-date, -SNEER) %>% lapply(function(x) (x/shift(x, 1)-1)) %>% as.data.table() %>% lead() %>%
  mutate(date=data$date) %>% select(date, everything())

# merge lead returns and positions
target_positions_df <- positions_df %>% rename(!!TARGET := target_position) %>% select(date, append(TARGET, coef_names))
target_ret_df <- ret_df %>% select(date, append(TARGET, coef_names)) %>% filter(date %in% target_positions_df$date)

output_df <- (target_positions_df %>% select(-date)) * (target_ret_df %>% select(-date))
output_df$date <- target_ret_df$date
output_df <- output_df %>% select(date, everything())
output_df$portfolio <- sum(output_df %>% select(-date))

cumprod_output_df <- cumprod(1+(output_df %>% select(-date))) %>% as.data.table() %>% mutate(date=output_df$date) %>%
  select(date, everything())

melt_cumprod_output_df <- melt(cumprod_output_df %>% select(-portfolio), id="date")
ggplot(melt_cumprod_output_df, aes(x=date, y=value, colour=variable, group=variable)) + geom_line()
