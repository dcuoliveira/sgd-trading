rm(list=ls())
library('dplyr')
library("tidyr")
library("data.table")
library("MTS")
library("MSwM")
library("lubridate")

source(file.path(getwd(), 'src', 'models', 'utils.R'))
source(file.path(getwd(), 'src', 'models', 'models.R'))

MODEL <- "dlm"
STRATEGY_TYPE <- "regime"
OUTPUT_PATH <- file.path(getwd(), 'src', 'data', 'outputs', MODEL)
TARGET <- "SGD"
SCALE_TYPE <- "rolling_scale"

# prices data
prices_df <- load_and_resample_currencies() %>% mutate(date=ymd(date)) %>% filter(date >= "2006-01-01")

# dlm output data  
dlmout <- readRDS(file = file.path(OUTPUT_PATH, paste0("model_results_", SCALE_TYPE, ".rds")))

# cointegration error
cointegration_error_df <- dlmout$residuals$res %>% mutate(ewma_vol=EWMAvol(residual,lambda = 0.8)$Sigma.t)

const = seq_along(cointegration_error_df$ewma_vol)*0+1
df <- data.frame(ewma_vol=cointegration_error_df$ewma_vol, const)
lrm <- lm(ewma_vol ~ const+0, df)
# summary(lrm)

k  <- 2 # two-regime
mv <- 3 # means of 2 variables + 1 for volatility
p1 <- 0
mv <- length(df)
rsm=msmFit(lrm, k, p=p1, sw=rep(TRUE, mv),
           control=list(parallel=FALSE))
summary(rsm)

# graphs with regime probabilities:
x11(); plotProb(rsm,which=1) # filtered/smoothed prob
x11(); plotProb(rsm,which=2) # regime1 prob & shaded area
x11(); plotProb(rsm,which=3) # regime 2 prob & shaded area

#VER SE REGIME DE MAIOR VOL É O 2. SE FOR O 1 trocar c1 com c2.

c1 <- 0.7
c2 <- 2.2
c1 <-sqrt(c1)
c2 <- sqrt(c2)

f <- rsm@Fit@filtProb
f1  <- f[,1]
f2 <- f[,2]
plot(f1, type = "l")

u1band <- 1.5*c1*f1 + 1.5*c2*(1-f1)
l1band <- -1.5*c1*f1 - 1.5*c2*(1-f1)
u2band <- 1.5*c1*(1-f2) + 1.5*c2*f2
l2band <- -1.5*c1*(1-f2) - 1.5*c2*f2

dataect_rs <- data.frame(cointegration_error_df$residual, u1band,l1band)

G1 <- ggplot(dataect_rs, aes(Data1)) +
  geom_line(aes(y=scale(dataect_rs$ect), col = "ect")) +
  geom_hline(yintercept = 0, linetype ="dashed", color = "black")+
  geom_line(aes(y=dataect_rs$u1band, col = "1.5 sd band")) +
  geom_line(aes(y=dataect_rs$l1band, col = "1.5 sd band")) +
  labs(title = "Cointegration Error", x = c("ect", "ub","lb"), y = "Standard Deviation") 
G1


# dlm betas - pode melhorar aqui colocando o filter ao invés do smooth
betas_df <- dlmout$smooth$s

# positions for the target (y) variable
positions_df <- data.frame(date=cointegration_error_df$date,
                           USDSGD=ifelse(cointegration_error_df["residual"] >= cointegration_error_df["ub"],
                                        -1,
                                        ifelse(cointegration_error_df["residual"] < cointegration_error_df["lb"],
                                               1,
                                               0)))
colnames(positions_df) <- c("date", TARGET)

# positions for the regressors (X)
positions_betas_df <- merge(x = positions_df, y = betas_df, by = "date") %>% select(-date, -sym(TARGET))

out_positions_betas_list <- list()
for (i in 1:nrow(positions_betas_df)){
  betas_row <- positions_betas_df[i,]
  coint_error_row <- positions_df[i,]
  
  if (coint_error_row[[TARGET]] > 0){
    out_positions_betas_list[[i]] <- betas_row * -1 
  }else if (coint_error_row[[TARGET]] < 0){
    out_positions_betas_list[[i]] <- betas_row * 1 
  }else{
    out_positions_betas_list[[i]] <- betas_row * 0 
  }
  
}
out_positions_betas_df <- do.call("rbind", out_positions_betas_list) %>% as.data.table()

# put all positions together
out_positions_df <- cbind(positions_df, out_positions_betas_df) %>% as.data.table()  %>% select(-intercept)

# returns data
prices_dtref <- prices_df$date
returns_df <- prices_df %>% select(-date)
returns_df <- (returns_df - lag(returns_df)) / lag(returns_df)
returns_df$date <- prices_dtref
returns_df <- returns_df %>% select(date, everything()) %>% drop_na()

# merge positions and returns 
returns_df <- merge(out_positions_df %>% select(date), returns_df, by = "date")

# check if columns are sorted correctly
returns_list = list()
for (colname in colnames(out_positions_df)){
  if (colname == "date"){
    next
  }else{
    returns_list[[colname]] <- returns_df[[colname]]
  }
}
returns_df <- do.call("cbind", returns_list) %>% as.data.table() %>% 
  mutate(date=out_positions_df$date) %>% select(date, everything())
lead_returns_df <- cbind(data.frame(date=returns_df$date[1:(dim(returns_df)[1]-1)]),
                         lead(returns_df %>% select(-date)) %>% drop_na())%>% as.data.table()

# generate stretegy returns
out_positions_df <- merge(out_positions_df, lead_returns_df %>% select(date), by = "date")
strategy_returns_df <- (out_positions_df %>% select(-date)) * (lead_returns_df %>% select(-date))

outputs <- list(signal=cointegration_error_df,
                positions=out_positions_df,
                returns=strategy_returns_df)

dir.create(file.path(OUTPUT_PATH), showWarnings = FALSE)
saveRDS(outputs, file.path(OUTPUT_PATH, paste0("backtest_", STRATEGY_TYPE, "_results.rds")))


