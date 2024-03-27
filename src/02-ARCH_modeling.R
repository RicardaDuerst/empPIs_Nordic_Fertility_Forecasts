# fitting and forecasting an ARCH model for the TFR data in the calibration series
# following the tutorial at https://rpubs.com/cyobero/arch

# Init ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(dynlm)
library(FinTS)
library(rugarch)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  asfr_cv = 'tmp/01-asfr_cv.csv',
  tfr_cv = 'tmp/01-tfr_cv.csv',
  tfr = 'tmp/00-TFR.csv'
)
paths$output <- list(
  fig = 'out',
  log = 'tmp/log.txt',
  prediction_intervals = 'out/02-ARCH_modeling.rds'
)

# constants specific to this analysis
cnst <- within(list(), {
  quantiles = c(0.05, 0.1, 0.9, 0.95)
  # how many threads used to fit models?
  cpu_nodes = 3
  cv_series_calibration = list("FIN" = 1:20, "SWE" = 1:68, "NOR" = 1:7, "DEN" = 1:43)
  cv_series_validation = list("FIN" = 21:25, "SWE" = 69:73, "NOR" = 8:12, "DEN" = 44:48) # last 5 series
  cv_series_application = list("FIN" = 25, "SWE" = 73, "NOR" = 12, "DEN" = 48) # do we need this?
  colors = list(
    cv_series = c(test = '#f51883', calibration = '#117396')
  )
  # list of countries
  cntr = c("FIN", "SWE", "NOR", "DEN")
  # forecast length
  test_length = data.frame(Country = c("FIN", "SWE", "NOR", "DEN"),
                           length = c(30, 30, 15, 30))
})


# global functions
source(paths$input$global)


# Load time series of TFR -----------------------------------------

tfr <- fread(paths$input$tfr)

# plot time series for Finland
tfr %>% filter(Country == "FIN") %>% 
  ggplot(aes(x = Year, y = TFR)) +
  geom_line() +
  labs(title = "Finland")


# Testing for ARCH effects ----------------------------------------
ARCH_test <- list()

for (i in 1:length(cnst$cntr)) {
  
  # filter for country
  tfr_cntr <- filter(tfr, Country == cnst$cntr[i])
  
  # Step 1: Estimate mean equation tfr = beta + error
  mean <- dynlm(TFR ~ 1, data = tfr_cntr)
  
  # Step 2: Retrieve the residuals from the former model and square them
  ehatsq <- ts(resid(mean)^2)
  
  # Step 3: regress squared residuals on one-lagged squared residuals
  arch <- dynlm(ehatsq ~ L(ehatsq), data = ehatsq)
  
  # Step 4: ARCH test
  archTest <- ArchTest(tfr_cntr$TFR, lags = 1, demean = TRUE)
  ARCH_test[[i]] <- archTest
}
names(ARCH_test) <- cnst$cntr

# Because the p-value is < 0.05, we reject the null hypothesis
# and conclude the presence of ARCH(1) effects in the full time series for all three countries.


# Load cross validation series of TFR ------------------------------

tfr_cv <- read.csv(paths$input$tfr_cv)


# # example fit
# tfr_example <- tfr_cv %>% filter(Country == "FIN" & cv_id == 1 & cv_sample == "training")
# 
# example_spec <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
#                            mean.model=list(armaOrder=c(0,0)),distribution.model="norm")
# 
# example_fit <- ugarchfit(data = tfr_example$TFR, spec = example_spec)
# example_AIC <- infocriteria(example_fit)[1]


# Set up automatic routine to find best GARCH order and ARMA order for best fitting GARCH model
# to forecast TFR for each country and each cv series (see https://rpubs.com/Sharique16/garch)
# selection based on smallest AIC

model_orders <- data.frame(GARCH_a = c(1,1,2,2),
                           GARCH_b = c(1,2,0,1),
                           ARMA_a = c(1,1,1,1),
                           ARMA_b = c(1,1,1,1),
                           AIC = NA)

GARCH_spec <- vector(mode = "list",
                     length = length(cnst$cntr))

for (i in 1:length(cnst$cntr)) {
  
  # filter for country and select training series
  tfr_cv_1 <- filter(tfr_cv, Country == cnst$cntr[i] & cv_sample == "training")
  id <- unique(tfr_cv_1$cv_id)
  
  GARCH_spec[[i]] <- vector(mode = "list",
                       length = length(id))
  
  for (j in 1:length(id)) {
    
    # filter for cv series
    tfr_cv_2 <- filter(tfr_cv_1, cv_id == id[j])
    
    for (k in 1:nrow(model_orders)) {
      tryCatch({
      # model specification
      spec <- ugarchspec(variance.model = list(model="sGARCH",
                                               garchOrder = c(model_orders[k,1], model_orders[k,2])),
                         mean.model = list(armaOrder=c(model_orders[k,3],model_orders[k,4])),
                         distribution.model="norm")
      
      # fit the model
      fit <- ugarchfit(data = tfr_cv_2$TFR, spec = spec)
      
      # save the AIC value
      model_orders[k,5] <- infocriteria(fit)[1]
      
      }, error=function(e){})
    }
    
    # select model specification with lowest AIC value
    best_fit <- model_orders %>% filter(AIC == min(AIC, na.rm = T))
    
    # save model spec in list
    GARCH_spec[[i]][[j]] <- best_fit
  }
  names(GARCH_spec[[i]]) <- id
}
names(GARCH_spec) <- cnst$cntr


# fit and forecast the best fitting model ----------------------------------
# this should be included in the loop before, but well...

ARCH_forecast <- vector(mode = "list",
                   length = length(cnst$cntr))

for (i in 1:length(cnst$cntr)) {
  
  # filter for country and select training series
  tfr_cv_1 <- filter(tfr_cv, Country == cnst$cntr[i] & cv_sample == "training")
  id <- unique(tfr_cv_1$cv_id)
  
  ARCH_forecast[[i]] <- vector(mode = "list",
                          length = length(id))
  
  for (j in 1:length(id)) {
    
    # filter for cv series
    tfr_cv_2 <- filter(tfr_cv_1, cv_id == id[j])
    
    # fit the model
    spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                             garchOrder = c(GARCH_spec[[i]][[j]][,1], GARCH_spec[[i]][[j]][,2])),
                       mean.model = list(armaOrder = c(GARCH_spec[[i]][[j]][,3],GARCH_spec[[i]][[j]][,4])),
                       distribution.model = "norm")
    fit <- ugarchfit(data = tfr_cv_2$TFR, spec = spec)
    
    # forecast
    ARCH_forecast[[i]][[j]] <- ugarchforecast(fit, n.ahead = cnst$test_length[i,2])
    
    # extract the forecast values
    ARCH_forecast[[i]][[j]] <- ARCH_forecast[[i]][[j]]@forecast[["seriesFor"]][1:cnst$test_length[i,2]]
  }
}
names(ARCH_forecast) <- cnst$cntr

