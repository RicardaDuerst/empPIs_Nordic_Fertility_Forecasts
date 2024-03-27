# fitting and forecasting a Lee-Carter model for the age-specific fertility data in the calibration series

# Init ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(StMoMo)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  asfr_cv = 'tmp/01-asfr_cv.csv',
  tfr_cv = 'tmp/01-tfr_cv.csv',
  births_exposure = 'tmp/01-B_E_cv.csv'
)
paths$output <- list(
  LC_TFR_forecast = 'tmp/02-Lee_Carter_TFR_forecast.rds'
)

# constants specific to this analysis
cnst <- within(list(), {
  quantiles = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
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
  # parameters for LC
  nsim = 250
  seed = 1987
  # age vector
  ages <- 12:55
})


# global functions
source(paths$input$global)


# Load cross-validation series of Births and Exposures ---------------
B_E_cv <- read.csv(paths$input$births_exposure)


# create stmomo objects ----------------------------------------------

stmomo <- list()

for (i in 1:length(cnst$cntr)) { 
  
  # filter for country and select training series
  B_E_cv_1 <- filter(B_E_cv, Country == cnst$cntr[i] & cv_sample == "training")
  id <- unique(B_E_cv_1$cv_id)
  
  stmomo[[i]] <- vector(mode = "list",
                        length = length(id))
  
  for (j in 1:length(id)) {
    
    # filter for cv series
    B_E_cv_2 <- filter(B_E_cv_1, cv_id == id[j])
    
    # create stmomo object
    stmomo[[i]][[j]] <- Pop2StMoMo(
      B_E_cv_2,
      name_births = 'Births',
      name_exposure = 'Exposure',
      age = cnst$ages,
      years = unique(B_E_cv_2$Year),
      sex = 'female',
      label = cnst$cntr[[i]]
    )
  }
}

# fit and forecast Lee-carter models ----------------------------------------------

fit <- vector(mode = "list",
              length = length(cnst$cntr))
LC_forecast_sim <- vector(mode = "list",
                          length = length(cnst$cntr))

for (i in 1:length(cnst$cntr)) { 
  
  # filter for country and select training series
  B_E_cv_1 <- filter(B_E_cv, Country == cnst$cntr[i] & cv_sample == "training")
  id <- unique(B_E_cv_1$cv_id)

  fit[[i]] <- vector(mode = "list",
                   length = length(id))
  LC_forecast_sim[[i]] <- vector(mode = "list",
                                 length = length(id))
  
  for (j in 1:length(id)) {
    
    # fit and forecast Lee-Carter model
    fit[[i]][[j]] <- ForecastLeeCarter(
      stmomo[[i]][[j]],
      h = cnst$test_length[i,2],
      betas = 'np',
      nsim = cnst$nsim,
      seed = cnst$seed
    )
    
    # calculate forecast TFR over simulations
    LC_forecast_sim[[i]][[j]] <- as.data.frame.table(apply(fit[[i]][[j]]$forecast_sim, 2:3, function (asfr) sum(asfr, na.rm = T))) %>%
      mutate(Country = cnst$cntr[i], model = "LC") %>%
      rename(Year = Var1,
             sim = Var2,
             TFR = Freq)
    
  }
}

names(stmomo) <- cnst$cntr
names(fit) <- cnst$cntr
names(LC_forecast_sim) <- cnst$cntr


# calculate quantiles and to receive prediction intervals and median forecast
  
LC_tfr <- vector(mode = "list",
                 length = length(cnst$cntr))

for (i in 1:length(cnst$cntr)) {
  id <- length(LC_forecast_sim[[i]])
  
  LC_tfr[[i]] <- vector(mode = "list",
                        length = id)
  
  for (j in 1:id) {
    year <- cnst$test_length[i,2]
    
    LC_tfr[[i]][[j]] <- vector(mode = "list",
                          length = year)
    for (k in 1:year){
      
       a <- LC_forecast_sim[[i]][[j]] %>% filter(Year == LC_forecast_sim[[i]][[j]]$Year[k])
      
        LC_tfr[[i]][[j]][[k]] <- quantile(a$TFR, probs = cnst$quantiles) %>%
            t() %>% as.data.frame() %>%
            rename(L95.model = '2.5%',
                   L90.model = '5%',
                   L80.model = '10%',
                   L50.model = '25%',
                   predicted = '50%',
                   U50.model = '75%',
                   U80.model = '90%',
                   U90.model = '95%', 
                   U95.model = '97.5%') %>%
            mutate(model = "LC",
                   Year = as.numeric(as.character(LC_forecast_sim[[i]][[j]]$Year[k])),
                   Country = cnst$cntr[i],
                   cv_id = j,
                   cv_sample = "test")
    }
    LC_tfr[[i]][[j]] <- do.call(rbind, LC_tfr[[i]][[j]])
  }
  LC_tfr[[i]] <- do.call(rbind, LC_tfr[[i]])  
}
LC_tfr <- do.call(rbind, LC_tfr) 


# Export LC forecast TFR ------------------------------------------------------
saveRDS(LC_tfr, paths$output$LC_TFR_forecast)
