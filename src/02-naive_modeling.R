# Predict TFR over cv series using naive forecasts (fixing last observed TFR) in the calibration series

# Init ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  tfr_cv = 'tmp/01-tfr_cv.csv'
)
paths$output <- list(
  fig = 'out',
  Naive_TFR_forecast = 'tmp/02-Naive_TFR_forecast.rds'
)

# constants specific to this analysis
cnst <- within(list(), {
  cv_series_calibration = list("FIN" = 1:20, "SWE" = 1:68, "NOR" = 1:7, "DEN" = 1:43)
  cv_series_validation = list("FIN" = 21:25, "SWE" = 69:73, "NOR" = 8:12, "DEN" = 44:48) # last 5 series
  cv_series_application = list("FIN" = 25, "SWE" = 73, "NOR" = 12, "DEN" = 48) # do we need this?
  colors = list(
    cv_series = c(test = '#f51883', calibration = '#117396')
  )
  # list of countries
  cntr = c("FIN", "SWE", "NOR", "DEN")
})

# global functions
source(paths$input$global)


# Load cross validation series ------------------------------------

tfr_cv <- read.csv(paths$input$tfr_cv)


# Predict TFR over cv series using naive forecasts (fixing last observed TFR) -------------

naive_tfr <- list()

for (i in 1:length(cnst$cntr)) {
  tfr_cv_1 <- filter(tfr_cv, Country == cnst$cntr[i])

  id <- unique(tfr_cv_1$cv_id)

  naive_tfr_id <- list()

  for (j in 1:length(id)) {
    tfr_cv_2 <- filter(tfr_cv_1, cv_id == id[j])

    naive_tfr_id[[j]] <- tfr_cv_2 %>%
      mutate(predicted = if_else(cv_sample == "test", .$TFR[.$years_since_test_start==-1], NA_real_)) %>%
      select(-c(TFR, origin_year, origin_year_test, series_year, years_since_test_start))
    }
  naive_tfr_id <- do.call(rbind, naive_tfr_id)
  naive_tfr[[i]] <- naive_tfr_id
}

# bind country data sets together
naive_tfr <- do.call(rbind, naive_tfr)

# add model variable
naive_tfr <- naive_tfr %>% mutate(model = "naive")


# Export naive forecast TFR ------------------------------------------------------

saveRDS(naive_tfr, paths$output$Naive_TFR_forecast)
