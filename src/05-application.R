# application of the empirical prediction intervals to forecasts of Nordic fertility up to 2050

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
  application_forecast = 'tmp/04-application_forecast.rds',
  empirical_PI = 'out/03-prediction_intervals_full.rds'
)
paths$output <- list(
  forecast_and_PIs = 'out/05-forecast_and_PIs.rds'
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
  # list of forecast models to validate (all but naive)
  models = c("LC", "PPS", "ARCH", "naive")
  # first year of forecast horizon
  fh_1 = 2023
})

# global functions
source(paths$input$global)


# load forecast data and empirical prediction interval data --------
forecast <- readRDS(paths$input$application_forecast)
prediction_intervals <- readRDS(paths$input$empirical_PI)

# extract empirical quantiles of error distribution
emp.quantiles <- prediction_intervals %>% 
          filter(Country != "all") %>%
          select(Country, model, model_id, model_spec, predicted_quantiles) %>%
          unnest(predicted_quantiles) %>%
          select(Country, model, model_id, model_spec, years_since_test_start, q1, q3, q6, q8) %>%
          mutate(Year = years_since_test_start + cnst$fh_1,
                 score = .[[4]][[1]][["score"]]) %>%
          select(-c(years_since_test_start, model_spec)) %>%
          rename(L95.emp = q1,
                 L80.emp = q3,
                 U80.emp = q6,
                 U95.emp = q8) %>%
          pivot_wider(id_cols = c('Country', 'model', 'score', 'Year'), names_from = 'model_id', 
                      values_from = c('L95.emp', 'L80.emp', 'U80.emp', 'U95.emp'), 
                      names_glue = '{.value}.{model_id}')

emp.quantiles.pooled <- prediction_intervals %>% 
  filter(Country == "all") %>%
  select(Country, model, model_id, model_spec, predicted_quantiles) %>%
  unnest(predicted_quantiles) %>%
  select(Country, model, model_id, model_spec, years_since_test_start, q1, q3, q6, q8) %>%
  mutate(Year = years_since_test_start + cnst$fh_1,
         score = .[[4]][[1]][["score"]]) %>%
  select(-c(years_since_test_start, model_spec)) %>%
  rename(L95.emp = q1,
         L80.emp = q3,
         U80.emp = q6,
         U95.emp = q8) %>%
  pivot_wider(id_cols = c('Country', 'model', 'score', 'Year'), names_from = 'model_id', 
              values_from = c('L95.emp', 'L80.emp', 'U80.emp', 'U95.emp'), 
              names_glue = '{.value}.{model_id}') %>%
  select(-Country)

# add pooled prediction intervals to the rest of the PI data
emp.quantiles <- left_join(emp.quantiles, emp.quantiles.pooled)

# apply empirical prediction intervals to point forecasts ----------

emp.PI <-
  forecast %>%
  filter(Year >= cnst$fh_1) %>%
  select(Country, model, Year, predicted) |>
  left_join(
    emp.quantiles
  ) %>%
  mutate(across(
    c(L95.emp.SNO, L80.emp.SNO, U95.emp.SNO, U80.emp.SNO,
      L95.emp.rawQ, L80.emp.rawQ, U95.emp.rawQ, U80.emp.rawQ,
      L95.emp.publ, L80.emp.publ, U95.emp.publ, U80.emp.publ,
      L95.emp.SNO_pooled, L80.emp.SNO_pooled, U95.emp.SNO_pooled, U80.emp.SNO_pooled,
      L95.emp.rawQ_pooled, L80.emp.rawQ_pooled, U95.emp.rawQ_pooled, U80.emp.rawQ_pooled,
      L95.emp.publ_pooled, L80.emp.publ_pooled, U95.emp.publ_pooled, U80.emp.publ_pooled),
    ~ NonconformityScore(score[1])$InverseScore(., predicted), .names = 'PI.{.col}')) %>%
    select(-c(score,
              L95.emp.SNO, L80.emp.SNO, U95.emp.SNO, U80.emp.SNO,
              L95.emp.rawQ, L80.emp.rawQ, U95.emp.rawQ, U80.emp.rawQ,
              L95.emp.publ, L80.emp.publ, U95.emp.publ, U80.emp.publ,
              L95.emp.SNO_pooled, L80.emp.SNO_pooled, U95.emp.SNO_pooled, U80.emp.SNO_pooled,
              L95.emp.rawQ_pooled, L80.emp.rawQ_pooled, U95.emp.rawQ_pooled, U80.emp.rawQ_pooled,
              L95.emp.publ_pooled, L80.emp.publ_pooled, U95.emp.publ_pooled, U80.emp.publ_pooled)) %>%
    rename(L95.emp.SNO = PI.L95.emp.SNO,
           L80.emp.SNO = PI.L80.emp.SNO,
           U95.emp.SNO = PI.U95.emp.SNO,
           U80.emp.SNO = PI.U80.emp.SNO,
           L95.emp.rawQ = PI.L95.emp.rawQ,
           L80.emp.rawQ = PI.L80.emp.rawQ,
           U95.emp.rawQ = PI.U95.emp.rawQ,
           U80.emp.rawQ = PI.U80.emp.rawQ,
           L95.emp.publ = PI.L95.emp.publ,
           L80.emp.publ = PI.L80.emp.publ,
           U95.emp.publ = PI.U95.emp.publ,
           U80.emp.publ = PI.U80.emp.publ,
           L95.emp.SNO_pooled = PI.L95.emp.SNO_pooled,
           L80.emp.SNO_pooled = PI.L80.emp.SNO_pooled,
           U95.emp.SNO_pooled = PI.U95.emp.SNO_pooled,
           U80.emp.SNO_pooled = PI.U80.emp.SNO_pooled,
           L95.emp.rawQ_pooled = PI.L95.emp.rawQ_pooled,
           L80.emp.rawQ_pooled = PI.L80.emp.rawQ_pooled,
           U95.emp.rawQ_pooled = PI.U95.emp.rawQ_pooled,
           U80.emp.rawQ_pooled = PI.U80.emp.rawQ_pooled,
           L95.emp.publ_pooled = PI.L95.emp.publ_pooled,
           L80.emp.publ_pooled = PI.L80.emp.publ_pooled,
           U95.emp.publ_pooled = PI.U95.emp.publ_pooled,
           U80.emp.publ_pooled = PI.U80.emp.publ_pooled)

# join empirical prediction interval data with forecast data -------

forecast_and_PIs <- left_join(forecast, emp.PI)


# Export -----------------------------------------------------------

saveRDS(forecast_and_PIs, paths$output$forecast_and_PIs)
