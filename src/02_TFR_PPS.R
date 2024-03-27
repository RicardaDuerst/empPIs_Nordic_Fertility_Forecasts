# reading in cv forecasts in the calibration series by Julia Hellstrand 
# we call the model PPS for postponement scenario

# Init ------------------------------------------------------------

library(dplyr)
library(tidyr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  Julia_TFR_forecast = 'tmp/quant_PI_years.xlsx'
)
paths$output <- list(
  PPS_TFR_forecast = 'tmp/02-PPS_TFR_forecast.rds'
)

# constants specific to this analysis
cnst <- within(list(), {
  quantiles = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
  # how many threads used to fit models?
  cpu_nodes = 3
  # cross-validation series
  starting_year = list("FIN" = 1969, "SWE" = 1921, "NOR" = 1997, "DEN" = 1946) # first year of first cv_series
  cv_series_calibration = list("FIN" = 1:20, "SWE" = 1:68, "NOR" = 1:7, "DEN" = 1:43)
  cv_series_validation = list("FIN" = 21:25, "SWE" = 69:73, "NOR" = 8:12, "DEN" = 44:48) # last 5 series
  colors = list(
    cv_series = c(test = '#f51883', calibration = '#117396')
  )
  # list of countries
  cntr = c("FIN", "SWE", "NOR", "DEN")
  # age vector
  ages <- 12:55
  # forecast length
  test_length = data.frame(Country = c("FIN", "SWE", "NOR", "DEN"),
                           length = c(30, 30, 15, 30))
})


# global functions
source(paths$input$global)


# Load cross-validation forecast data by Julia --------------------------------------
Julia_input <- readxl::read_xlsx(paths$input$Julia_TFR_forecast)

# bring into needed format (same as for the other forecast models)
PPS_tfr <- Julia_input %>%
             rename(predicted = q_0.50,
                    Year = forecast_year,
                    L95.model = q_0.025,
                    L90.model = q_0.05,
                    L80.model = q_0.10,
                    L50.model = q_0.25,
                    U50.model = q_0.75,
                    U80.model = q_0.90,
                    U90.model = q_0.95, 
                    U95.model = q_0.975) %>%
             mutate(model = "PPS",
                    cv_sample = "test",
                    Country = case_when(country == "Finland" ~ "FIN",
                                        country == "Denmark" ~ "DEN",
                                        country == "Sweden" ~ "SWE",
                                        country == "Norway" ~ "NOR"),
                    cv_id = case_when(country == "Finland" ~ jump_off_year - cnst$starting_year$FIN+2,
                                      country == "Denmark" ~ jump_off_year - cnst$starting_year$DEN+2,
                                      country == "Sweden" ~ jump_off_year - cnst$starting_year$SWE+2,
                                      country == "Norway" ~ jump_off_year - cnst$starting_year$NOR+2)) %>%
             dplyr::select(-c(country, jump_off_year))


# Export Julia's forecast TFR ------------------------------------------------------
saveRDS(PPS_tfr, paths$output$PPS_TFR_forecast)
