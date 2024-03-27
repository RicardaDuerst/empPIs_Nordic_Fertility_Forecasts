# Visualizations for Lab Talk December 2023

# Init ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  asfr = 'tmp/00-ASFR.csv',
  tfr = 'tmp/00-TFR.csv',
  asfr_cv = 'tmp/01-asfr_cv.csv',
  tfr_cv = 'tmp/01-tfr_cv.csv',
  tfr_forecast = 'out/02-tfr_forecast.rds',
  prediction_intervals = 'out/02-prediction_intervals.rds'
)
paths$output <- list(
  fig = 'out'
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
})

# global functions
source(paths$input$global)

# list containers for analysis artifacts
fig <- list()


# Nordic Fertility Decline -------------------------------------------

# load TFR data
tfr <- fread(paths$input$tfr)

# plot TFR time series

fig$tfr <- tfr %>%
  ggplot() +
  aes(x = Year, y = TFR, color = Country) +
  geom_line(linewidth = 1) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_label_repel(data = filter(tfr, Year == 2010),
            aes(label = round(TFR, 2), fill = Country),
            color = 'white',
            nudge_y = 0.4,
            size = 4) + 
  geom_label_repel(data = filter(tfr, Year == 2022),
                 aes(label = round(TFR, 2), fill = Country),
                 color = 'white',
                 nudge_x = 5,
                 size = 4)

fig$tfr

# Empirical prediction intervals from cross-validation --------

# load TFR cv data with naive forecast
tfr_naive <- readRDS(paths$input$tfr_forecast)

# plot a few cv series
fig$cv_series_naive <- tfr_naive %>% filter(Country == "FIN" & cv_id %in% c(1, 10, 20)) %>%
  ggplot() +
  aes(x = Year, y = TFR, color = cv_sample) +
  geom_point(size = 0.5) +
  geom_line(aes(x = Year, y = predicted_naive), linewidth = 0.5) + # naive forecast
  facet_wrap(~ cv_id , ncol = 1) +
  labs(title = "Finland") +
  figspec$MyGGplotTheme(axis = 'xy')

fig$cv_series_naive
  

# load naive forecast errors
tfr_naive_PI <- readRDS(paths$input$prediction_intervals)

tfr_naive_PI <-
  tfr_naive_PI %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_calibration[["FIN"]], Country == "FIN") %>% 
  filter(model_id %in% c('SNO')) %>%
  mutate(model_id = factor(model_id, names(tfr_naive_PI$model_id_labs)))

# plot error distribution
fig$rel_error_naive <-
  tfr_naive_PI %>%
  ggplot() +
  aes(x = years_since_test_start, y = score) +
  geom_ribbon(
    aes(x = years_since_test_start, ymin = q1, ymax = q4),
    color = NA, fill = 'grey70', alpha=0.5
  ) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_point(color = '#f8766d', size = 0.5) + 
  scale_y_continuous() +
  scale_color_manual(
    values = cnst$colors$cv_series
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'log(observed/predicted)', x = 'Years into forecasting period',
       title = "Finland, relative errors in validation series")
fig$rel_error_naive
