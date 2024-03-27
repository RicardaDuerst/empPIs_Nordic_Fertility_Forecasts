# Validate the prediction intervals using validation data series

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
  prediction_intervals = 'out/02-prediction_intervals.rds',
  prediction_intervals_pooled = 'out/02-prediction_intervals_pooled.rds'
)
paths$output <- list(
  calibration_table = 'out/03-calibration_table.rds',
  validation_plots = 'out/03-validation_plots.rds',
  fig = 'out',
  prediction_intervals_full = 'out/03-prediction_intervals_full.rds'
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
  models = c("LC", "PPS")
})

# global functions
source(paths$input$global)


# Load prediction intervals ---------------------------------------

prediction_intervals_single <- readRDS(paths$input$prediction_intervals)
prediction_intervals_pooled <- readRDS(paths$input$prediction_intervals_pooled) %>% mutate(Country = "all")


# Calculate calibration scores ------------------------------------
calibration <- list()

for (i in 1:length(cnst$cntr)) {
  calibration[[i]] <-
    prediction_intervals_single %>%
    dplyr::select(-predicted_quantiles,
                  -predicted_parameters,
                  -data,
                  -Country) %>%
    unnest(prediction) %>%
    filter(Country == cnst$cntr[[i]] & cv_id %in% cnst$cv_series_validation[[i]]) %>%
    group_by(model_id, model) %>%
    summarise(
      COV.95 = Coverage(observed, PIq1, PIq8, na.rm = TRUE),
      MIS.95 = MIS(observed, PIq1, PIq8, alpha = 0.1, na.rm = TRUE),
      COV.90 = Coverage(observed, PIq2, PIq7, na.rm = TRUE),
      MIS.90 = MIS(observed, PIq2, PIq7, alpha = 0.1, na.rm = TRUE),
      COV.80 = Coverage(observed, PIq3, PIq6, na.rm = TRUE),
      MIS.80 = MIS(observed, PIq3, PIq6, alpha = 0.1, na.rm = TRUE),
      COV.50 = Coverage(observed, PIq4, PIq5, na.rm = TRUE),
      MIS.50 = MIS(observed, PIq4, PIq5, alpha = 0.1, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Country = cnst$cntr[[i]])
}

# pooled
calibration_pooled <- list()

for (i in 1:length(cnst$cntr)) {
  calibration_pooled[[i]] <-
    prediction_intervals_pooled %>%
    dplyr::select(-predicted_quantiles,
                  -predicted_parameters,
                  -Country) %>%
    unnest(prediction) %>%
    filter(Country == cnst$cntr[[i]] & cv_id %in% cnst$cv_series_validation[[i]]) %>%
    group_by(model_id, model) %>%
    summarise(
      COV.95 = Coverage(observed, PIq1, PIq8, na.rm = TRUE),
      MIS.95 = MIS(observed, PIq1, PIq8, alpha = 0.1, na.rm = TRUE),
      COV.90 = Coverage(observed, PIq2, PIq7, na.rm = TRUE),
      MIS.90 = MIS(observed, PIq2, PIq7, alpha = 0.1, na.rm = TRUE),
      COV.80 = Coverage(observed, PIq3, PIq6, na.rm = TRUE),
      MIS.80 = MIS(observed, PIq3, PIq6, alpha = 0.1, na.rm = TRUE),
      COV.50 = Coverage(observed, PIq4, PIq5, na.rm = TRUE),
      MIS.50 = MIS(observed, PIq4, PIq5, alpha = 0.1, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Country = cnst$cntr[[i]])
}

calibration <- do.call(rbind, calibration)
calibration_pooled <- do.call(rbind, calibration_pooled)

calibration_full <- rbind(calibration, calibration_pooled) %>%
   arrange(Country, model, model_id) %>%
   relocate(Country)


# combine pooled and not-pooled data into one data set ------------
prediction_intervals <- rbind(prediction_intervals_single, prediction_intervals_pooled)


# Validation plots ------------------------------------------------

validation <- list()

validation$model_id_labs <- c(
  modelBased = '(a) Model based PIs',
  SNO = '(b) Empirical PIs (SNO)',
  rawQ = '(c) Empirical PIs (raw quantiles)',
  publ = '(d) Empirical PIs from publications',
  SNO_pooled = '(e) Pooled empirical PIs (SNO)',
  rawQ_pooled = '(f) Pooled empirical PIs (raw quantiles)',
  publ_pooled = '(g) Pooled empirical PIs from publications'
)

validation$data <-
  prediction_intervals %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters,
                -data,
                -Country) %>%
  unnest(prediction) %>%
  mutate(model_id = factor(model_id, names(validation$model_id_labs)))


validation$observed_vs_predicted_validation <- list()

for (i in 1:length(cnst$cntr)) {
  
  data.country <- validation$data %>% filter(cv_id %in% cnst$cv_series_validation[[cnst$cntr[[i]]]] &
                                             Country == cnst$cntr[[i]])
  validation.country <- list()
  
  for (j in 1:length(cnst$models)) {
    
    data.model <- data.country %>% filter(model == cnst$models[[j]])
    
    validation.country[[j]] <-
      data.model %>%
      ggplot() +
      aes(x = years_since_test_start, y = predicted) +
      geom_ribbon(
        aes(x = years_since_test_start, ymin = PIq1, ymax = PIq8),
        color = NA, fill = 'grey70'
      ) +
      geom_point(aes(y = observed), color = cnst$colors$cv_series['test'],
                 size = 0.2) +
      geom_line(color = '#860644') +
      facet_grid(
        cv_id ~ model_id,
        labeller = labeller(model_id = validation$model_id_labs)
      ) +
      figspec$MyGGplotTheme(axis = 'xy') +
      guides(color = 'none') +
      labs(y = 'TFR', x = 'Years into forecasting period',
           title = paste(data.model$Country, ", observed vs. predicted", cnst$models[[j]], ", validation series", sep = ""))
  }
  names(validation.country) <- cnst$models
  validation$observed_vs_predicted_validation[[i]] <- validation.country
}
names(validation$observed_vs_predicted_validation) <- cnst$cntr



# relative errors

validation$relative_errors_validation <- list()

for (i in 1:length(cnst$cntr)) {
  
  data.country <- validation$data %>% filter(cv_id %in% cnst$cv_series_validation[[cnst$cntr[[i]]]],
                                             Country == cnst$cntr[[i]])
  validation.country <- list()
  
  for (j in 1:length(cnst$models)) {
    
    data.model <- data.country %>% filter(model == cnst$models[[j]])
    
    validation.country[[j]] <-
      data.model %>%
      ggplot() +
      aes(x = years_since_test_start, y = score) +
      geom_ribbon(
        aes(x = years_since_test_start, ymin = q1, ymax = q8),
        color = NA, fill = 'grey70', alpha=0.5
      ) +
      geom_hline(yintercept = 0, color = 'grey50') +
      geom_point(color = cnst$colors$cv_series['test'], size = 0.2) +
      facet_wrap(
        ~model_id, labeller = labeller(model_id = validation$model_id_labs)
      ) +
      scale_y_continuous() +
      scale_color_manual(
        values = cnst$colors$cv_series
      ) +
      figspec$MyGGplotTheme(axis = 'xy') +
      guides(color = 'none') +
      labs(y = 'log(observed/predicted)', x = 'Years into forecasting period',
           title = paste(data.model$Country, ",", cnst$models[[j]], ", relative errors, validation series"))
  }
  names(validation.country) <- cnst$models
  validation$relative_errors_validation[[i]] <- validation.country
}
names(validation$relative_errors_validation) <- cnst$cntr


# Export ----------------------------------------------------------

saveRDS(calibration_full, paths$output$calibration_table)
saveRDS(validation, paths$output$validation_plots)
saveRDS(prediction_intervals, paths$output$prediction_intervals_full)


ExportFigure(
  validation$relative_errors_validation,
  paths$output$fig,
  filename = '03-relative_errors_validation',
  device = 'pdf',
#  width = config$figure$width,
#  height = config$figure$width*0.5
)

ExportFigure(
  validation$observed_vs_predicted_validation,
  paths$output$fig,
  filename = '03-observed_vs_predicted_validation',
  device = 'pdf',
#  width = config$figure$width,
#  height = config$figure$width*0.5
)
