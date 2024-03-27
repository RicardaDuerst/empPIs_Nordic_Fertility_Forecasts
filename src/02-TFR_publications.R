# Empirical TFR errors from previous publications
# and calculation of error distribution

# Init ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(parallel)
library(doParallel)
library(gamlss)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  Keilman_TFR = 'dat/Keilman_TFRerrors.csv',
  StatFin_TFR = 'dat/TFR_projections_statistics_Finland.xlsx',
  StatNor_TFR = 'dat/TFR_projections_Statistics_Norway.xlsx',
  StatSwe_TFR = 'dat/TFR_projections_Statistics_Sweden.xlsx',
  tfr_cv = 'tmp/01-tfr_cv.csv'
)
paths$output <- list(
  tfr_publ = 'tmp/02-TFR_publications.rds',
  PIs_publ = 'tmp/02-PIs_publications.rds',
  PIs_publ_pooled = 'tmp/02-PIs_publications_pooled.rds'
)

# constants specific to this analysis
cnst <- within(list(), {
  quantiles = c(0.025, 0.05, 0.1, 0.25, 0.75, 0.9, 0.95, 0.975)
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
  cntr_long = c("Finland", "Sweden", "Norway", "Denmark")
  # age vector
  ages <- 12:55
  # forecast length
  test_length = data.frame(Country = c("FIN", "SWE", "NOR", "DEN"),
                           length = c(30, 30, 15, 30))
})

# setup parallel computation
cnst$cl <- makeCluster(cnst$cpu_nodes, outfile = paths$output$log)
registerDoParallel(cnst$cl)

# global functions
source(paths$input$global)


# load data from Nico Keilman -------------------------------------------------------

TFR_Keilman <- read.csv(paths$input$Keilman_TFR, sep = ";") %>%
            filter(Country %in% cnst$cntr_long,
                   Variant == "M") %>%
            rename(origin_year_test = Forecast,
                   Year = Cal.year,
                   predicted = Pred.Value) %>%
            mutate(cv_sample = "test") %>%
            dplyr::select(-c(Indicator, Obs.value)) %>%
            mutate(Year = as.numeric(Year),
                   origin_year_test = as.numeric(origin_year_test),
                   Country = case_when(Country == "Finland" ~ "FIN",
                                       Country == "Denmark" ~ "DEN",
                                       Country == "Sweden" ~ "SWE",
                                       Country == "Norway" ~ "NOR"))

# load data from Statistics Finland --------------------------------------------

TFR_StatFin <- read_excel(paths$input$StatFin_TFR, skip = 3) %>%
               dplyr::select(-c(2,3,4)) %>%
               filter(!row_number() %in% c(1:11)) %>%
               rename(Year = '...1') %>%
               gather(-Year, key = "origin_year_test", value = "predicted") %>%
               mutate(Country = "FIN",
                      cv_sample = "test",
                      Variant = "M")

# load data from Statistics Norway ---------------------------------------------

TFR_StatNor <- read_excel(paths$input$StatNor_TFR) %>%
               pivot_longer(cols = 2:43,
                            names_to = c("origin_year_test", "Variant"),
                            names_pattern = "(....).(.)",
                            values_to = "predicted") %>%
               filter(Variant == "M") %>%
               mutate(Country = "NOR",
                      cv_sample = "test") %>%
               arrange(origin_year_test, Year, Variant)

# load data from Statistics Sweden ---------------------------------------------

TFR_StatSwe <- read_excel(paths$input$StatSwe_TFR) %>%
  pivot_longer(cols = 2:22,
               names_to = c("origin_year_test", "Variant"),
               names_pattern = "(....).(.)",
               values_to = "predicted") %>%
  filter(Variant  == "M") %>%
  mutate(Country = "SWE",
         cv_sample = "test") %>%
  arrange(origin_year_test, Year, Variant)


# bind country data together ---------------------------------------------------
TFR_Stat <-  rbind(TFR_StatFin, TFR_StatFin, TFR_StatNor, TFR_StatSwe) %>% # TFR_StatDen
             mutate(Year = as.numeric(Year),
                    origin_year_test = as.numeric(origin_year_test))


# fill in missing data with data from Nico Keilman (but new data has priority) -

# create empty skeleton with available prediction years from both datasets
TFR_STAT_empty <- TFR_Stat %>% dplyr::select(Year, origin_year_test, Country, Variant, cv_sample)
TFR_Keilman_empty <- TFR_Keilman %>% dplyr::select(Year, origin_year_test, Country, Variant, cv_sample)
TFR_skeleton <- union(TFR_STAT_empty, TFR_Keilman_empty)# duplicates are removed

# fill skeleton with new data from statistical office and then with Keilman's data
TFR_publ <- TFR_skeleton %>%
            left_join(TFR_Stat) %>%
            rows_patch(TFR_Keilman, # only NAs are filled in
                       by = c("Year", "origin_year_test", "Country", "Variant", "cv_sample")) %>%
            mutate(model = "published") %>%
            filter(!is.na(predicted))

# add cv_id based on forecast year and drop data if there is no observed data available (cv_id = NA)
a <- data.frame(Country = rep("FIN", 55), origin_year_test = 1969:2023, cv_id = 1:55)
b <- data.frame(Country = rep("SWE", 103), origin_year_test = 1921:2023, cv_id = 1:103)
c <- data.frame(Country = rep("NOR", 27), origin_year_test = 1997:2023, cv_id = 1:27)
d <- data.frame(Country = rep("DEN", 78), origin_year_test = 1946:2023, cv_id = 1:78)
year_cv_id <- rbind(a, b, c, d)

TFR_publ <- TFR_publ %>% left_join(year_cv_id) %>% filter(!is.na(cv_id)) %>% dplyr::select(-origin_year_test)



# retrieve an empirical error distribution from the published TFR forecasts -----

# load observed data
tfr_cv <- read.csv(paths$input$tfr_cv)

# join observed and forecast data, delete rows with missing forecast data
tfr_cv_predict <- left_join(tfr_cv, TFR_publ) %>% filter(!is.na(predicted)) %>% distinct()

# calculate forecast error
forecasting_error <- list()

# test series
forecasting_error$error_series <-
  tfr_cv_predict %>%
  filter(cv_sample == 'test') %>%
  rename(observed = TFR)

# simple forecasting error (log(observed/predicted))
forecasting_error$error_series <-
  forecasting_error$error_series %>% mutate(error = log(observed/predicted))

# plot errors
  forecasting_error$error_series %>%
  filter(Country == "FIN" & model == "published") %>%
  ggplot() +
  aes(x = years_since_test_start, y = error) +
  geom_point() +
  geom_hline(yintercept = 0, col = "grey")+
  labs(title = "Finland, empirical errors from publications, all CV series",
       y = "log(observed/predicted)")


# error model specifications
forecasting_error$specs <- tribble(
  ~model_id, ~model_spec,
  'rawQ', list(
    type = 'empirical',
    score = 'logratio'),
  'SNO', list(
    type = 'gamlss',
    family = gamlss.dist::SN1(sigma.link = 'log'),
    formula = as.formula(score~1),
    sigma.formula =
      as.formula(~ 1 + pbc(Year)),
    nu.formula = as.formula(~ 1 + pbc(Year)),
    tau.formula = as.formula(~ 1),
    score = 'logratio'
  ),
  'SNO_smooth', list(
    type = 'gamlss',
    family = gamlss.dist::SN1(sigma.link = 'log'),
    formula = as.formula(score~1),
    sigma.formula =
      as.formula(~ 1 + years_since_test_start),
    nu.formula = as.formula(~ 1 + years_since_test_start),
    tau.formula = as.formula(~ 1),
    score = 'logratio'
  ),
  'SNO_restricted', list(
    type = 'gamlss',
    family = gamlss.dist::SN1(sigma.link = 'log'),
    formula = as.formula(score~1),
    sigma.formula =
      as.formula(~ 1 + pbm(years_since_test_start, df = 1, mono = 'up')),
    nu.formula = as.formula(~ 1 + years_since_test_start),
    tau.formula = as.formula(~ 1),
    score = 'logratio'
  )
)

# merge data with model definitions
forecasting_error$for_fit <-
  forecasting_error$error_series %>%
  nest(data = c(-Country, -model)) %>%
  expand_grid(forecasting_error$specs)

# iterate in parallel model, country
forecasting_error$fitted <- foreach(
  x = iter(forecasting_error$for_fit, by = 'row'),
  .combine = bind_rows,
  .packages = c('dplyr', 'tidyr', 'gamlss', 'data.table')
) %dopar% {suppressPackageStartupMessages({
  
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Fit ", x$model_id,  " in ", x$Country, "\n",
      sep = ""
  )
  
  # input data
  input_data <- x[, "data"][[1]][[1]]
  
  # model parametrization
  model_para <- x$model_spec[[1]]
  score_type <- model_para[['score']]
  
  # add score
  input_data$score  <-
    NonconformityScore(score_type)$Score(input_data$observed,
                                         input_data$predicted)
  
  # the calibration data from which to learn the error distribution
  calibration <- input_data %>%
    filter(cv_id %in% cnst$cv_series_calibration[[x$Country]])
  # the predictors over which to construct the prediction intervals
  # needs to have the same length as the test series in each cv split
  X <- data.frame(years_since_test_start = c(0:30))
    # input_data %>%
    # filter(cv_id %in% (cnst$cv_series_validation[[x$Country]][3])) |>
    # dplyr::select(years_since_test_start, Year)
  
  # fit models and capture errors
  result <- tryCatch({
    
    
    #--------------------------------------------------------------------
    
    # model out-of-sample error by forecasting horizon
    if (identical(model_para$type, 'gamlss')) {
      
      fit <- gamlss(
        formula = eval(model_para$formula),
        sigma.formula = eval(model_para$sigma.formula),
        nu.formula = eval(model_para$nu.formula),
        tau.formula = eval(model_para$tau.formula),
        family = eval(model_para$family),
        data = na.omit(calibration),
        control = gamlss.control(n.cyc = 400)
      )
      
      # reconstruct the call to gamlss because the package
      # messes up the storage of that call evaluation when
      # in an foreach environment
      fit$call <-
        call('gamlss', model_para$formula, model_para$sigma.formula,
             model_para$nu.formula, model_para$tau.formula,
             model_para$family, calibration)
      
      predicted_time_varying_params <-
        predictAll(
          object = fit, data = calibration,
          newdata = X,
          type = 'response', output = 'list'
        )
      
      predicted_time_varying_params[['y']] <- NULL
      
      # get the name of the quantile function corresponding
      # to the distribution of our fitted model
      quantile_name <- paste0('q', model_para$family$family[1])
      distribution_name <- paste0('p', model_para$family$family[1])
      
      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      predicted_quantiles_of_error_distribution <- bind_cols(
        X,
        q1 = do.call(quantile_name, c(p = cnst$quantiles[1],
                                      predicted_time_varying_params)),
        q2 = do.call(quantile_name, c(p = cnst$quantiles[2],
                                      predicted_time_varying_params)),
        q3 = do.call(quantile_name, c(p = cnst$quantiles[3],
                                      predicted_time_varying_params)),
        q4 = do.call(quantile_name, c(p = cnst$quantiles[4],
                                      predicted_time_varying_params)),
        q5 = do.call(quantile_name, c(p = cnst$quantiles[5],
                                      predicted_time_varying_params)),
        q6 = do.call(quantile_name, c(p = cnst$quantiles[6],
                                      predicted_time_varying_params)),
        q7 = do.call(quantile_name, c(p = cnst$quantiles[7],
                                      predicted_time_varying_params)),
        q8 = do.call(quantile_name, c(p = cnst$quantiles[8],
                                      predicted_time_varying_params)),
        pscore_10p = do.call(
          distribution_name, c(q = log(1.1),
                               predicted_time_varying_params,
                               lower.tail = FALSE))
      )
      
      predicted_quantiles_of_forecast_distribution <-
        input_data |>
        dplyr::select(years_since_test_start, cv_id, observed, predicted, score) |>
        left_join(
          predicted_quantiles_of_error_distribution %>%
            dplyr::select(years_since_test_start, q1, q2, q3, q4, q5, q6, q7, q8),
          by = 'years_since_test_start'
        ) %>%
        # apply predicted quantiles of error distribution to the point
        # forecasts to derive the prediction intervals
        mutate(across(
          c(q1, q2, q3, q4, q5, q6, q7, q8),
          ~NonconformityScore(score_type)$InverseScore(., predicted),
          .names = 'PI{.col}')) %>%
        mutate(Country = x$Country)
      
    }
    
    #--------------------------------------------------------------------
    
    # raw quantiles
    if (identical(model_para$type, 'empirical')) {
      
      predicted_time_varying_params <- NA
      
      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      predicted_quantiles_of_error_distribution <-
        calibration |>
        transmute(
          cv_id, years_since_test_start,
          observed, predicted,
          score
        ) |> group_by(years_since_test_start) %>%
        summarise(
          q1 = quantile(score, cnst$quantiles[1]),
          q2 = quantile(score, cnst$quantiles[2]),
          q3 = quantile(score, cnst$quantiles[3]),
          q4 = quantile(score, cnst$quantiles[4]),
          q5 = quantile(score, cnst$quantiles[5]),
          q6 = quantile(score, cnst$quantiles[6]),
          q7 = quantile(score, cnst$quantiles[7]),
          q8 = quantile(score, cnst$quantiles[8])
        ) %>%
        mutate(Country = x$Country)
      
      predicted_quantiles_of_forecast_distribution <-
        input_data %>%
        dplyr::select(years_since_test_start, cv_id, observed, predicted, score) |>
        left_join(
          predicted_quantiles_of_error_distribution
        ) %>%
        # apply predicted quantiles of error distribution to the point
        # forecasts to derive the prediction intervals
        mutate(across(
          c(q1, q2, q3, q4, q5, q6, q7, q8),
          ~NonconformityScore(score_type)$InverseScore(., predicted),
          .names = 'PI{.col}'))
      
    }
    
    #-------------------------------------------------------------------
    
    # return result if fitting succeeded
    result_if_no_error <- bind_cols(
      x,
      tibble(
        prediction = list(predicted_quantiles_of_forecast_distribution),
        predicted_quantiles = list(predicted_quantiles_of_error_distribution),
        predicted_parameters = list(predicted_time_varying_params)
      ),
      error_while_fit = FALSE,
      error_message = NA
    )
    
    return(result_if_no_error)
    
  },
  
  # return result if fitting did not succeed
  error = function(e) {
    cat(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Error on ", x$model_id, " in ", x$Country, ": ",
      geterrmessage(), "\n"
    )
    # return same object as fitted model, but with NA predictions
    input_data$q1 <- NA; input_data$q2 <- NA
    input_data$q3 <- NA; input_data$q4 <- NA
    input_data$q5 <- NA; input_data$q6 <- NA
    input_data$q7 <- NA; input_data$q8 <- NA
    input_data$PIq1 <- NA; input_data$PIq2 <- NA
    input_data$PIq3 <- NA; input_data$PIq4 <- NA
    input_data$PIq5 <- NA; input_data$PIq6 <- NA
    input_data$PIq7 <- NA; input_data$PIq8 <- NA
    result_if_error <- bind_cols(
      x,
      tibble(
        prediction = list(input_data),
        predicted_quantiles = NA,
        predicted_parameters = NA
      ),
      error_while_fit = TRUE,
      error_message = geterrmessage()
    )
    return(result_if_error)
  }
  ) # end of tryCatch()
  
  return(result)
  
})} # end of dopar(suppressPackageStartupMessages)

stopCluster(cnst$cl)

# extract the predicted quantiles of the error distribution
Publ_PIs <- do.call(rbind, forecasting_error$fitted$predicted_quantiles)


# retrieve an empirical error distribution from country-pooled data -------------

# merge data with model definitions
forecasting_error$for_fit_pooled <-
  forecasting_error$error_series %>%
  nest(data = c(-model)) %>%
  expand_grid(forecasting_error$specs)


# iterate in parallel model
forecasting_error$fitted_pooled <- foreach(
  x = iter(forecasting_error$for_fit_pooled, by = 'row'),
  .combine = bind_rows,
  .packages = c('dplyr', 'tidyr', 'gamlss', 'data.table')
) %dopar% {suppressPackageStartupMessages({
  
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Fit ", x$model, x$model_id,  "\n",
      sep = " "
  )

  # input data
  input_data <- x[, "data"][[1]][[1]]

  # model parametrization
  model_para <- x$model_spec[[1]]
  score_type <- model_para[['score']]

  # add score
  input_data$score  <-
    NonconformityScore(score_type)$Score(input_data$observed,
                                         input_data$predicted)

  # the calibration data from which to learn the error distribution
  calibration <- input_data
  # the predictors over which to construct the prediction intervals
  # needs to have the same length as the test series in each cv split
  X <- input_data |>
    filter(cv_id==23) |> #%in% cnst$cv_series_validation[["DEN"]][1]
    dplyr::select(years_since_test_start, Year)

  # fit models and capture errors
  result <- tryCatch({
 
   #--------------------------------------------------------------------
  
  # model out-of-sample error by forecasting horizon
  if (identical(model_para$type, 'gamlss')) {
    
    fit <- gamlss(
      formula = eval(model_para$formula),
      sigma.formula = eval(model_para$sigma.formula),
      nu.formula = eval(model_para$nu.formula),
      tau.formula = eval(model_para$tau.formula),
      family = eval(model_para$family),
      data = calibration,
      control = gamlss.control(n.cyc = 400)
    )
    
    # reconstruct the call to gamlss because the package
    # messes up the storage of that call evaluation when
    # in an foreach environment
    fit$call <-
      call('gamlss', model_para$formula, model_para$sigma.formula,
           model_para$nu.formula, model_para$tau.formula,
           model_para$family, calibration)
    
    predicted_time_varying_params <-
      predictAll(
        object = fit, data = calibration,
        newdata = X,
        type = 'response', output = 'list'
      )
    
    predicted_time_varying_params[['y']] <- NULL
    
    # get the name of the quantile function corresponding
    # to the distribution of our fitted model
    quantile_name <- paste0('q', model_para$family$family[1])
    distribution_name <- paste0('p', model_para$family$family[1])
    
    # get the quantiles of the empirical error distribution
    # over forecasting horizon
    predicted_quantiles_of_error_distribution <- bind_cols(
      X,
      q1 = do.call(quantile_name, c(p = cnst$quantiles[1],
                                    predicted_time_varying_params)),
      q2 = do.call(quantile_name, c(p = cnst$quantiles[2],
                                    predicted_time_varying_params)),
      q3 = do.call(quantile_name, c(p = cnst$quantiles[3],
                                    predicted_time_varying_params)),
      q4 = do.call(quantile_name, c(p = cnst$quantiles[4],
                                    predicted_time_varying_params)),
      q5 = do.call(quantile_name, c(p = cnst$quantiles[5],
                                    predicted_time_varying_params)),
      q6 = do.call(quantile_name, c(p = cnst$quantiles[6],
                                    predicted_time_varying_params)),
      q7 = do.call(quantile_name, c(p = cnst$quantiles[7],
                                    predicted_time_varying_params)),
      q8 = do.call(quantile_name, c(p = cnst$quantiles[8],
                                    predicted_time_varying_params)),
      
    ) %>% dplyr::select(-Year)
    
    predicted_quantiles_of_forecast_distribution <-
      input_data |>
      dplyr::select(Country, years_since_test_start, cv_id, observed, predicted, score, Year) |>
      left_join(
        predicted_quantiles_of_error_distribution
      ) %>%
      # apply predicted quantiles of error distribution to the point
      # forecasts to derive the prediction intervals
      mutate(across(
        c(q1, q2, q3, q4, q5, q6, q7, q8),
        ~NonconformityScore(score_type)$InverseScore(., predicted),
        .names = 'PI{.col}'))
    
  }
  
   #--------------------------------------------------------------------
  
    # raw quantiles
    if (identical(model_para$type, 'empirical')) {

      predicted_time_varying_params <- NA

      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      predicted_quantiles_of_error_distribution <-
        calibration |>
        transmute(
          cv_id, years_since_test_start,
          observed, predicted,
          score
        ) |> group_by(years_since_test_start) %>%
        summarise(
          q1 = quantile(score, cnst$quantiles[1]),
          q2 = quantile(score, cnst$quantiles[2]),
          q3 = quantile(score, cnst$quantiles[3]),
          q4 = quantile(score, cnst$quantiles[4]),
          q5 = quantile(score, cnst$quantiles[5]),
          q6 = quantile(score, cnst$quantiles[6]),
          q7 = quantile(score, cnst$quantiles[7]),
          q8 = quantile(score, cnst$quantiles[8])
        )

      predicted_quantiles_of_forecast_distribution <-
        input_data %>%
        dplyr::select(Country, years_since_test_start, cv_id, observed, predicted, score) |>
        left_join(
          predicted_quantiles_of_error_distribution
        )  %>%
        # apply predicted quantiles of error distribution to the point
        # forecasts to derive the prediction intervals
        mutate(across(
          c(q1, q2, q3, q4, q5, q6, q7, q8),
          ~NonconformityScore(score_type)$InverseScore(., predicted),
          .names = 'PI{.col}'))

    }

    #-------------------------------------------------------------------

  # prediction intervals from publications
  if (identical(model_para$type, 'publications')) {
    
    predicted_time_varying_params <- NA
    predicted_quantiles_of_error_distribution <- PIs_publ_pooled %>%
      dplyr::select(years_since_test_start,
                    q1, q2, q3, q4, q5, q6, q7, q8)
    
    predicted_quantiles_of_forecast_distribution <-
      input_data %>%
      dplyr::select(Country, years_since_test_start, cv_id, observed, predicted, score) |>
      left_join(
        predicted_quantiles_of_error_distribution
      ) %>%
      # apply predicted quantiles of error distribution to the point
      # forecasts to derive the prediction intervals
      mutate(across(
        c(q1, q2, q3, q4, q5, q6, q7, q8),
        ~NonconformityScore(score_type)$InverseScore(., predicted),
        .names = 'PI{.col}'))
  }
  
  #-------------------------------------------------------------------
  
  # return result if fitting succeeded
  result_if_no_error <- bind_cols(
    x,
    tibble(
      prediction = list(predicted_quantiles_of_forecast_distribution),
      predicted_quantiles = list(predicted_quantiles_of_error_distribution),
      predicted_parameters = list(predicted_time_varying_params)
    ),
    error_while_fit = FALSE,
    error_message = NA
  )
  
  return(result_if_no_error)
  
},

# return result if fitting did not succeed
error = function(e) {
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " Error on ", x$model_id, " in ", x$Country, ": ",
    geterrmessage(), "\n"
  )
  # return same object as fitted model, but with NA predictions
  input_data$q1 <- NA; input_data$q2 <- NA
  input_data$q3 <- NA; input_data$q4 <- NA
  input_data$q5 <- NA; input_data$q6 <- NA
  input_data$q7 <- NA; input_data$q8 <- NA
  input_data$PIq1 <- NA; input_data$PIq2 <- NA
  input_data$PIq3 <- NA; input_data$PIq4 <- NA
  input_data$PIq5 <- NA; input_data$PIq6 <- NA
  input_data$PIq7 <- NA; input_data$PIq8 <- NA
  result_if_error <- bind_cols(
    x,
    tibble(
      prediction = list(input_data),
      predicted_quantiles = NA,
      predicted_parameters = NA
    ),
    error_while_fit = TRUE,
    error_message = geterrmessage()
  )
  return(result_if_error)
}
) # end of tryCatch()
  
  return(result)
  
})} # end of dopar(suppressPackageStartupMessages)

# extract the predicted quantiles of the error distribution
Publ_PIs_pooled <- do.call(rbind, forecasting_error$fitted_pooled$predicted_quantiles)

# Export ------------------------------------------------------------------------

saveRDS(TFR_publ, paths$output$tfr_publ)
# saveRDS(Publ_PIs, paths$output$PIs_publ)
saveRDS(Publ_PIs_pooled, paths$output$PIs_publ_pooled)
