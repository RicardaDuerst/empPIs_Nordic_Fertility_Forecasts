# Global functions

# Split time series into cross-validation series ------------------

CutCVData <- function(training_length, test_length, cv_data){
  
  last_observed <-
    max(cv_data$Year)
  
  number_of_series <-
    max(cv_data$Year) - test_length - training_length + 1 - min(cv_data$Year) + 1
  
  last_years_of_series <- 
    seq(from = min(cv_data$Year) + training_length + test_length - 1, by = 1, length.out = number_of_series)
  
  data_cv_series <- list()
  
  for (i in 1:number_of_series) {
    data_cv_series[[i]] <-
      cv_data %>%
      filter(
        Year %in% rev(seq(last_years_of_series[i],
                          by = -1,
                          length = training_length + test_length))
      ) %>%
      mutate(
        origin_year =
          last_years_of_series[i] - training_length - test_length + 1,
        origin_year_test =
          last_years_of_series[i] - test_length + 1
      ) %>%
      mutate(cv_id = i)
    
    data_cv_series[[i]] <-
      data_cv_series[[i]] %>%
      mutate(series_year =
               Year - origin_year + 1,
             years_since_test_start =
               Year - origin_year_test
      )
    
    data_cv_series[[i]] <-
      data_cv_series[[i]] %>%
      mutate(
        cv_sample = if_else(years_since_test_start >= 0, 'test', 'training')
      )
  }
  
  cut_stats <- within(list(), {
    number_of_series = number_of_series
    last_years_of_series = last_years_of_series
  })
  
  print(cut_stats)
  return(bind_rows(data_cv_series))
}


# Scoring rules ---------------------------------------------------

#' MEAN! Interval Score
#'
#' @param observed observed number of deaths
#' @param lower lower bound of prediction interval
#' @param upper upper bound of prediction interval
#' @param alpha quantile level (e.g. alpha = 0.1 for 90% prediction interval)
#' @param na.rm whether or not missing values should be removed
#'
#' @return
#' @export
#'
#' @examples
MIS <- function (observed, lower, upper, alpha, na.rm = FALSE) {
  N = length(observed)
  below_lower = observed < lower
  above_upper = observed > upper
  interval_width = upper - lower
  
  mis <- sum(
    interval_width +
      2/alpha*(lower-observed)*below_lower +
      2/alpha*(observed-upper)*above_upper,
    na.rm = na.rm
  ) / N
  
  return(mis)
}

#' Coverage
#'
#' @param observed observed number of deaths
#' @param lower lower bound of prediction interval
#' @param upper upper bound of prediction interval
#' @param na.rm whether or not missing values should be removed
#'
#' @return
#' @export
#'
#' @examples
Coverage <- function (observed, lower, upper, na.rm = FALSE) {
  N = length(observed)
  below_upper = observed < upper
  above_lower = observed > lower
  within_interval = below_upper & above_lower
  
  cov <- sum(within_interval, na.rm = na.rm) / N
  
  return(cov)
}

# Nonconformity scores --------------------------------------------

# this functions allows for implementation of different nonconformity scores
# (observed, predicted) -> score and their inverse
# (score, predicted) -> observed
NonconformityScore <- function (type = 'logratio') {
  switch (type,
          'logratio' = list(
            Score = function (observed, predicted) {
              log(observed) - log(predicted)
            },
            InverseScore = function (score, predicted) {
              exp(score + log(predicted))
            }
          ),
          'residual' = list(
            Score = function (observed, predicted) {
              observed - predicted
            },
            InverseScore = function (score, predicted) {
              score + predicted
            }
          )
  )
}

# Figures ---------------------------------------------------------

# fonts
library(showtext)
# font_add_google("Roboto", "roboto")
# font_add_google("Roboto Condensed", "robotocondensed")
# showtext_auto()

figspec <- list()
figspec <- within(figspec, {
  
  # color coding
  colors <- list(
    sample =
      c(
        training = "grey30",
        test = "red"
      ),
    sex =
      c(
        `Male` = "#004B87",
        `Female` = "#c60c30"
      )
  )
  
  # figure dimensions in mm
  fig_dims <- list(width = 180)
  
  # ggplot theme by Jonas Schöley
  MyGGplotTheme <-
    function(size = 8,
             family = "sans",
             scaler = 1,
             axis = "x",
             panel_border = FALSE,
             grid = "y",
             minor_grid = "",
             show_legend = TRUE,
             ar = NA,
             axis_title_just = "rt",
             axis_ticks = TRUE) {
      size_med <- size * scaler
      size_sml <- round(size * 0.7) * scaler
      base_linesize <- 0.3 * scaler
      
      # justification of axis titles
      xj <- switch(tolower(substr(axis_title_just, 1, 1)),
                   b = 0,
                   l = 0,
                   m = 0.5,
                   c = 0.5,
                   r = 1,
                   t = 1
      )
      yj <- switch(tolower(substr(axis_title_just, 2, 2)),
                   b = 0,
                   l = 0,
                   m = 0.5,
                   c = 0.5,
                   r = 1,
                   t = 1
      )
      
      list(
        theme_minimal(base_size = size_med, base_family = family),
        theme(
          # basic
          text = element_text(color = "black"),
          line = element_line(size = base_linesize, lineend = "square"),
          # axis
          axis.title = element_text(size = size_med, face = "bold"),
          axis.title.x = element_text(hjust = xj),
          axis.title.y = element_text(hjust = yj),
          axis.title.y.right = element_text(hjust = yj, angle = 90),
          axis.text = element_text(size = size_med, color = "black"),
          # strips
          strip.text = element_text(color = "black", size = size_med),
          strip.background = element_blank(),
          # plot
          title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "black", size = size_med, face = "bold"),
          plot.caption = element_text(color = "black", size = size_sml, face = "plain"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          # plot.margin = unit(c(1, 0.1, 0.5, 0.5), units = 'mm'),
          # grid
          panel.grid = element_blank()
        ),
        if (isTRUE(axis_ticks)) {
          theme(axis.ticks = element_line(size = rel(0.5), color = "black"))
        },
        if (identical(grid, "y")) {
          theme(
            panel.grid.major.y =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(grid, "x")) {
          theme(
            panel.grid.major.x =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(grid, "xy") | identical(grid, "yx")) {
          theme(
            panel.grid.major.y =
              element_line(size = base_linesize, linetype = 3, color = "grey80"),
            panel.grid.major.x =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(minor_grid, "y")) {
          theme(
            panel.grid.minor.y =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(minor_grid, "x")) {
          theme(
            panel.grid.minor.x =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(minor_grid, "xy") | identical(grid, "yx")) {
          theme(
            panel.grid.minor.y =
              element_line(size = base_linesize, linetype = 3, color = "grey80"),
            panel.grid.minor.x =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (isTRUE(panel_border)) {
          theme(
            panel.border =
              element_rect(fill = NA)
          )
        },
        if (!isTRUE(show_legend)) {
          theme(legend.position = "none")
        },
        if (axis == "x") {
          theme(
            axis.line.x = element_line(linetype = 1, color = "black")
          )
        },
        if (axis == "y") {
          theme(
            axis.line.y = element_line(linetype = 1, color = "black")
          )
        },
        if (axis == "xy") {
          theme(
            axis.line = element_line(linetype = 1, color = "black")
          )
        },
        if (!is.na(ar)) {
          theme(
            aspect.ratio = ar
          )
        }
      )
    }
})

#' Export ggplot
#'
#' @author Jonas Schöley
ExportFigure <-
  function(figure,
           path,
           filename,
           width = 180,
           height = 100,
           scale = 1,
           device = "png",
           dpi = 300,
           add_date = FALSE) {
    require(ggplot2)
    
    if (missing(filename)) {
      filename <- tolower(gsub("\\.", "_", make.names(deparse(substitute(figure)))))
    }
    if (isTRUE(add_date)) {
      filename <- paste0(Sys.Date(), "-", filename)
    }
    
    arguments <-
      list(
        filename = paste0(filename, ".", device),
        plot = figure,
        path = path,
        width = width,
        height = height,
        units = "mm",
        scale = scale,
        dpi = dpi,
        device = device
      )
    if (device == "pdf") {
      arguments$useDingbats <- FALSE
    }
    
    do.call(ggsave, arguments)
  }



#' Function to create object to use with StMoMo() -------------------------------- 
#'
#' @param df data frame with time series of age specific
#'           births and exposures
#' @param name_births name of birth count variable in df
#' @param name_exposure name of exposure variable in df
#' @param years vector of unique years in the data
#' @param age vector of unique ages in the data
#' @param sex "female"
#' @param label Name of data series
#'
#' @return
#' An StMoMo object
Pop2StMoMo <- function (
    df,
    name_births = 'Births', name_exposure = 'Exposure',
    years, sex, label, age = cnst$age_start:cnst$age_end
) {
  nage = length(age)
  nyear = length(years)
  
  Dxt_vec <- df[[name_births]]
  Dxt <- matrix(Dxt_vec, nrow = nage, ncol = nyear)
  colnames(Dxt) <- years
  rownames(Dxt) <- age
  
  Ext_vec <- df[[name_exposure]]
  Ext <- matrix(Ext_vec, nrow = nage, ncol = nyear)
  colnames(Ext) <- years
  rownames(Ext) <- age
  
  stmomodata <- structure(
    list(Dxt = Dxt, Ext = Ext, ages = age, years = years,
         type = 'central', series = sex, label = label),
    class = "StMoMoData"
  )
  
  return(stmomodata)
}


#' StMoMo Lee-Carter Forecast --------------------------------------------------------------
#'
#' @param stmomo StMoMo object created with Pop2StMoMo
#' @param h forecast horizont
#' @param betas specification for LC beta_x parameters
#'              ('np' for nonparametric, 'p0' for constant,
#'               'p1' for linear over age, 'p2' for quadratic,
#'               'p3' for cubic)
#' @param nsim number of simulations
#' @param seed simulation seed
#' @param maxit maximum number of iterations
#'
#' @return
#' List with elements "fit" and "sim"
ForecastLeeCarter <- function (stmomo, h = 1, betas = 'np',
                               nsim = 500, seed = 1987, maxit = 1e2) {
  
  constLC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    c1 <- mean(kt[1, ], na.rm = TRUE)
    c2 <- sum(bx[, 1], na.rm = TRUE)
    list(ax = ax + c1 * bx, bx = bx / c2, kt = c2 * (kt - c1))
  }
  
  f1 <- function (x, ages) {x}
  f2 <- function (x, ages) {x^2}
  f3 <- function (x, ages) {x^3}
  paf <- switch (betas,
                 np = 'NP',
                 p0 = '1',
                 p1 = c(1, f1),
                 p2 = c(1, f1, f2),
                 p3 = c(1, f1, f2, f3)
  )
  
  LC_def <- StMoMo(link = 'log', staticAgeFun = TRUE,
                   periodAgeFun = paf)
  LC_fit <- fit(LC_def, data = stmomo, iterMax = maxit, trace = TRUE)
  LC_avg <-
    forecast(
      LC_fit, h = h,
      jumpchoice = 'actual', kt.method = 'mrwd'
    )[['rates']]
  LC_sim <-
    simulate(LC_fit, nsim = nsim, h = h,
             jumpchoice = 'actual',
             seed = seed,
             kt.method = 'mrwd'
    )[['rates']]
  
  return(list(forecast_fit = LC_fit, forecast_avg = LC_avg,
              forecast_sim = LC_sim))
}