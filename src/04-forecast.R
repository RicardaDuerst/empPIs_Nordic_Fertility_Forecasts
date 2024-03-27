# forecasting Nordic fertility up to 2050
# using the following models: naive, PPS, Lee-Carter
# calculate model-based prediction intervals where applicable
# create data set with simulated TFR paths from forecasts using PPS und Lee-Carter

# Init ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(StMoMo)
library(rugarch)
library(dynlm)
library(FinTS)
library(stats)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  asfr = 'tmp/00-ASFR.csv',
  tfr = 'tmp/00-TFR.csv',
  births_exposure = 'tmp/00-B_E.csv',
  PPS_forecast = 'tmp/quant_PI_years.xlsx',
  PPS_sim_FIN = 'tmp/sim_TFR_paths_FIN.xlsx',
  PPS_sim_SWE = 'tmp/sim_TFR_paths_SWE.xlsx',
  PPS_sim_NOR = 'tmp/sim_TFR_paths_NOR.xlsx',
  PPS_sim_DEN = 'tmp/sim_TFR_paths_DNK.xlsx'
)
paths$output <- list(
  application_forecast = 'tmp/04-application_forecast.rds',
  forecast_sim = 'tmp/04-application_forecast-simulations.rds'
)

# constants specific to this analysis
cnst <- within(list(), {
  quantiles = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
  cv_series_calibration = list("FIN" = 1:20, "SWE" = 1:68, "NOR" = 1:7, "DEN" = 1:43)
  cv_series_validation = list("FIN" = 21:25, "SWE" = 69:73, "NOR" = 8:12, "DEN" = 44:48) # last 5 series
  colors = list(
    cv_series = c(test = '#f51883', calibration = '#117396')
  )
  # list of countries
  cntr = c("FIN", "SWE", "NOR", "DEN")
  # list of forecast models
  models = c("LC", "PPS", "ARCH", "naive")
  # number of years used as base period (input)
  bp = 30
  # last year of base period
  bp_last = 2022
  # last year of forecast horizon
  fh_last = 2050
  # parameters for LC
  nsim = 250
  seed = 1987
  # age vector
  ages <- 12:55
})

# global functions
source(paths$input$global)


# skeleton of final data set including countries (4 different), forecasts (4 different), prediction intervals (4 different)

fh <- (cnst$bp_last+1):cnst$fh_last # vector of forecast years
bp_years <- (cnst$bp_last-cnst$bp+1):cnst$bp_last # vector of base period years
fh_length <- length(fh) # length of forecast horizon
  
skeleton <- data.frame(Country = rep(cnst$cntr, each = length(fh)*length(cnst$models)),
                       Year = rep(fh, length(cnst$models)),
                       model = rep(cnst$models, each = length(fh)))


# Load time series of ASFR, TFR, Births and Exposures -------------------

asfr <- fread(paths$input$asfr)
tfr <- fread(paths$input$tfr)
B_E <- fread(paths$input$births_exposure)


# naive model -----------------------------------------------------------

naive_forecast <- list()

for (i in 1:length(cnst$cntr)) {
  tfr.country <- tfr %>% filter(Country == cnst$cntr[i]) %>% rbind(fh, fill = T) %>%
                 mutate(Year = if_else(is.na(Year), x, Year),
                        Country = cnst$cntr[[i]]) %>%
                 dplyr::select(-x) %>%
                 rename(observed = TFR)
                 
  naive_forecast[[i]] <- tfr.country %>%
      mutate(predicted = if_else(Year > cnst$bp_last, observed[Year == cnst$bp_last], NA),
             model = "naive") %>%
      dplyr::select(-observed) %>%
      filter(Year %in% fh)
}

# bind country data sets together
naive_forecast <- do.call(rbind, naive_forecast)


# Lee-Carter model -------------------------------------------------------

# create stmomo object for each country
stmomo <- list()

for (i in 1:length(cnst$cntr)) {
  
  stmomo[[i]] <- Pop2StMoMo(
    B_E |> filter(Country == cnst$cntr[[i]] & Year %in% bp_years),
    name_births = 'Births',
    name_exposure = 'Exposure',
    age = cnst$ages,
    years = bp_years,
    sex = 'female',
    label = cnst$cntr[[i]]
  )
}
names(stmomo) <- cnst$cntr

# fit Lee-carter model,
# usually we would set betas = 'np' for the standard Lee-Carter
# but in cases where the model does not converge, as for Denmark and Norway,
# we restrict the beta's to follow a cubic function over age

fit <- list()

for (i in 1:2) { # without Denmark and Norway
  fit[[i]] <- ForecastLeeCarter(
    stmomo[[i]],
    h = fh_length, betas = 'np',
    nsim = cnst$nsim,
    seed = cnst$seed
  )
}

# for Denmark and Norway
for (i in 3:4) {
  fit[[i]] <- ForecastLeeCarter(
    stmomo[[i]],
    h = fh_length, betas = 'p3',
    nsim = cnst$nsim,
    seed = cnst$seed
  )
}
names(fit) <- cnst$cntr

# Calculate forecast Total Fertility rate over simulations
LC_forecast_sim <- list()

for (i in 1:length(cnst$cntr)) {
  LC_forecast_sim[[i]] <- as.data.frame.table(apply(fit[[i]]$forecast_sim, 2:3, function (asfr) sum(asfr))) %>%
    mutate(Country = cnst$cntr[i], model = "LC") %>%
    rename(Year = Var1,
           sim = Var2,
           TFR = Freq)
}
names(LC_forecast_sim) <- cnst$cntr

# calculate quantiles and to receive prediction intervals and median forecast

LC_forecast <- list()

for (i in 1:length(cnst$cntr)) {
  LC_forecast_year <- list()
  for (j in 1:length(fh)) {
    year <- LC_forecast_sim[[i]] %>% filter(Year == fh[j])
    LC_forecast_year[[j]] <- quantile(year$TFR, probs = cnst$quantiles) %>%
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
                                    Year = fh[j],
                                    Country = cnst$cntr[i])
  }
  LC_forecast[[i]] <- do.call(rbind, LC_forecast_year)
}
LC_forecast <- do.call(rbind, LC_forecast)


# # ARCH model -------------------------------------------------------------
# 
# # Set up automatic routine to find best GARCH order and ARMA order for best fitting GARCH model
# # to forecast TFR for each country
# # selection based on smallest AIC
# 
# model_orders <- data.frame(GARCH_a = c(1,1,2,2),
#                            GARCH_b = c(1,2,0,1),
#                            ARMA_a = c(1,1,1,1),
#                            ARMA_b = c(2,2,2,2),
#                            AIC = NA)
# 
# GARCH_spec <- vector(mode = "list",
#                      length = length(cnst$cntr))
# 
# for (i in 1:length(cnst$cntr)) {
#   
#   # filter for country and base period years
#   tfr.country <- filter(tfr, Country == cnst$cntr[i] & Year %in% bp_years)
#   GARCH_spec[[i]] <- list()
# 
#     for (k in 1:nrow(model_orders)) {
#       tryCatch({
#         # model specification
#         spec <- ugarchspec(variance.model = list(model="sGARCH",
#                                                  garchOrder = c(model_orders[k,1], model_orders[k,2])),
#                            mean.model = list(armaOrder=c(model_orders[k,3],model_orders[k,4])),
#                            distribution.model="norm")
#         # fit the model
#         fit <- ugarchfit(data = tfr.country$TFR, spec = spec)
#         # save the AIC value
#         model_orders[k,5] <- infocriteria(fit)[1]
#         
#       }, error=function(e){})
#     }
#     
#     # select model specification with lowest AIC value
#     best_fit <- model_orders %>% filter(AIC == min(AIC, na.rm = T))
#     # save model spec in list
#     GARCH_spec[[i]] <- best_fit
# }
# names(GARCH_spec) <- cnst$cntr
# 
# # problem: unable to fit an ARCH model to Finnish data (no problem for the other countries)
# 
# # fit and forecast the best fitting model
# ARCH_forecast <- vector(mode = "list",
#                         length = length(cnst$cntr))
# ARCH_PI <- vector(mode = "list",
#                         length = length(cnst$cntr))
# 
# for (i in 2:length(cnst$cntr)) { # without Finland
#   
#   # filter for country and base period years
#   tfr.country <- filter(tfr, Country == cnst$cntr[i] & Year %in% bp_years)
#   
#   ARCH_forecast[[i]] <- list
#     
#     # fit the model
#     spec <- ugarchspec(variance.model = list(model = "sGARCH",
#                                              garchOrder = c(GARCH_spec[[i]][,1], GARCH_spec[[i]][,2])),
#                        mean.model = list(armaOrder = c(GARCH_spec[[i]][,3],GARCH_spec[[i]][,4])),
#                        distribution.model = "norm")
#     fit <- ugarchfit(data = tfr.country$TFR, spec = spec)
#     # forecast
#     ARCH_forecast[[i]] <- ugarchforecast(fit, n.ahead = fh_length)
#     # extract the forecast values
#     ARCH_forecast[[i]] <- ARCH_forecast[[i]]@forecast[["seriesFor"]][1:fh_length] %>%
#       as.data.frame() %>% setNames("predicted") %>%
#       mutate(Country = cnst$cntr[i],
#              Year = fh,
#              model = "ARCH")
#     # create model-based prediction intervals via bootstrapping
#     boot <- ugarchboot(fit, method = "Partial", n.ahead = fh_length, n.bootpred = 2000)
#     ARCH_PI[[i]] <- as.data.frame(boot, which = "sigma", type = "q", qtile = c(0.025, 0.975, 0.9, 0.1)) %>%
#                     t() %>%
#                     as.data.frame() %>%
#                     rename(U95.model = q0.975,
#                            L95.model = q0.025,
#                            U90.model = q0.95,
#                            L90.model = q0.05,
#                            U80.model = q0.9,
#                            L80.model = q0.1,
#                            U50.model = q0.75,
#                            L50.model = q0.25) %>%
#                     mutate(Country = cnst$cntr[i],
#                            Year = fh,
#                            model = "ARCH")
#     # join forecast und PI values
#     ARCH_forecast[[i]] <- left_join(ARCH_forecast[[i]], ARCH_PI[[i]])
# }
# names(ARCH_forecast) <- cnst$cntr
# 
# # bind country data sets together (without Finland)
# ARCH_forecast <- do.call(rbind, ARCH_forecast[-1])


# PPS model --------------------------------------------------------------

# load data provided by Julia Hellstrand
Julia_input <- readxl::read_xlsx(paths$input$PPS_forecast)

# bring into needed format (same as for the other forecast models)
PPS_forecast <- Julia_input %>%
  rename(predicted = q_0.50,
         U95.model = q_0.975,
         L95.model = q_0.025,
         U90.model = q_0.95,
         L90.model = q_0.05,
         U80.model = q_0.90,
         L80.model = q_0.10,
         U50.model = q_0.75,
         L50.model = q_0.25,
         Year = forecast_year) %>%
  mutate(model = "PPS",
         Country = case_when(country == "Finland" ~ "FIN",
                             country == "Denmark" ~ "DEN",
                             country == "Sweden" ~ "SWE",
                             country == "Norway" ~ "NOR")) %>%
  filter(jump_off_year == 2022) %>%
  dplyr::select(-jump_off_year)
  


# filling the skeleton ---------------------------------------------------

models_forecast <- rbind(naive_forecast, PPS_forecast, # ARCH_forecast,
                         LC_forecast, fill = T)
application_observed <- tfr %>% filter(Year %in% bp_years) %>%
                        mutate(observed = TFR) %>%
                        dplyr::select(-TFR)
application_forecast <- left_join(skeleton, models_forecast) 
application_forecast_observed <- rbind(application_observed, application_forecast, fill = T)


# create data set with LC and PPS forecast simulated paths of TFR---------

# load PPS simulations
FIN_sim <- readxl::read_xlsx(paths$input$PPS_sim_FIN)
PPS_sim_FIN <- FIN_sim %>% rename(sim = '...1') %>%
  gather(2:29, key = "Year", value = "TFR_sim") %>%
  mutate(sim = as.numeric(sim),
         Year = as.numeric(Year),
         model = "PPS",
         Country = "FIN") %>%
  arrange(sim)

SWE_sim <- readxl::read_xlsx(paths$input$PPS_sim_SWE)
PPS_sim_SWE <- SWE_sim %>% rename(sim = '...1') %>%
  gather(2:29, key = "Year", value = "TFR_sim") %>%
  mutate(sim = as.numeric(sim),
         Year = as.numeric(Year),
         model = "PPS",
         Country = "SWE") %>%
  arrange(sim)

NOR_sim <- readxl::read_xlsx(paths$input$PPS_sim_NOR)
PPS_sim_NOR <- NOR_sim %>% rename(sim = '...1') %>%
  gather(2:29, key = "Year", value = "TFR_sim") %>%
  mutate(sim = as.numeric(sim),
         Year = as.numeric(Year),
         model = "PPS",
         Country = "NOR") %>%
  arrange(sim)

DEN_sim <- readxl::read_xlsx(paths$input$PPS_sim_DEN)
PPS_sim_DEN <- DEN_sim %>% rename(sim = '...1') %>%
  gather(2:29, key = "Year", value = "TFR_sim") %>%
  mutate(sim = as.numeric(sim),
         Year = as.numeric(Year),
         model = "PPS",
         Country = "DEN") %>%
  arrange(sim)

PPS_sim <- rbind(PPS_sim_FIN, PPS_sim_NOR, PPS_sim_SWE, PPS_sim_DEN)

# combine data with LC simulations
forecast_sim <- do.call(rbind, LC_forecast_sim) %>%
  rename(TFR_sim = TFR) %>%
  rbind(PPS_sim) %>%
  mutate(Year = as.numeric(as.character(Year)),
         sim = as.numeric(sim))

# combine with observed TFR and median forecast
forecast_sim_obs <- application_observed %>%
  mutate(model = "observed") %>%
  rename(TFR = observed) %>%
  full_join(forecast_sim) %>%
  left_join(dplyr::select(application_forecast_observed, Country, Year, model, predicted))


# plot
ggplot(data = filter(forecast_sim_obs, Country == "FIN"),
       aes(x = Year, y = TFR, color = model)) +
  geom_point(color = "black") +
  geom_line(data = filter(forecast_sim_obs, Country == "FIN" & model == "LC"),
            aes(x = Year, y = TFR_sim, group = sim),
            alpha = 0.1) +
  geom_line(data = filter(forecast_sim_obs, Country == "FIN" & model == "LC"),
            aes(x = Year, y = predicted, color = model),
            size = 1) +
  geom_line(data = filter(forecast_sim_obs, Country == "FIN" & model == "PPS"),
            aes(x = Year, y = TFR_sim, group = sim),
            alpha = 0.1) +
  geom_line(data = filter(forecast_sim_obs, Country == "FIN" & model == "PPS"),
            aes(x = Year, y = predicted, color = model),
            size = 1) +
  ggtitle("Finnland")


# export application forecast data ---------------------------------------

saveRDS(application_forecast_observed, paths$output$application_forecast)
saveRDS(forecast_sim_obs, paths$output$forecast_sim)
