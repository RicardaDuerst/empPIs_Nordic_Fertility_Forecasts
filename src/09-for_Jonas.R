# single country data sets for Jonas
# to showcase application for StMoMo for Lee-Carter forecasts

library(dplyr)
library(data.table)
setwd('.')

# ASFR, Denmark, application period input years
asfr <- fread("./tmp/00-ASFR.csv")
DEN_ASFR <- filter(asfr, Country == "DEN" & Year %in% c(1993:2022))
saveRDS(DEN_ASFR, 'tmp/09-DEN_ASFR.rds')

# Births and Exposures, Denmark, application period input years
DEN_births <- read.table("dat/DEN_Births.txt", skip = 2, header = T)
DEN_births$Age[DEN_births$Age=="12-"] <- 12 # make age variable numeric
DEN_births$Age[DEN_births$Age=="55+"] <- 55
DEN_births$Age <- as.numeric(DEN_births$Age)
DEN_births$Country <- "DEN" # add country variable
DEN_births <- rename(DEN_births, Births = Total)

DEN_exposure <- read.table("dat/DEN_Exposure.txt", skip = 2, header = T)
DEN_exposure$Age[DEN_exposure$Age=="55+"] <- 55
DEN_exposure$Age <- as.numeric(DEN_exposure$Age)
DEN_exposure$Country <- "DEN" # add country variable

DEN_B_E <- left_join(DEN_births, DEN_exposure)
DEN_B_E <- filter(DEN_B_E, Year %in% c(1993:2022))
saveRDS(DEN_B_E, 'tmp/09-DEN_Births_Exposures.rds')


# errors by cv series
tfr_cv_errors <- forecasting_error$error_series %>% dplyr::select(-starts_with(c("L", "U"))) %>% arrange(Country, model, cv_id)
saveRDS(tfr_cv_errors, 'tmp/09-tfr_cv_errors.rds')
