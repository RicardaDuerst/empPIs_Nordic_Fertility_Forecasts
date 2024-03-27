# Preparing the cross validation data series

# Init ------------------------------------------------------------

library(yaml)
library(dplyr)
library(data.table)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  asfr = 'tmp/00-ASFR.csv',
  tfr = 'tmp/00-TFR.csv',
  asfr_bo = 'tmp/00-ASFR_BO.csv',
  births_exposure = 'tmp/00-B_E.csv'
)
paths$output <- list(
  asfr_cv = 'tmp/01-asfr_cv.csv',
  tfr_cv = 'tmp/01-tfr_cv.csv',
  asfr_bo_cv = 'tmp/01-asfr_bo_cv.csv',
  B_E_cv = 'tmp/01-B_E_cv.csv'
)

# constants specific to this analysis
cnst <- within(list(), {
  # in years
  training_length = 30
  test_length = data.frame(Country = c("FIN", "SWE", "NOR", "DEN"),
                           length = c(30, 30, 15, 30))
  # list of countries
  cntr = c("FIN", "SWE", "NOR", "DEN")
})

# global functions
source(paths$input$global)

# Load time series of ASFR and TFR --------------------------------

asfr <- fread(paths$input$asfr)
tfr <- fread(paths$input$tfr)
B_E <- fread(paths$input$births_exposure)
#asfr_bo <- fread(paths$input$asfr_bo)


# Cut TFR data ----------------------------------------------------------

tfr_cv <- list()
for (i in 1:length(cnst$cntr)) { 
  tfr_cv[[i]] <- CutCVData(
    training_length = cnst$training_length,
    test_length = cnst$test_length[which(cnst$test_length$Country == cnst$cntr[i]),2],
    cv_data = filter(tfr, Country == cnst$cntr[i])
  )
}
names(tfr_cv)  <- cnst$cntr


# Cut ASFR data ---------------------------------------------------------

asfr_cv <- list()
for (i in 1:length(cnst$cntr)) {
  asfr_cv[[i]] <- CutCVData(
    training_length = cnst$training_length,
    test_length = cnst$test_length[which(cnst$test_length$Country == cnst$cntr[i]),2],
    cv_data = filter(asfr, Country == cnst$cntr[i])
  )
}
names(asfr_cv)  <- cnst$cntr


# Cut Births and Exposure data ---------------------------------------------

B_E_cv <- list()
for (i in 1:length(cnst$cntr)) {
  B_E_cv[[i]] <- CutCVData(
    training_length = cnst$training_length,
    test_length = cnst$test_length[which(cnst$test_length$Country == cnst$cntr[i]),2],
    cv_data = filter(B_E, Country == cnst$cntr[i])
  )
}
names(B_E_cv)  <- cnst$cntr


# # Cut ASFR birth order data ----------------------------------------------
# 
# asfr_bo_cv <- list()
# for (i in 1:length(cnst$cntr)) {
#   asfr_bo_cv[[i]] <- CutCVData(
#     training_length = cnst$training_length,
#     test_length = cnst$test_length[which(cnst$test_length$Country == cnst$cntr[i]),2],
#     cv_data = filter(asfr_bo, Country == cnst$cntr[i])
#   )
# }
# names(asfr_bo_cv)  <- cnst$cntr


# bind country files together ---------------------------------------------
asfr_cv <- do.call(rbind, asfr_cv) 
tfr_cv <- do.call(rbind, tfr_cv)
B_E_cv <- do.call(rbind, B_E_cv)
#asfr_bo_cv <- do.call(rbind, asfr_cv) 


# data summary ------------------------------------------------------------
id <- tfr_cv %>% group_by(Country) %>% distinct(cv_id)


# Export cross validation data sets ---------------------------------------

write.csv(asfr_cv, file = paths$output$asfr_cv, row.names = F)
write.csv(tfr_cv, file = paths$output$tfr_cv, row.names = F)
write.csv(B_E_cv, file = paths$output$B_E_cv, row.names = F)
#write.csv(asfr_bo_cv, file = paths$output$asfr_bo_cv, row.names = F)