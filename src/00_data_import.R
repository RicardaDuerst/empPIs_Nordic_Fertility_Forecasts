# Data import

# date of data download: 09.11.2023
# source: Human Fertility Database
# files: Age-specific fertility rates by year and age, all birth orders combined, period data
#        Age-specific fertility rates by year, age and birth order, period data

# author: Ricarda Duerst


# Init ------------------------------------------------------------

library(dplyr)

setwd('.')

cntr <- c("FIN", "SWE", "NOR", "DEN") # vector of countries


# load asfr files by country --------------------------------------

asfr <- list()

for (i in 1:length(cntr)) {
  asfr[[i]] <- read.table(paste("dat/", cntr[i], "_ASFR.txt", sep =""),
                          skip = 2, header = T) 

  asfr[[i]]$Age[asfr[[i]]$Age=="12-"] <- 12 # make age variable numeric
  asfr[[i]]$Age[asfr[[i]]$Age=="55+"] <- 55
  asfr[[i]]$Age <- as.numeric(asfr[[i]]$Age)
  asfr[[i]]$Country <- cntr[i] # add country variable
}
names(asfr) <- cntr

# # by birth order
# asfr_bo <- list()
# 
# for (i in 1:length(cntr)) {
#   asfr_bo[[i]] <- read.table(paste("dat/", cntr[i], "_ASFR_BO.txt", sep =""),
#                           skip = 2, header = T) 
#   
#   asfr_bo[[i]]$Age[asfr_bo[[i]]$Age=="12-"] <- 12 # make age variable numeric
#   asfr_bo[[i]]$Age[asfr_bo[[i]]$Age=="55+"] <- 55
#   asfr_bo[[i]]$Age <- as.numeric(asfr_bo[[i]]$Age)
#   asfr_bo[[i]]$Country <- cntr[i] # add country variable
# }
# names(asfr_bo) <- cntr


# calculate total fertility rate ----------------------------------

tfr <- list()

for (i in 1:length(cntr)) {
  tfr[[i]] <- asfr[[i]] %>% group_by(Year) %>% summarise(TFR = sum(ASFR))
  tfr[[i]]$Country <- cntr[i] # add country variable
}
names(tfr) <- cntr

# bind country files together
asfr <- do.call(rbind, asfr) 
tfr <- do.call(rbind, tfr)
# asfr_bo <- do.call(rbind, asfr_bo) 

# reorder columns
asfr <- asfr[,c(4,1,2,3)] 
tfr <- tfr[,c(3,1,2)] 
# asfr_bo <- asfr_bo[,c(9,1,2,3,4,5,6,7,8)] 


# load Births and Exposure data by country ------------------------

births <- list()

for (i in 1:length(cntr)) {
  births[[i]] <- read.table(paste("dat/", cntr[i], "_Births.txt", sep =""),
                            skip = 2, header = T)
  
  births[[i]]$Age[births[[i]]$Age=="12-"] <- 12 # make age variable numeric
  births[[i]]$Age[births[[i]]$Age=="55+"] <- 55
  births[[i]]$Age <- as.numeric(births[[i]]$Age)
  births[[i]]$Country <- cntr[[i]]
  births[[i]] <- births[[i]] %>% rename(Births = Total)
}
names(births) <- cntr

exposure <- list()

for (i in 1:length(cntr)) {
  exposure[[i]] <- read.table(paste("dat/", cntr[i], "_Exposure.txt", sep =""),
                            skip = 2, header = T)
  
  exposure[[i]]$Age[exposure[[i]]$Age=="12-"] <- 12 # make age variable numeric
  exposure[[i]]$Age[exposure[[i]]$Age=="55+"] <- 55
  exposure[[i]]$Age <- as.numeric(exposure[[i]]$Age)
  exposure[[i]]$Country <- cntr[[i]]
}
names(exposure) <- cntr

# bind country files together
births <- do.call(rbind, births) 
exposure <- do.call(rbind, exposure)

# combine births and exposure data
B_E <- left_join(births, exposure)


# export data -----------------------------------------------------

write.csv(asfr, file = "./tmp/00-ASFR.csv", row.names = F) 
write.csv(tfr, file = "./tmp/00-TFR.csv", row.names = F)
write.csv(B_E, file = "./tmp/00-B_E.csv", row.names = F)

# write.csv(asfr_bo, file = "./tmp/00-ASFR_BO.csv", row.names = F) 
