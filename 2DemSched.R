################################################################################
#1.PACKAGES 
################################################################################
rm(list=ls())

#packages
library(tidyverse)

#working directory
setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/2DemSched/")
setwd("C:/Users/micha/Documents/ProjIndigLangCan/2DemSched/")

#############################################################################################
#2. ESTABLISH DEMOGRAPHIC SCHEDULES
#############################################################################################
#2.1 Fertility ##############################################################################
frt <- read.csv("WPP2022_Fertility_by_Age1.csv")

#All except Michif
#select desired schedule
f1 <- frt %>% filter(Location=="Less developed regions", Variant=="Medium", Time>=2006, Time<=2099) %>% pull(ASFR)

#assign to matrix
f1 <- matrix(unlist(
  lapply(seq(1, length(f1), 35), 
         function(x) c(rep(0, 15), f1[x:(x+34)]/2000, rep(0, 51)))), ncol=94)
  
#Michif
#select desired schedule
f2 <- frt %>% filter(Location=="Northern America", Variant=="Medium", Time>=2006, Time<=2099) %>% pull(ASFR)

#assign to matrix
f2 <- matrix(unlist(
  lapply(seq(1, length(f2), 35), 
         function(x) c(rep(0, 15), f2[x:(x+34)]/2000, rep(0, 51)))), ncol=94)

#2.2 Mortality#################################################################################
#load data
mrt1 <- read.csv("WPP2022_Life_Table_Complete_Medium_Both_1950-2021.csv")
mrt2 <- read.csv("WPP2022_Life_Table_Complete_Medium_Both_2022-2100.csv")
mrt <- bind_rows(mrt1, mrt2)

#select desired schedule 
m1 <- mrt %>% filter(Location=="Asia", Time>=2006, Time<=2099) %>% pull(qx)
m1 <- matrix(m1, ncol=94)

m2 <- mrt %>% filter(Location=="Europe", Time>=2006, Time<=2099) %>% pull(qx)
m2 <- matrix(m2, ncol=94)

m3 <- mrt %>% filter(Location=="More developed regions", Time>=2006, Time<=2099) %>% pull(qx)
m3 <- matrix(m3, ncol=94)

#############################################################################################
#3. SAVE
#############################################################################################
#save matrices
saveRDS(f1, "FertilityRest")
saveRDS(f2, "FertilityMichif")
saveRDS(m1, "MortalityInuktitut")
saveRDS(m2, "MortalityRest")
saveRDS(m3, "MortalityMichif")
