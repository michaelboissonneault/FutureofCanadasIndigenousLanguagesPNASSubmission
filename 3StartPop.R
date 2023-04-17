############################################################################################
#PACKAGES, DIRECTORY AND DATA
############################################################################################
rm(list=ls())

#packages
library(tidyverse)

#Working directory
setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/")
setwd("C:/Users/micha/Documents/ProjIndigLangCan/")

#get populations by age and language
ms_avg <- readRDS("1CensusData/ms_avg")

#vector with language names (in order of population size) 
languages <- ms_avg %>% 
  group_by(language) %>% 
  summarise(speaker=sum(speaker)) %>% 
  arrange(speaker) %>% 
  pull(language)

#collapse speakers from the two sources together
ms_avg <- ms_avg %>% 
  group_by(family, language, age) %>% 
  summarise(speaker=sum(speaker))

############################################################################################
#ESTIMATION OF THE STARTING POPULATIONS
############################################################################################
#Function for the starting populations
spfct <- function(l, r){
  
  #select speaker numbers by age, year 2014
  spk_floor <- ms_avg %>% filter(language==l) %>% pull(speaker) %>% floor() 
  spk <- spk_floor + rbinom(21, 1, ms_avg %>% filter(language==l) %>% pull(speaker) - spk_floor)

  #create directory to save results
  dir.create(paste("3StartPop/", l, sep=""))
  
  #random ages falling within the five-year period
  p <- unlist(lapply(1:21, function(x) floor(runif(spk[x], x*5-5, x*5))))
  p <- sapply(0:100, function(x) length(p[which(p==x)]))
  
  #save
  saveRDS(list(p), paste("3StartPop/", l, "/sp", r, sep=""))
  
  }

#Run function
lapply(languages, function(l) lapply(1:30, function(r) spfct(l, r)))

