################################################################################
#PACKAGES, DIRECTORY AND DATA
################################################################################
rm(list=ls())

#packages
library(tidyverse)

#specify directory
setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/")

#specify languages to run simulations on
languages <- readRDS("1CensusData/ms_avg") %>% 
  group_by(language) %>% 
  summarise(speaker=sum(speaker)) %>% 
  arrange(speaker) %>% 
  pull(language)

#intergenerational transmission
it <- readRDS("5IntTrans/all_dist_new")

#starting populations
sp <- lapply(languages, function(l) lapply(1:30, function(r) readRDS(paste("3StartPop/", l, "/sp", r, sep=""))))

############################################################################################
#FORECASTS
############################################################################################
#function for the simulation####################################################
sim <- function(l){
  
  #create matrices to store results
  allages <- matrix(NA, ncol=1000, nrow=2100-baseyear)
  parents <- matrix(NA, ncol=1000, nrow=2100-baseyear)
  children <- matrix(NA, ncol=1000, nrow=2100-baseyear)
  
  #starting populations specific to language l 
  spl <- sp[[which(languages==l)]]
    
  #intergenerational transmission specific to language l
  itl <- it[[which(languages==l)]]
  
  #Loop to repeat simulation 1,000 times
  for (r in 1:1000){
    
    #draw random starting population
    p <- spl[[floor(runif(1, 1, 31))]]
  
    #draw random value from the joint posterior distribution of intergenerational transmission
    it <- sample(itl, 1)
  
    #estimate slope of change in it (depends on the number of people ages 20-40)
    s <- it / sum(p[[1]][21:41])
    s <- ifelse(is.na(s), 0, s)
  
    #Loop through all years between the base year and 2100
    for (y in 1:(2099-baseyear)){
    
      #remove those who die
      p[[y+1]] <- p[[y]] - rbinom(101, p[[y]], m[,y])
        
      #intergenerational transmission vector
      it <- c(rep(0, 15), unlist(lapply(5:39, function(x) sum(p[[y+1]][x:(x+20)])*s)), rep(0, 51))
      it <- replace(it, it>1, 1)
    
      #find the number of newborns 
      p[[y+1]] <- c(sum(rbinom(101, p[[y+1]], f[, y]*it)), p[[y+1]][-101])
      
    }
    
    #Put results for this run in the matrices
    allages[ , r] <- sapply(1:length(p), function(y) sum(p[[y]]))
    parents[ , r] <- sapply(1:length(p), function(y) sum(p[[y]][1:50]))
    children[ , r] <- sapply(1:length(p), function(y) sum(p[[y]][1:15]))
    
  }
  
  #save results (could probably gain speed by saving only one matrix)
  saveRDS(allages, paste("6Forecast/Allages/", l, sep=""))
  saveRDS(parents, paste("6Forecast/Parents/", l, sep=""))
  saveRDS(children, paste("6Forecast/Children/", l, sep=""))
  
  }

#CAYUGA#########################################################################
#specify mortality and fertility matrices
f <- readRDS("2DemSched/FertilityRest")[,6:94]
m <- readRDS("2DemSched/MortalityRest")[,6:94]

#specify base year
baseyear <- 2010

#run function
sim("Cayuga")

#COMOX##########################################################################
#specify mortality and fertility matrices
f <- readRDS("2DemSched/FertilityRest")[,11:94]
m <- readRDS("2DemSched/MortalityRest")[,11:94]

#specify base year
baseyear <- 2015

#run function
sim("Comox")

#INUKTITUT & INUVIALUKTUN#######################################################
#specify mortality and fertility matrices
f <- readRDS("2DemSched/FertilityRest")[,9:94]
m <- readRDS("2DemSched/MortalityInuktitut")[,9:94]

#specify base year
baseyear <- 2013

#run function
sim("Inuktitut")
sim("Inuvialuktun")

#MICHIF#########################################################################
#specify mortality and fertility matrices
f <- readRDS("2DemSched/FertilityMichif")[,9:94]
m <- readRDS("2DemSched/MortalityMichif")[,9:94]

#run function
sim("Michif")

#OTHER LANGUAGES################################################################
#specify mortality and fertility matrices
f <- readRDS("2DemSched/FertilityRest")[,9:94]
m <- readRDS("2DemSched/MortalityRest")[,9:94]

#run function
lapply(
  languages[! languages %in% c("Comox", "Michif", "Inuktitut", "Cayuga", "Inuvialuktun")], 
       function(l) sim(l)
  )
