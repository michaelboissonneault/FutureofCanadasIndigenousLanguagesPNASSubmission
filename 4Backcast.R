################################################################################
#1.PACKAGES & LOAD DATA
################################################################################
rm(list=ls())

#packages
library(tidyverse)

#working directory
setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/")
#setwd("C:/Users/micha/Documents/ProjIndigLangCan/")

#Load vector of language names
languages <- readRDS("1CensusData/ms_avg") %>% 
  group_by(language) %>% 
  summarise(speaker=sum(speaker)) %>% 
  arrange(speaker) %>% 
  pull(language)

#Load fertility and mortality schedules
f1 <- readRDS("2DemSched/FertilityRest")
f2 <- readRDS("2DemSched/FertilityMichif")
m1 <- readRDS("2DemSched/MortalityInuktitut")
m2 <- readRDS("2DemSched/MortalityRest")
m3 <- readRDS("2DemSched/MortalityMichif")

###################################################################################
#BACKCAST
###################################################################################
#Specify the backcast function
backcastfct <- function(l, r){  

  #choose mortality and fertility schedules
  m <- if(l=="Inuktitut") {m1[,4:94]} else if(l=="Michif") {m3[,4:94]} else if(l=="Cayuga"){m2} else if(l=="Comox"){m2[,6:94]} else {m2[,4:94]} 
  f <- if(l=="Michif") {f2[,4:94]} else if(l=="Cayuga"){f1} else if(l=="Comox"){f1[, 6:94]} else {f1[ , 4:94]}
  
  #Load starting population (add 1 per age group to account for people who might have been there in the past but died)
  p <- list(unlist(readRDS(paste("3StartPop/", l ,"/sp", r, sep="")))+1)

  #Simulation to check how many died in the last five years
  for (y in 1:5){
    
    p[[y]] <- p[[y]] + rbinom(102-y, p[[y]], m[1:(102-y), 6-y])
    
    p[[y+1]] <- p[[y]][-1]
    
  }
  
  p <- list(p[[6]]-1)
  
  #simulation based on the reconstructed population five years prior to baseline
  for (y in 1:5){
    
    #remove those who die
    p[[y]] <- p[[y]] - rbinom(95+y, p[[y]], m[, y])
    
    #find the number of newborns and add it to the next population
    p[[y+1]] <- c(sum(rbinom(95+y, p[[y]], f[, y])), p[[y]])
    
  }
  
  return(sum(p[[6]][1:5]))
  
}

#Run function
backcastresult <- lapply(languages, function(l) sapply(1:30, function(r) backcastfct(l, r)))

#Save results
lapply(1:length(languages), function(l) saveRDS(backcastresult[[l]], paste("4Backcast/", languages[l], sep="")))

####################################################################################
#TESTING WHETHER BACKCAST + FORECAST = STARTING POPULATION (ANSWER IS YES)
####################################################################################
#Use same function, change output for people alive
#Specify the backcast function
test <- function(l, r){  
  
  #choose mortality and fertility schedules
  m <- if(l=="Inuktitut") {m1[,4:94]} else if(l=="Michif") {m3[,4:94]} else if(l=="Cayuga"){m2} else {m2[,4:94]} 
  f <- if(l=="Michif") {f2[,4:94]} else if(l=="Cayuga"){f1} else {f1[,4:94]}
  
  #Load starting population of childbearing age (15-55)
  p <- readRDS(paste("Results/Parameters/", l ,"/sp", r, sep=""))
  p <- list(p[which(2014-p>=16 & 2014-p<=55)])
  
  for (y in 1:5){
    
    pp <- c(p[[y]], ((2014-y)-16):((2014-y)-55))
    
    p[[y+1]] <- c(p[[y]], pp[which(ifelse(runif(length(pp))<m[(2014-y) - pp, (6-y)], 1, 0)==1)])
    
  }
  
  p <- list(p[[6]])
  
  baseyear <- ifelse(l=="Cayuga", 2005, 2008)
  
  nb <- list()
  
  for (y in 1:5){
    
    #remove those who die
    p[[y+1]] <- p[[y]][which(ifelse(runif(length(p[[y]])) < m[(y + baseyear) - p[[y]], y], 1, 0)==0)]
    
    #count the number of newborns
    nb[[y]] <- sum(ifelse(runif(length(p[[y]])) < f[(y + baseyear) - p[[y]], y], 1, 0))
    
  }
  
  return(p[[6]])
  
}

#Run function
testresult <- lapply(languages, function(l) lapply(1:100, function(r) test(l, r)))

#find equivalent measures in true populations
realpops <- lapply(languages, function(l) lapply(1:100, function(r) readRDS(paste("Results/Parameters/", l ,"/sp", r, sep=""))))
realpops <- lapply(1:43, function(l) lapply(1:100, function(r) realpops[[l]][[r]][which(2014-realpops[[l]][[r]]>=16 & 2014-realpops[[l]][[r]]<=55)]))

#Mean
tmean <- unlist(lapply(1:43, function(x) mean(unlist(testresult[[x]]))))
rmean <- unlist(lapply(1:43, function(x) mean(unlist(realpops[[x]]))))

#Size
tn <- unlist(lapply(1:43, function(x) length(unlist(testresult[[x]]))))/100
rn <- unlist(lapply(1:43, function(x) length(unlist(realpops[[x]]))))/100

#Min 
tmin <- unlist(lapply(1:43, function(x) min(unlist(testresult[[x]]))))
rmin <- unlist(lapply(1:43, function(x) min(unlist(realpops[[x]]))))

#Max
tmax <- unlist(lapply(1:43, function(x) max(unlist(testresult[[x]]))))
rmax <- unlist(lapply(1:43, function(x) max(unlist(realpops[[x]]))))

#data frame
plottestresults <- data.frame(language=c(languages, languages), mean=c(tmean, rmean), size=c(tn, rn), min=c(tmin, rmin), max=c(tmax, rmax), population=rep(c("backcast", "real"), length(languages)) )

#plot
ggplot(plottestresults)+
  geom_point(aes(log(size), language, color=population, group=population))

