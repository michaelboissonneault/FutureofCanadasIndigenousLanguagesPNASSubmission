############################################################################################
#PACKAGES, DIRECTORY AND DATA
############################################################################################
rm(list=ls())

#packages
library(rstan)
library(tidyverse)

#setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/")
setwd("C:/Users/micha/Documents/ProjIndigLangCan/")

#vector with language names 
#(excluding Atikamekw because it is the only language for which the number of children in the backcast is constantly lower than in the data)
#(we do Atikamekw separately below)
languages <- readRDS("1CensusData/ms_avg") %>% 
  filter(language!="Atikamekw") %>%
  group_by(language) %>% 
  summarise(speaker=sum(speaker)) %>% 
  arrange(speaker) %>% 
  pull(language)

#Create directory for saving results
#dir.create("5IntTrans")

############################################################################################
#ESTIMATION OF JOINT POSTERIOR DISTRIBUTIONS FOR THE LEVEL OF INTERGENERATIONAL TRANSMISSION
############################################################################################
#Stan model
stanmodelcode <- "
  data {            // data block for data declarations
    int<lower=0> N;
    int y[N];
  }
  parameters {      // parameter block for parameter declarations
    real<lower=0,upper=1> theta;
  }
  model {  // model block: priors for parameters and model for data
    theta ~ beta(1,1);  // not really needed since it's uniform (the default)
    y ~ bernoulli(theta); // y is a vector but this works
  }
  "

#function for estimating and saving the distributions
itfct <- function(l, r){

  #Get number of children ages 0-4 
  cc <- sum(unlist(readRDS(paste("3StartPop/", l, "/sp", r, sep="")))[1:5])
  
  #Get total number of children from backcast
  bac <- readRDS(paste("4Backcast/", l, sep=""))[r]
  
  #bac should be at least as great as cc and have a value of at least 2
  bac <- ifelse(cc > bac, cc, bac)
  bac <- ifelse(bac<2, 2, bac)
    
  #put information in lists (one per run) including the value for N and a vector of successes
  mat <- list(N = bac, y = c(rep(1, cc), rep(0, bac - cc)))
  
  #create directory to save results
  dir.create(paste("5IntTrans/", l, sep=""))
  
  #estimate stan models sample from distribution
  saveRDS(rstan::extract(rstan::stan(model_code = stanmodelcode, 
                                     model_name = "Bernoulli Beta(1,1)",
                                     iter = 750,
                                     warmup = 300,
                                     chains = 2,
                                     data = mat))$theta,
          paste("5IntTrans/", l, "/pd", r, sep=""))
  
  }

#run function (takes about 30-40 minutes per language)
lapply(languages[38:43], function(l) lapply(1:30, function(r) itfct(l, r)))

lapply(8:30, function(r) itfct("Blackfoot", r))

#make joint distribution from the 30 post distributions
singled <- lapply(languages, function(l) unlist(lapply(1:30, function(r) readRDS(paste("5IntTrans/", l, "/pd", r, sep="")))))

#Atikamekw's pd#########################################################################################################
#get children counts in data
cc <- lapply(1:30, function(r)
  unlist(readRDS(paste("3StartPop/Atikamekw/sp", r, sep="")))) 

cc <- lapply(1:30, function(r) 
  sum(cc[[r]][1:5]))

#Atikamekw's number of children speakers is either 745 or 746
#We run both once
#MIN (745)
#put information in list
mat <- list(N = min(unlist(cc)), y = rep(1, min(unlist(cc))))
  
#estimate stan model, save distribution
atik1 <- rstan::extract(rstan::stan(model_code = stanmodelcode, 
                                   model_name = "Bernoulli Beta(1,1)",
                                   iter = 750,
                                   warmup = 300,
                                   chains = 2,
                                   data = mat))$theta

#MAX (746)
#put information in list
mat <- list(N = max(unlist(cc)), y = rep(1, max(unlist(cc))))

#estimate stan model, save distribution
atik2 <- rstan::extract(rstan::stan(model_code = stanmodelcode, 
                                    model_name = "Bernoulli Beta(1,1)",
                                    iter = 750,
                                    warmup = 300,
                                    chains = 2,
                                    data = mat))$theta

#Put all distributions together (pay attention to order)
all_dist <- c(singled[1:36], list(c(atik1, atik2)), singled[37:43])

#save all distributions
saveRDS(all_dist, "5IntTrans/all_dist_new")

