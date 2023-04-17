################################################################################
#PACKAGES, DIRECTORY AND DATA
################################################################################
rm(list=ls())

#packages
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/")

################################################################################
#SECWEPEMCTSIN (SHUSHWAP)
#A STEP-BY-STEP EXAMPLE
################################################################################
################################################################################
#Baseline population
################################################################################
#Data frame
baselinepop <- readRDS("1CensusData/ms_avg") %>% 
  filter(language=="Secwepemctsin", age!=100) %>%
  group_by(age) %>%
  summarise(speaker=round(sum(speaker))) 

#Figure 
data1 <- ggplot(baselinepop)+
  geom_col(aes(age, speaker))+
  coord_flip()+
  theme_bw()+
  ylab("Number of speakers in year 2014")+
  xlab("Age")+
  annotate("text", x=10, y=40, label="n = 493")+
  theme(plot.margin = margin(1,1,1,1, "cm"))

################################################################################
#Backcast
################################################################################
#Load fertility and mortality schedules
f <- readRDS("2DemSched/FertilityRest")[,4:94]
m <- readRDS("2DemSched/MortalityRest")[,4:94]

backcast_fct <- function(r){
  
  #Load starting population (add 1 per age group)
  p <- list(unlist(readRDS(paste("3StartPop/Secwepemctsin/sp", floor(runif(1, 1, 31)), sep="")))+1)
    
  #Simulation to check how many died in the last five years
  for (y in 1:5){
      
    p[[y]] <- p[[y]] + rbinom(102-y, p[[y]], m[1:(102-y), 6-y])
      
    p[[y+1]] <- p[[y]][-1]
    
    }
  
  #Take population five years ago  
  p <- list(p[[6]]-1)
    
  #Simulation to check how many children were born
  for (y in 1:5){
      
    #remove those who die
    p[[y]] <- p[[y]] - rbinom(95+y, p[[y]], m[, y])
      
    #find the number of newborns and add it to the next population
    p[[y+1]] <- c(sum(rbinom(95+y, p[[y]], f[, y])), p[[y]])
    
  }
  
  return(
    
    #Data frame  
    backcast <- bind_rows(
      lapply(seq(1, 96, 5), function(x) 
        data.frame(age=x-1, speaker=sum(p[[6]][x:(x+4)]), run=paste("Run #", r, sep="")))) %>%
      mutate(counterfactual=ifelse(age==0, "1", "0"),
             speaker=ifelse(is.na(speaker), 0, speaker))
    )
  
  }

backcast <- bind_rows(lapply(1:30, function(x) backcast_fct(x)))
backcast$run <- ifelse(backcast$run=="Run #30", "...run #30", backcast$run)    
backcast$run <- factor(backcast$run, levels=unique(backcast$run))

#Figure: backcast
a1 <- ggplot(backcast %>% filter(run=="Run #1" | run=="Run #2" | run=="...run #30"))+
  geom_col(aes(age, speaker, fill=counterfactual, group=counterfactual))+
  coord_flip()+
  theme_bw()+
  scale_fill_grey(start=0.7, end=0.3)+
  theme(legend.position = "none")+
  xlab("Age")+
  ylab("Number of speakers by age (grey) and\ntotal number of children (speakers and non-speakers; black)")+
  facet_wrap(~run, nrow=1)

#Figure: Counterfactual children
a2 <- ggplot(bind_rows(backcast) %>% filter(age==0) %>% mutate(speaker=round(speaker/2)*2))+
  geom_bar(aes(speaker), fill= grey(.3))+
  theme_bw()+
  xlab("Total number of children\n(speakers and non-speakers)")+
  scale_x_continuous(breaks=c(seq(12, 36, 4)))+
  scale_y_continuous(breaks=c(seq(0, 12, 2)))

################################################################################
#Probability of intergenerational transmission
################################################################################
#Figure
b <- ggplot(data.frame(PIT=readRDS("5IntTrans/all_dist_new")[[33]]))+
  stat_density(aes(PIT), geom = "area", fill= grey(.3))+
  theme_bw()+
  xlab("Probability of intergenerational transmission")

################################################################################
#Figure E: Simulation
################################################################################.
sim_fct <- function(r){
  
  #draw random starting population
  p <- readRDS(paste("3StartPop/Secwepemctsin/sp", floor(runif(1, 1, 31)), sep=""))
  
  #draw random value from the joint posterior distribution of intergenerational transmission
  it <- sample(readRDS("5IntTrans/all_dist_new")[[33]], 1)
  
  #estimate slope of change in it (depends on the number of people ages 20-40)
  s <- lm(c(it, 0) ~ c(sum(p[[1]][21:41]), 0))$coefficients[[2]]
  
  #Loop through all years between the base year and 2100
  for (y in 1:86){
    
    #remove those who die
    p[[y]] <- p[[y]] - rbinom(101, p[[y]], m[,y])
    
    #intergenerational transmission vector
    it <- c(rep(0, 15), unlist(lapply(5:39, function(x) sum(p[[y]][x:(x+20)])*s)), rep(0, 51))
    it <- replace(it, it>1, 1)
    
    #find the number of newborns and add it to the next population
    p[[y+1]] <- c(sum(rbinom(101, p[[y]], f[, y]*it)), p[[y]][-101])
    
    }
  
  return(
    bind_rows(
      lapply(seq(1, 96, 5), function(x) 
        data.frame(age=x-1, speaker=sum(p[[87]][x:(x+4)]), run=r)))
    )
  
  }
               
#Run simulation 1,000 times
result <- bind_rows(lapply(1:1000, function(x) sim_fct(x)))

#Adjust the run variable
result <- result %>% mutate(run=case_when(
  run<1000 ~ paste("Run #", run, sep=""),
  run==1000 ~ paste("...run #1,000")
))

#Specify levels
result$run <- factor(result$run, levels=unique(result$run))

#Create label to be put in the figure
result <- result %>% left_join(
  result %>% group_by(run) %>% summarise(n=paste("n =", sum(speaker)))
)

#Make figure for pyramids
c <- ggplot(result %>% filter(run=="Run #1" | run=="Run #2" | run=="...run #1,000"))+
  geom_col(aes(age, speaker))+
  coord_flip()+
  theme_bw()+
  facet_wrap(~run, nrow=1)+
  ylab("Number of speakers in year 2100")+
  scale_y_continuous(breaks=c(seq(0,12, 4)))+
  geom_text(aes(x = 10, y = 8, label = n))

#Check results
result %>% 
  group_by(run) %>% 
  summarise(speaker=sum(speaker)) %>% 
  pull(speaker) %>% 
  quantile(c(0.05, 0.5, .95)) %>%
  round()

result %>% 
  filter(age<50) %>%
  group_by(run) %>% 
  summarise(speaker=sum(speaker)) %>% 
  pull(speaker) %>% 
  quantile(c(0.05, 0.5, .95)) %>%
  round()

result %>% 
  filter(age<15) %>%
  group_by(run) %>% 
  summarise(speaker=sum(speaker)) %>% 
  pull(speaker) %>% 
  quantile(c(0.05, 0.5, .95)) %>%
  round()

################################################################################
#Put all steps in one figure
################################################################################
#Create empty figure to help arrange
empty <- ggplot()+theme_minimal()

ggarrange(data1, 
  ggarrange(
    ggarrange(a1, a2, labels = c("a", ""), widths = c(2,1)),
    ggarrange(empty, b, empty, labels= c("Data 2", "b", ""), nrow = 1, widths=c(2.3,5,1)),
    ggarrange(empty, c, labels = c("", "c"), ncol=2, widths = c(2,5)),
    ncol=1
  ),
  ncol=2, labels=c("Data 1",""), widths = c(2, 3))

ggsave("8Example/Fig8b.tiff", height = 6, width=12, dpi=1200)
