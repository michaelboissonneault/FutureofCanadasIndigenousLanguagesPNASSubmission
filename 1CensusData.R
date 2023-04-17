################################################################################
#IN THIS DO FILE: 
################################################################################
#1. PACKAGES 

#2. LOAD AND CLEAN DATA ON SPEAKER NUMBERS
#2.1 Year 2016
#2.2 Year 2011

#3. ATTRIBUTE NON-ATTRIBUTED SPEAKERS
#3.1 Year 2016
#3.2 Year 2011

#4. EVALUATE DATA 

################################################################################
#1.PACKAGES 
################################################################################
rm(list=ls())

#packages
library(tidyverse)
library(lubridate)
library(openxlsx)

#There should be a folder (called here 1CensusData) that contains the census data on speakers by groups of 5 years of age

#set working directory
setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/1CensusData/")
#setwd("C:/Users/micha/Documents/ProjIndigLangCan/1CensusData/")

################################################################################
#2. LOAD AND CLEAN DATA ON SPEAKER NUMBERS 
################################################################################
#2.1 Year 2016##################################################################
#load data 
ms16 <- read.csv("indigenousmothertongue2016.csv")

#take part of df that is about indigenous languages
ms16 <- ms16[8:89,]

#pivot longer
ms16 <- pivot_longer(ms16, Total...Age:X100.years.and.over, names_to="age", values_to="speaker")

#remove variable total 
ms16 <- filter(ms16,age!="Total...Age")

#create new age variable
ms16$age <- rep(seq(0,100,5), length(ms16$Mother.tongue)/21)

#rename mother tongue to language
ms16 <- rename(ms16, language=Mother.tongue)

#remove trailing spaces at beginning of language names
ms16$language <- trimws(ms16$language)

#save residual categories to other df
family16 <- filter(ms16, str_detect(language,", n.o.s."))

#remove the unncessary information in the family df
family16$language <- str_remove(family16$language, " languages, n.o.s.")
family16$language <- str_remove(family16$language, ", n.o.s.")

#Cree, not otherwise specified
cree16 <- filter(family16, language=="Cree")

#Slavey, not otherwise specified
slavey16 <- filter(family16, language=="Slavey")

#Aboriginal, not otherwise specified
aboriginal16 <- filter(family16, language=="Aboriginal")

#remove sums of languages in same family/dialect group 
ms16 <- filter(ms16, !str_detect(language,"languages") & !(str_detect(language,", n.o.s.") | str_detect(language,", n.i.e.")))

#add year
ms16$year <- 2016

#2.1 Year 2011##################################################################
#load data 
ms11 <- read.csv("indigenousmothertongue2011.csv")

#take part of df that is about indigenous languages
ms11 <- ms11[7:80,]

#pivot longer
ms11 <- pivot_longer(ms11,Total...Age:X100.years.and.over,names_to="age",values_to="speaker")

#remove variable total
ms11 <- filter(ms11,age!="Total...Age")

#create new age variable
ms11$age <- rep(seq(0,100,5),length(ms11$Mother.Tongue)/21)

#rename mother tongue to language
ms11 <- rename(ms11,language=Mother.Tongue)

#remove trailing spaces at beginning of language names
ms11$language <- trimws(ms11$language)

#save residual categories to other df
family11 <- filter(ms11, str_detect(language,", n.o.s."))

#remove the unnecessary information in the family df
family11$language <- str_remove(family11$language, " languages, n.o.s.")
family11$language <- str_remove(family11$language, ", n.o.s.")

#Cree, not otherwise specified
cree11 <- filter(family11, language=="Cree")

#Slavey, not otherwise specified
slavey11 <- filter(family11, language=="Slavey")

#Athapaskan = Athabaskan
family11$language <- ifelse(family11$language=="Athapaskan", "Athabaskan", family11$language)

#remove sums of languages in same family/dialect group 
ms11 <- filter(ms11, !str_detect(language,"languages") & !(str_detect(language,", n.o.s.") | str_detect(language,", n.i.e.")))

#add year
ms11$year <- 2011

#########################################################################################
#3. ATTRIBUTE NON-ATTRIBUTED SPEAKERS
#########################################################################################
#3.1 Year 2016############################################################################
#Harmonize language names
ms16 <- ms16 %>% 
  mutate(language=case_when(
    language=="Malecite" ~ "Wolastoqewi",
    language=="Algonquin" ~ "Anicinabemowin",
    language=="Montagnais (Innu)" ~ "Innu",
    language=="Kutenai" ~ "Ktunaxa",
    language=="South Slavey" ~ "Deh Gah Ghotie Zhatie",
    language=="North Slavey (Hare)" ~ "Satuotine Yati",
    language=="Carrier" ~ "Dakelh",
    language=="Beaver" ~ "Dane-zaa",
    language=="Dene" ~ "Denesuline",
    language=="Dogrib (Tlicho)" ~ "Dogrib",
    language=="Sekani" ~ "Tse'khene",
    language=="Chilcotin" ~ "Tsilhqot'in",
    language=="Sarsi (Sarcee)" ~ "Tsuu T'ina",
    language=="Babine (Wetsuwet'en)" ~ "Wetsuwet'en-Babine",
    language=="Thompson (Ntlakapamux)" ~ "Ntlakapamux",
    language=="Shuswap (Secwepemctsin)" ~ "Secwepemctsin",
    language=="Gitxsan (Gitksan)" ~ "Gitxsan",
    language=="Kwakiutl (Kwak'wala)" ~ "Kwak'wala",
    language=="Kaska (Nahani)" ~ "Kaska",
    language=="Nuu-chah-nulth (Nootka)" ~ "Nuu-chah-nulth",
    language=="Ottawa (Odawa)" ~ "Ottawa", 
    language=="Inuinnaqtun (Inuvialuktun)" ~ "Inuinnaqtun", 
    TRUE ~ as.character(language))) 

#add information on family
ms16$family <- rep(c(rep("Algonquian", 16), rep("Athabaskan", 15), "Isolate", rep("Inuit", 2), rep("Iroquoian", 3),
                     "Isolate", "Mixed", rep("Salish", 8), rep("Siouan", 2), "Isolate", rep("Tsimshian", 3), rep("Wakashan", 4)),
                   each=21)

#add information on dialect group
ms16 <- ms16 %>% 
  mutate(dialectgroup = case_when(
    language=="Satuotine Yati" | language=="Deh Gah Ghotie Zhatie" ~ "Slavey",
    language=="Moose Cree" | language=="Northern East Cree" | language=="Plains Cree" | 
      language=="Southern East Cree" | language=="Swampy Cree" | language=="Woods Cree" ~ "Cree"))

#total number of speakers for all languages and each dialect group
total <- sum(ms16$speaker)
totdialectgroup <- ms16 %>% group_by(dialectgroup) %>% summarise(totgroup=sum(speaker)) %>% na.omit()

#proportion of each language in the whole, its family, its dialect group
prop16 <- ms16 %>% 
  group_by(language, dialectgroup) %>% 
  summarise(totlanguage=sum(speaker)) %>% 
  left_join(totdialectgroup) %>%
  mutate(total=total,
         propgroup=totlanguage/totgroup,
         proptotal=totlanguage/total)

#join proportions in the dialect group, family, and all languages to counts by age 
ms16 <- ms16 %>% 
  left_join(
    prop16 %>%
      ungroup() %>% 
      select(language, dialectgroup, propgroup)) %>%
  left_join(
    prop16 %>% 
      ungroup() %>%
      select(language, proptotal))
  
#add non-attributed speakers
ms16 <- ms16 %>% 
  left_join(bind_rows(cree16, slavey16) %>% rename(added_group=speaker), by=c("age", "dialectgroup"="language")) %>%
  left_join(aboriginal16 %>% rename(added_total=speaker) %>% select(-language), by="age")

#calculate additional speakers and drop unnecessary columns
ms16 <- ms16 %>% 
  mutate(propgroup = ifelse(is.na(propgroup), 0, propgroup), 
         added_group = ifelse(is.na(added_group), 0, added_group),
         added_speaker = propgroup*added_group + proptotal*added_total) %>%
  select(language, age, speaker, year, family, added_speaker)

#3.2 Year 2011######################################################################
#Harmonize language names
ms11 <- ms11 %>% mutate(language=case_when(
  language=="Malecite" ~ "Wolastoqewi",
  language=="Algonquin" ~ "Anicinabemowin",
  language=="Innu/Montagnais" ~ "Innu",
  language=="Kutenai" ~ "Ktunaxa",
  language=="South Slavey" ~ "Deh Gah Ghotie Zhatie",
  language=="North Slavey (Hare)" ~ "Satuotine Yati",
  language=="Carrier" ~ "Dakelh",
  language=="Beaver" ~ "Dane-zaa",
  language=="Dene" ~ "Denesuline",
  language=="Tlicho (Dogrib)" ~ "Dogrib",
  language=="Sekani" ~ "Tse'khene",
  language=="Chilcotin" ~ "Tsilhqot'in",
  language=="Sarcee" ~ "Tsuu T'ina",
  language=="Wetsuweten" ~ "Wetsuwet'en-Babine",
  language=="Thompson (Ntlakapamux)" ~ "Ntlakapamux",
  language=="Shuswap (Secwepemctsin)" ~ "Secwepemctsin",
  language=="Gitksan" ~ "Gitxsan",
  language=="Kaska (Nahani)" ~ "Kaska",
  language=="Kwakiutl (Kwak'wala)" ~ "Kwak'wala",
  language=="Nootka (Nuu-chah-nulth)" ~ "Nuu-chah-nulth",
  TRUE ~ as.character(language))) 

#Add family 
ms11$family <- rep(c(rep("Algonquian", 12), "Mixed", rep("Athabaskan", 15), "Isolate", rep("Iroquoian", 3), "Isolate",  
                     rep("Salish", 7), rep("Siouan", 2), "Isolate", rep("Tsimshian", 3), rep("Wakashan", 4), rep("Inuit", 3)),
                   each=21)

#Add dialect group
ms11 <- ms11 %>% mutate(dialectgroup = case_when(
  language=="Swampy Cree" | language=="Plains Cree" | language=="Woods Cree" ~ "Cree",
  language=="Satuotine Yati" | language=="Deh Gah Ghotie Zhatie" ~ "Slavey",
  language=="Northern Tutchone" | language=="Southern Tutchone" ~ "Tutchone"
))

#total number of speakers for all languages and each dialect group
total <- sum(ms11$speaker)
totdialectgroup <- ms11 %>% group_by(dialectgroup) %>% summarise(totgroup=sum(speaker)) %>% na.omit()

#proportion of each language in the whole, its family, its dialect group
prop16 <- ms11 %>% 
  group_by(language, dialectgroup) %>% 
  summarise(totlanguage=sum(speaker)) %>% 
  left_join(totdialectgroup) %>%
  mutate(total=total,
         propgroup=totlanguage/totgroup,
         proptotal=totlanguage/total)

#join proportions in the dialect group, family, and all languages to counts by age 
ms11 <- ms11 %>% 
  left_join(
    prop16 %>%
      ungroup() %>% 
      select(language, dialectgroup, propgroup)) %>%
  left_join(
    prop16 %>% 
      ungroup() %>%
      select(language, proptotal))

#add non-attributed speakers
ms11 <- ms11 %>% 
  left_join(bind_rows(cree16, slavey16) %>% rename(added_group=speaker), by=c("age", "dialectgroup"="language")) %>%
  left_join(aboriginal16 %>% rename(added_total=speaker) %>% select(-language), by="age")

#calculate additional speakers and drop unnecessary columns
ms11 <- ms11 %>% 
  mutate(propgroup = ifelse(is.na(propgroup), 0, propgroup), 
         added_group = ifelse(is.na(added_group), 0, added_group),
         added_speaker = propgroup*added_group + proptotal*added_total) %>%
  select(language, age, speaker, year, family, added_speaker)

################################################################################
#4. EVALUATE DATA 
################################################################################
#Put two years in one data frame, pivot longer according to source of speaker number
ms <- bind_rows(ms11, ms16) %>% pivot_longer(c(speaker, added_speaker), names_to = "source", values_to = "speaker")

#visualize pyramids
ggplot(ms %>% filter(family=="Algonquian"))+
  geom_col(aes(age, speaker, color=source, group=source, fill=source))+
  facet_grid(year ~ language, scales="free")+
  coord_flip()

#data to check the correlation between the two years and identify eventual problems
dat <- ms %>%
  group_by(language, year) %>% 
  summarise(speaker=log(sum(speaker))) %>% 
  pivot_wider(names_from = year, values_from = speaker) %>% 
  na.omit()

#visualize correlation between the two years
ggplot(dat, aes(`2011`, `2016`))+
  geom_point()+
  geom_segment(aes(x=3, xend=11, y=3, yend=11))+
  theme_bw()

#linear model
summary(lm(`2011` ~ `2016`, data= dat))

#points that are fartest off the line
data.frame(language=dat$language, 
           residuals=summary(lm(`2011` ~ `2016`, data= dat))$residuals) %>%
  arrange(-(residuals^2))

#Exclude languages mainly spoken in the US
ms <- ms %>% filter(language!="Tlingit", 
                    language!="Mohawk", 
                    language!="Oneida", 
                    language!="Okanagan", 
                    language!="Dakota", 
                    language!="Gwich'in")

#Merge Cree languages
#Merge Ottawa and Ojibway
#Merge Inuinnaqtun and Inuvialuktun
#Merge Innu and Naskapi
ms <- ms %>% 
  mutate(language=case_when(
    language=="Moose Cree" | language=="Northern East Cree" | language=="Plains Cree" | 
      language=="Southern East Cree" | language=="Swampy Cree" | language=="Woods Cree" ~ "Cree",
    language=="Ottawa" ~ "Ojibway", 
    language=="Inuinnaqtun" ~ "Inuvialuktun",
    language=="Naskapi" ~ "Innu-Naskapi",
    language=="Innu" ~ "Innu-Naskapi",
    TRUE ~ as.character(language))) %>% 
  group_by(family, language, age, year, source) %>% 
  summarise(speaker=sum(speaker))

#new data frame
dat <- ms %>%
  filter(language!="Cayuga") %>%
  group_by(language, year) %>% 
  summarise(speaker=log(sum(speaker))) %>% 
  pivot_wider(names_from = year, values_from = speaker) %>% 
  na.omit()

#visualize correlation again
ggplot(dat, aes(`2011`, `2016`))+
  geom_point()+
  geom_segment(aes(x=3, xend=11, y=3, yend=11))+
  theme_bw()

#linear model
summary(lm(`2011` ~ `2016`, data= dat))

#Find mean between both years except for Cayuga (year 2011 only) and Comox (year 2016 only)
ms_avg <- ms %>% 
  filter(language!="Cayuga") %>%
  group_by(family, language, age, source) %>%
  summarise(speaker=mean(speaker)) %>%
  bind_rows(ms %>% filter(language=="Cayuga", year==2011) %>% ungroup() %>% select(-year))

#Save
saveRDS(ms_avg, "ms_avg")

