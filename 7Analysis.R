################################################################################
#1.PACKAGES
################################################################################
rm(list=ls())

setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/")

#packages
library(tidyverse)
library(openxlsx)
library(ggpubr)
library(rnaturalearth)
library(treemapify)
library(sf)
library(ggridges)

################################################################################
#2. PREPARATION
################################################################################
#Census data (i.e. before the simulations)###################################
#Speakers by age and languages in the year 2014
ms <- readRDS("1CensusData/ms_avg") 
  
#Total number of speakers in the year 2014
ms_tot <- readRDS("1CensusData/ms_avg") %>% 
  group_by(language, family) %>% 
  summarise(speaker=sum(speaker)) %>% 
  arrange(speaker) 

#language name vector
languages <- factor(ms_tot %>% pull(language), levels=c(ms_tot %>% pull(language)))

#make sure the ms and ms_tot data frames have same language levels
ms$language <- factor(ms$language, levels=languages)
ms_tot$language <- factor(ms_tot$language, levels=languages)

#ISO code
ms_tot$iso <- c("squ", "hai", "sek", "coo", "tce", "hei", "tht", "kut", "str", 
                "srs", "bcr", "has", "ttm", "cay", "kkz", "tsi", "bea", "lil", 
                "nuk", "thp", "pqm", "kwk", "crg", "hur", "ncg", "shs", "clc", 
                "scs", "git", "ikt", "caf", "xsl", "alq", "dgr", "bla", "sto", 
                "atj", "mic", "chp", "ojs", "moe", "ojc", "ike", "csw")

#Model results###############################################################
#Number of speakers by year (median + 90% bounds)#########
#All ages
#Matrix
mat <- lapply(languages, function(l) apply(readRDS(paste("6Forecast/Allages/", l, sep="")), 1, quantile, probs = c(0.05, 0.5, 0.95)))

#Data frame
n_all <- bind_rows(
  lapply(1:44, function(l) 
    data.frame(language=languages[l], 
               year=(if(l==14){2011}else if(l==4){2016}else{2014}):2100, 
               lower=mat[[l]][1,], 
               median=mat[[l]][2,], 
               upper=mat[[l]][3,])))

#Childbearing age and below
#Matrix
mat <- lapply(languages, function(l) apply(readRDS(paste("6Forecast/Parents/", l, sep="")), 1, quantile, probs = c(0.05, 0.5, 0.95)))

#Data frame
n_parents <- bind_rows(
  lapply(1:44, function(l) 
    data.frame(language=languages[l], 
                year=(if(l==14){2011}else if(l==4){2016}else{2014}):2100, 
                lower=mat[[l]][1,], 
                median=mat[[l]][2,], 
                upper=mat[[l]][3,])))

#Children age
#Matrix
mat <- lapply(languages, function(l) apply(readRDS(paste("6Forecast/Children/", l, sep="")), 1, quantile, probs = c(0.05, 0.5, 0.95)))

#Data frame
n_children <- bind_rows(lapply(1:44, function(l) 
  data.frame(language=languages[l], 
             year=(if(l==14){2011}else if(l==4){2016}else{2014}):2100, 
             lower=mat[[l]][1,], 
             median=mat[[l]][2,], 
             upper=mat[[l]][3,])))

#Dormancy risk by year###################################
#All ages
#Matrix
mat <- lapply(languages, function(l) 
  sapply(1:if(l=="Comox"){85}else if(l=="Cayuga"){90}else{87}, function(x) sum(readRDS(paste("6Forecast/Allages/", l, sep=""))[x,] == 0)))

#Data frame
risk_all <- bind_rows(
  lapply(1:44, function(l) 
    data.frame(language=languages[l], 
               year=(if(l==14){2011}else if(l==4){2016}else{2014}):2100,
               risk=mat[[l]]/10)))

#Parents and children
#Matrix
mat <- lapply(languages, function(l) 
  sapply(1:if(l=="Comox"){85}else if(l=="Cayuga"){90}else{87}, function(x) sum(readRDS(paste("6Forecast/Parents/", l, sep=""))[x,] == 0)))

#Data frame
risk_parents <- bind_rows(
  lapply(1:44, function(l) 
    data.frame(language=languages[l], 
               year=(if(l==14){2011}else if(l==4){2016}else{2014}):2100,
               risk=mat[[l]]/10)))

#Children
#Matrix
mat <- lapply(languages, function(l) 
  sapply(1:if(l=="Comox"){85}else if(l=="Cayuga"){90}else{87}, function(x) sum(readRDS(paste("6Forecast/Children/", l, sep=""))[x,] == 0)))

#Data frame
risk_children <- bind_rows(
  lapply(1:44, function(l) 
    data.frame(language=languages[l], 
               year=(if(l==14){2011}else if(l==4){2016}else{2014}):2100,
               risk=mat[[l]]/10)))

#Ethnologue data#############################################################
#read in data
ethno <- read.table("C:/Users/micbo262/Documents/EthnologueData/e13-25/e25.tab", sep="\t", fill = TRUE, header = T) %>% 
  select(Language.Status, Population.Numeric, name, ISO.639.3) 

#Change names
names(ethno) <- c("ethno_status", "ethno_speaker", "ethno_language", "iso")
  
#select languages
ethno <- ethno %>% filter(iso %in% ms_tot$iso)

#include note about Cree, Ottawa, Naskapi
ethnonote <- data.frame(ethno_language=c("Cree", "Ojibway", "Innu"),
                        note=c("Includes: Southern East, Plains, Northern East, Moose, Swampy, Woods Cree. Speakers: 10,875. Status: 5-7", 
                               "Includes: Ottawa; Northwestern, Western, Eastern, Central Ojibwa. Speakers: 7,630; 63,900. Status: 7; 6b-7",
                               "Includes: Naskapi. Speakers: 1,210. Status: 4"))
  
#endangerment status: keep only the label
ethno$ethno_status <- substr(ethno$ethno_status, 0, 2)

#remove trailing spaces following status
ethno$ethno_status <- trimws(ethno$ethno_status)

#Change population numbers to numeric
ethno$ethno_speaker <- as.numeric(ethno$ethno_speaker)

#add Haida & Ojibway back (they were removed because split in different dialects/languages)
ethno <- bind_rows(ethno %>% filter(iso!="hax", iso!="ojc", iso!="csw"),
                   data.frame(ethno_language = "Haida", ethno_speaker=9, ethno_status = "8b", iso="hax"),
                   data.frame(ethno_language = "Ojibway", ethno_speaker=89860, ethno_status = "6b", iso="ojc"),
                   data.frame(ethno_language = "Cree", ethno_speaker=10875, ethno_status = "6a", iso="csw"))

#add notes
ethno <- ethno %>% left_join(ethnonote)

#Harald's data, coordinates##################################################
#merge with ethnologue data using ISO code
ethno <- ethno %>% 
  left_join(read.xlsx("C:/Users/micbo262/Documents/EthnologueData/Tobias2023.xlsx") %>% 
              select(Lat, Lon, `ISO.639-3`) %>% 
              rename(latitude=Lat,
                     longitude=Lon,
                     iso=`ISO.639-3`))

#statcan, ethno, coordinates together
ms_tot <- ms_tot %>% left_join(ethno, by="iso")
ms_tot$latitude <- ifelse(ms_tot$language=="Cayuga", 44, ms_tot$latitude)
ms_tot$longitude <- ifelse(ms_tot$language=="Cayuga", -78, ms_tot$longitude)

################################################################################
#3. FIGURE 1: MAP
################################################################################
#map data from natural earth
world <- ne_countries(scale = "small", country=c("Canada", "United States of America"), returnclass = "sf")

# filter to bbox
bbox <- st_bbox(world)

#set limits
ylimits <- c(min(ms_tot$latitude)-1, max(ms_tot$latitude)+8)
xlimits <- c(min(ms_tot$longitude)-2, max(ms_tot$longitude)+8)

setwd("7Analysis/map")
world.rst <- ne_load(type="MSR_50M", category='raster', destdir=".", returnclass="sf")
world.rst.df <- raster::as.data.frame(world.rst, xy=TRUE)
world.rst.df <- world.rst.df[dplyr::between(world.rst.df$x, bbox[['xmin']], bbox[['xmax']]), ]
world.rst.df <- world.rst.df[dplyr::between(world.rst.df$y, xlimits[[1]], xlimits[[2]]), ]

setwd("C:/Users/micbo262/Documents/ProjIndigLangCan/")

ggplot(data=world) +
  geom_sf(fill = "mintcream") +
  geom_point(data=ms_tot %>% mutate(family = case_when(family=="Isolate" | family=="Mixed" ~ "Isolate/Mixed", TRUE ~ as.character(family))), 
             aes(x=longitude, y=latitude, size=speaker, color=family)) +
  ylim(ylimits) +
  xlim(xlimits)+
  theme_bw(base_size=14) +
  theme(
    panel.background = element_rect(fill = "slategray1"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    legend.key.height = unit(.3, 'cm'),
    legend.title = element_text(size=8), 
    legend.text = element_text(size=7))+
  scale_color_brewer(palette = "Set1")+
  scale_size_continuous(breaks=c(100, 1000, 10000), name="Population size")

#Adjust legend position!
#Idea: show families with text in the map
#Use lines to connect the points and the text, if needed

ggsave('7Analysis/Fig1.tiff', width=6, height=4, dpi=1000)

#In text comments
ms_tot %>% 
  ungroup %>%
  summarise(min=round(min(speaker)), 
            max=round(max(speaker)),
            mean=mean(speaker),
            median=quantile(speaker, .5))

################################################################################
#4.FIGURE 2: SPEAKER NUMBERS IN 2014 AND 2100 AND AGE OF YOUNGEST SPEAKER IN 2100
################################################################################
#Data frame###################################################################
df2 <- n_all %>% 
  filter(year==2100) %>%
  select(-year) %>%
  mutate(Year="2100") %>%
  rename(speaker = median) %>% 
  mutate(Youngest=ifelse(filter(n_children, year==2100)$median>0, "Children",
                         ifelse(filter(n_parents, year==2100)$median>0, "Childbearing", "Grandparent"))) %>%
  bind_rows(ms_tot %>% 
              ungroup() %>%
              select(language, speaker) %>%
              mutate(Year="2014") %>%
              mutate(Youngest=ifelse(filter(n_children, 
                                            year==ifelse(language=="Comox", 2016, 
                                                                         ifelse(language=="Cayuga", 2011, 2014)))$median>0, "Children",
                                         ifelse(filter(n_parents, year==ifelse(language=="Comox", 2016, 
                                                                               ifelse(language=="Cayuga", 2011, 2014)))$median>0, "Childbearing", "Grandparent")))
  ) %>%
  mutate(speaker=round(speaker))

#assign levels to age category
df2$Youngest <- factor(df2$Youngest, levels=c("Children", "Childbearing", "Grandparent"))

#Plot
ggplot(df2, aes(speaker, language, color=Youngest, shape=Year, group=interaction(Youngest, Year)))+
  geom_point()+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10),
                     breaks = c(0, 10^1, 10^2, 10^3, 10^4, 10^5),
                     labels=scales::comma,
                     lim=c(0,105000))+
  annotation_logticks(sides="b")+
  geom_segment(aes(x=lower, xend=speaker-1/10*speaker, y=language, yend=language, color=Youngest, group=Youngest), linewidth=.8)+
  geom_segment(aes(x=upper, xend=speaker+1/10*speaker, y=language, yend=language, color=Youngest, group=Youngest), linewidth=.8)+
  theme_bw()+
  scale_shape_manual(values=c(16, 1))+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position = c(.85, .3))+
  ylab("")+
  xlab("Population size") 

ggsave("7Analysis/Fig2.tiff", height=7, width=6, dpi=1000)

#In text comments
df2 %>% 
  group_by(Year) %>%
  summarise(total=sum(speaker),
            lower=sum(lower),
            upper=sum(upper))

df2b <- df2 %>%
  filter(Year=="2014") %>%
  select(Year, language, speaker) %>%
  left_join(df2 %>% filter(Year=="2100") %>%
              select(language, lower, speaker, upper) %>%
              rename(predict=speaker)) %>%
  mutate(decrease=case_when(
    upper<speaker ~ 1,
    upper>=speaker ~ 0
  ), 
  foldchange=predict/speaker) 
  
sum(df2b$decrease)  #significant decrease
length(filter(df2b, foldchange<.5)$language) #fold change .5 or lower
length(filter(df2b, foldchange<.1)$language) #fold change .1 or lower
filter(df2b, lower==0)$language #prediction intervals including 0
table(filter(df2, Year=="2100")$Youngest) #Age category of youngest speaker

################################################################################
#5. TABLE 1 DORMANCY RISKS 
################################################################################
#data frame
df2b %>% 
  left_join(risk_all %>% filter(year==2100) %>% select(-year)) %>% 
  left_join(risk_parents %>% filter(year==2100) %>% select(-year) %>% rename(risk_it=risk)) %>%
  filter(risk_it>=10) %>% 
  ungroup() %>%
  rename(Language=language, 
         Dormancy = risk,
         `Interrupted IT` = risk_it,
         `2014` = speaker) %>%
  mutate(`2100` = paste(round(predict), " (", round(lower), "-", round(upper),")", sep="")) %>%
  select(Language, Dormancy, `Interrupted IT`, `2014`, `2100`) %>%
  arrange(-Dormancy) %>% 
  write.xlsx("7Analysis/table1.xlsx")

#Secwepemctsin
n_all %>% 
  filter(language=="Secwepemctsin", year==2100)

risk_all %>% 
  filter(language=="Secwepemctsin", year==2100)

risk_parents %>% 
  filter(language=="Secwepemctsin", year==2100)

################################################################################
#SUPPLEMENTARY INFORMATION
################################################################################
#Table S1: Language inclusion###################################################
#Statististics Canada 2016
sc_name_2016 <- read.csv("1CensusData/indigenousmothertongue2016.csv")[8:89,] %>% 
  pivot_longer(Total...Age:X100.years.and.over, names_to="age", values_to="speaker") %>%
  filter(age!="Total...Age") %>%
  mutate(age = rep(seq(0, 100, 5), 82)) %>%
  rename(sc_name_2016 = Mother.tongue) %>%
  filter(!str_detect(sc_name_2016,"languages") & !(str_detect(sc_name_2016, ", n.o.s.") | str_detect(sc_name_2016, ", n.i.e."))) %>%
  pull(sc_name_2016)

#Statististics Canada 2011
sc_name_2011 <- read.csv("1CensusData/indigenousmothertongue2011.csv")[7:80,] %>% 
  pivot_longer(Total...Age:X100.years.and.over, names_to="age", values_to="speaker") %>%
  filter(age!="Total...Age") %>%
  mutate(age = rep(seq(0, 100, 5), 74)) %>%
  rename(sc_name_2011 = Mother.Tongue) %>%
  filter(!str_detect(sc_name_2011,"languages") & !(str_detect(sc_name_2011, ", n.o.s.") | str_detect(sc_name_2011, ", n.i.e."))) %>%
  pull(sc_name_2011)

#iso names, SC 2016
iso_2016 <- c("alq", "atj", "bcr", "bea", "bla", "crx", "cay", "clc", "coo", "dak", 
              "chp", "dgr", "git", "gwi", "hai", "has", "hur", "hei", "ikt", "ike",
              "kkz", "kut", "kwk", "lil", "pqm", "mic", "crg", "moh", "moe", "crm",
              "nsk", "ncg", "scs", "crl", "ttm", "nuk", "ojs", "oji", "oka", "one",
              "otw", "crk", "srs", "sek", "shs", "xsl", "crj", "tce", "squ", "sto",
              "str", "csw", "tht", "thp", "tli", "tsi", "cwd")

#iso names, SC 2011
iso_2011 <- c("alq", "atj", "bea", "bla", "crx", 
              "cay", "clc", "dak", "chp", "git", 
              "gwi", "hai", "has", "hur", "hei", 
              "moe", "ikt", "ike", "", "kkz", 
              "kut", "kwk", "lil", "pqm", "mic", 
              "crg", "moh", "nsk", "ncg", "nuk", 
              "scs", "ttm", "ojs", "oji", "oka", 
              "one", "crk", "srs", "sek", "shs", 
              "xsl", "tce", "squ", "sto", "str", 
              "csw", "tht", "thp", "dgr","tli", "tsi", "bcr", "cwd")

#Data frame, language names, SC 2016 and 2011
sc <- data.frame(iso=iso_2016, sc_name_2016=sort(unique(trimws(sc_name_2016)))) %>%
  full_join(data.frame(iso=iso_2011,  sc_name_2011=sort(unique(trimws(sc_name_2011))))) %>%
  mutate(sc_name_2011 = ifelse(is.na(sc_name_2011), "None, category not specified", sc_name_2011))

#Names used in this study (based mostly on SC 2021)
study <- ms_tot %>% 
  rename(study_name=language) %>%
  select(study_name, iso) %>%
  mutate(iso=ifelse(iso=="caf", "crx", iso),
         iso=ifelse(iso=="ojc", "oji", iso),
         iso=ifelse(iso=="csw", NA, iso)) 

#Names in Ethnologue
ethno <- read.table("C:/Users/micbo262/Documents/EthnologueData/e13-25/e25.tab", sep="\t", fill = TRUE, header = T) %>% 
  filter(Country=="Canada" | name=="Michif" | name=="Tlingit" | name=="Dakota") %>% 
  select(Language.Status, Population.Numeric, name, ISO.639.3) %>%
  filter(name!="Quebec Sign Language", name!="Hutterisch", name!="Maritime Sign Language", name!="Inuit Sign Language", name!="Plautdietsch") %>%
  arrange(name) 

#Change data frame column names
names(ethno) <- c("ethno_status", "ethno_number", "ethno_name", "iso")

#endangerment status: keep only the label (to be used below)
ethno$ethno_status <- trimws(substr(ethno$ethno_status, 0, 2))

#Change population numbers to numeric (to be used below)
ethno$ethno_number <- as.numeric(ethno$ethno_number)

#SC, this study, Ethnologue: comparison
comparison_names <- sc %>% 
  full_join(study %>% filter(study_name!= "Cree", study_name!="Innu-Naskapi")) %>% 
  full_join(ethno %>% filter(ethno_name!= "Cree", ethno_name!="Inuktitut", ethno_name!="Slave")) %>% 
  select(iso, ethno_name, study_name, sc_name_2016, sc_name_2011) 

#change study_name to character (not sure why it was converted to factor)
comparison_names$study_name <- as.character(comparison_names$study_name)

#Make table with full comparison, save to Excel
comparison_names %>%
  mutate(study_name = ifelse(iso %in% c("dak", "gwi", "moh", "oka", "one", "tli"), "None, not included", study_name),
         note = ifelse(iso %in% c("dak", "gwi", "moh", "oka", "one", "tli"), "Significant proportion of speakers in the US", NA), 
         study_name = ifelse(iso %in% c("crm", "crl", "crk", "crj", "cwd", "csw"), "Cree", study_name),
         note = ifelse(iso %in% c("crm", "crl", "crk", "crj", "cwd", "csw"), "Whole Cree continuum as one language", note),
         study_name = ifelse(iso=="nsk" | iso=="moe", "Innu-Naskapi", study_name),
         note = ifelse(iso=="nsk" | iso=="moe", "Innu and Naskapi modelled as one language", note),
         study_name = ifelse(iso=="otw", "Ojibway", study_name),
         note = ifelse(iso=="otw" | iso=="oji", "Ojibway and Ottawa modelled as one language", note),
         study_name = ifelse(iso %in% c("abe", "asb", "blc", "caf", "dtd", "hdn", "hax", "umu", "ojc", "ojg", "ojb", "ojw", "ono", "sec", "tgx", "tta", "wdt"),
                             "None, not included", study_name),
         sc_name_2016 = ifelse(iso %in% c("abe", "asb", "blc", "caf", "dtd", "hdn", "hax", "umu", "ojc", "ojg", "ojb", "ojw", "ono", "sec", "tgx", "tta", "wdt"),
                               "None, not specified", sc_name_2016),
         sc_name_2011 = ifelse(iso %in% c("abe", "asb", "blc", "caf", "dtd", "hdn", "hax", "umu", "ojc", "ojg", "ojb", "ojw", "ono", "sec", "tgx", "tta", "wdt"),
                               "None, not specified", sc_name_2011),
         study_name = ifelse(sc_name_2011=="Inuvialuktun", "Inuvialuktun", study_name),
         note = ifelse(sc_name_2011=="Inuvialuktun" | study_name == "Inuvialuktun", "Whole Inuvialuktun continuum as one language", note),
         included = ifelse(study_name=="None, not included", 0, 1)) %>%
  arrange(-included, study_name) %>%
  write.xlsx("7Analysis/tableS1.xlsx")

#Table S2 pop. size & age youngest speaker/endang. status, Ethno################
ms_tot %>% 
  mutate(study_number=round(speaker)) %>%
  select(language, study_number, iso) %>%
  mutate(iso=ifelse(iso=="caf", "crx", iso),
         iso=ifelse(iso=="ojc", "oji", iso),
         iso=ifelse(iso=="csw", NA, iso)) %>%
  left_join(ms %>% group_by(language, age) %>% 
              summarise(speaker=round(sum(speaker))) %>% 
              filter(round(speaker)>0) %>%
              group_by(language) %>%
              summarise(study_youngest=min(age))) %>%
  left_join(ethno %>% select(-ethno_name)) %>%
  mutate(ethno_status=case_when(
    ethno_status=="2" | ethno_status=="5"  ~ paste(ethno_status, " (All generations)", sep=""),
    ethno_status=="6b" ~ paste(ethno_status, " (All generations but losing users)", sep=""),
    ethno_status=="7" ~ paste(ethno_status, " (Child-bearing generation)", sep=""),
    ethno_status=="8a" ~ paste(ethno_status, " (Grandparent generation)", sep=""),
    ethno_status=="8b" ~ paste(ethno_status, " (Elderly)", sep=""),
    ethno_status=="9" ~ paste(ethno_status, " (Dormant)", sep=""))) %>% 
  relocate(iso, language, study_number, ethno_number, study_youngest, ethno_status) %>%
  write.xlsx("7Analysis/tableS2.xlsx")

#Fig. S1 Language and family sizes (tree plot)##################################
df3 <- ms_tot %>% 
  select(family, language, speaker) %>% 
  left_join(n_all %>% filter(year==2100) %>% select(-year)) %>% 
  pivot_longer(c(speaker, median), names_to = "year", values_to = "speaker") %>%
  mutate(lower = ifelse(year==2014, NA, lower),
         upper = ifelse(year==2014, NA, upper), 
         year = ifelse(year=="speaker", 2014, 2100))

ggplot(df3 %>% mutate(family = ifelse(family=="Mixed" | family=="Isolate", "Isolate/Mixed", family)), 
       aes(area = speaker, fill = family, label = language, subgroup = family))+
  geom_treemap()+
  geom_treemap_text()+
  facet_wrap(.~year, ncol=1)+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal() 

ggsave("7Analysis/FigS1.tiff", height = 6, width=8, dpi=1000)

#in text comments
df3 %>% 
  group_by(family, year) %>%
  summarise(speaker=sum(speaker),
            lower=sum(lower),
            upper=sum(upper)) %>%
  pivot_wider(names_from = year, values_from = speaker) %>%
  relocate(family, `2014`, `2100`, lower, upper)

df3 %>% 
  filter(language=="Haida" | language=="Ktunaxa" | language=="Michif" | family=="Inuit") %>%
  pivot_wider(values_from = speaker, names_from = year) %>% 
  relocate(family, language, `2014`, `2100`, lower, upper)

left_join(
  df2 %>% group_by(Year) %>% filter(language %in% languages[36:44]) %>% summarise(largest=sum(speaker)),
  df2 %>% group_by(Year) %>% filter(language %in% languages[1:35]) %>% summarise(smallest=sum(speaker))) %>%
  left_join(df2 %>% group_by(Year) %>% summarise(total=sum(speaker))) %>%
  mutate(largest = paste(largest, " (", round(largest / total * 100, 2), ")", sep=""),
         smallest = paste(smallest, " (", round(smallest/ total * 100, 2), ")", sep=""))

df3 %>% group_by(year, family) %>% summarise(speaker=sum(speaker)) %>% 
  pivot_wider(names_from = year, values_from = speaker) %>%
  mutate(prop = `2100` / `2014`)

df3 %>% filter(family=="Isolate" | family=="Tsimshian") %>% group_by(year, language) %>% summarise(speaker=sum(speaker)) %>% 
  pivot_wider(names_from = year, values_from = speaker) %>%
  mutate(prop = `2100` / `2014`)

#Fig. S2 Dormancy risks over time at different thresholds#######################
#data frame
dfa4 <- bind_rows(
  risk_all %>% filter(risk>=5) %>% group_by(year) %>% summarise(All=n(), Risk = "5%"),
  risk_all %>% filter(risk>=50) %>% group_by(year) %>% summarise(All=n(), Risk = "50%"),
  risk_all %>% filter(risk>=95) %>% group_by(year) %>% summarise(All=n(), Risk = "95%")) %>% 
  full_join(
    bind_rows(
      risk_parents %>% filter(risk>=5) %>% group_by(year) %>% summarise(Childbearing=n(), Risk = "5%"),
      risk_parents %>% filter(risk>=50) %>% group_by(year) %>% summarise(Childbearing=n(), Risk = "50%"),
      risk_parents %>% filter(risk>=95) %>% group_by(year) %>% summarise(Childbearing=n(), Risk = "95%"))) %>%
  full_join(
    bind_rows(
      risk_children %>% filter(risk>=5) %>% group_by(year) %>% summarise(Children=n(), Risk = "5%"),
      risk_children %>% filter(risk>=50) %>% group_by(year) %>% summarise(Children=n(), Risk = "50%"),
      risk_children %>% filter(risk>=95) %>% group_by(year) %>% summarise(Children=n(), Risk = "95%"))) %>%
  arrange(year) %>%
  pivot_longer(c(All, Children, Childbearing), names_to = "Category", values_to = "N") %>%
  mutate(N = ifelse(is.na(N), 0, N))

#plot
ggplot(dfa4)+
  geom_line(aes(year, N, group=Risk, color=Risk))+
  facet_wrap(~Category)+
  theme_bw()+
  scale_x_continuous(breaks=seq(2020, 2100, 20))+
  scale_y_continuous(breaks=seq(0, 44, 4))+
  xlab("Year")+
  ylab("Number of languages")+
  scale_color_brewer(palette="Dark2")

ggsave("7Analysis/FigS2.tiff", dpi=1000, height=3, width=7)

#Fig S3 Population pyramids#####################################################
ggplot(ms %>% 
         group_by(age, language) %>% 
         summarise(speaker=round(sum(speaker))) %>%
         filter(language %in% languages[1:24]),
       aes(age, speaker))+
  geom_col()+
  theme_minimal()+
  coord_flip()+
  ylab("Number of speakers")+
  xlab("Age")+
  facet_wrap(~language, scales = "free_x", ncol=4)

ggsave("7Analysis/FigS3a.tiff", dpi=1000, height=9, width=6.5)

ggplot(ms %>% 
         group_by(age, language) %>% 
         summarise(speaker=round(sum(speaker))) %>%
         filter(language %in% languages[25:44]),
       aes(age, speaker))+
  geom_col()+
  theme_minimal()+
  coord_flip()+
  ylab("Number of speakers")+
  xlab("Age")+
  facet_wrap(~language, scales = "free_x", ncol=4)

ggsave("7Analysis/FigS3b.tiff", dpi=1000, height=7.5, width=6.5)

#Table S3 Counts of total children and mean it #################################
bac <- bind_rows(
  lapply(languages, function(l) 
    data.frame(language=l,
               mean=round(mean(unlist(readRDS(paste("4Backcast/", l, sep=""))))),
               sd=round(sd(unlist(readRDS(paste("4Backcast/", l, sep="")))), 1)))) %>%
  left_join(ms %>% filter(age==0) %>% group_by(language) %>% summarise(speaker=round(sum(speaker))))

it <- bind_rows(
  lapply(1:44, function(x) 
    data.frame(language=languages[x],
               it_mean=round(mean(readRDS("5IntTrans/all_dist_new")[[x]]),2),
               it_sd=round(sd(readRDS("5IntTrans/all_dist_new")[[x]]), 2))))

left_join(bac, it) %>%
  mutate(`Children total (mean and sd)` = paste(mean, " (", sd, ")", sep=""),
         `PIT (mean and sd)` = paste(it_mean, " (", it_sd, ")", sep="") ) %>%
  select(language, speaker, `Children total (mean and sd)`,  `PIT (mean and sd)`) %>% 
  rename(`Children speakers` = speaker) %>% 
  write.xlsx("7Analysis/tableS3.xlsx")

#Fig S4a Posterior distributions################################################
#1000 draws from the joint posterior distribution of intergenerational transmission
it <- unlist(lapply(1:44, function(x) sample(readRDS("5IntTrans/all_dist_new")[[x]], 1000)))

#data frame
ggplot(data.frame(language=rep(languages, each=1000), it=it),
       aes(x = it, y = language, fill = stat(x)))+
  geom_density_ridges_gradient(scale = 4, size = 0.3, rel_min_height = 0.005, bandwidth = 0.015) +
  scale_fill_viridis_c(name = "PIT", option = "C") + 
  theme_ridges(center_axis_labels = TRUE)+
  ylab("")+
  xlab("Probability of intergenerational transmission (PIT)")+
  scale_x_continuous(limits=c(-.05, 1.1), breaks=c(seq(0,1,.25)))+
  theme(legend.position="none")

ggsave("7Analysis/FigS4.tiff", height=15, width = 8, dpi=1000)

#Fig S5a Speaker counts by year#################################################
ggplot(n_all %>% filter(language %in% languages[1:24]), aes(year, median))+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.3)+
  facet_wrap(~language, scales = "free", ncol=4)+
  theme_minimal()+
  ylab("Number of speakers")+
  xlab("Year")+
  scale_x_continuous(breaks=c(2020,2060,2100))

ggsave("7Analysis/FigS5a.tiff", dpi=1000, height=9, width=6.5)

ggplot(n_all %>% filter(language %in% languages[25:44]), aes(year, median))+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.3)+
  facet_wrap(~language, scales = "free", ncol=4)+
  theme_minimal()+
  ylab("Number of speakers")+
  xlab("Year")+
  scale_x_continuous(breaks=c(2020,2060,2100))

ggsave("7Analysis/FigS5b.tiff", dpi=1000, height=7.5, width=6.5)

#Lost material: Index of linguistic diversity################################### 
#(I don't know how to compute it when languages go extinct, so I abandoned this idea)

index_a <- n_all %>% 
  select(-lower, -upper) %>% 
  left_join(n_all %>%
              filter(year>=2014) %>%
              group_by(year) %>%
              summarise(total=sum(median, na.rm=T))) %>%
  mutate(fly=median+1 / total) 

index_b <- index_a %>%
  group_by(year) %>%
  summarise(m=exp(mean(log(fly)))) %>% 
  filter(year>=2014) %>%
  mutate(I=m/lag(m)) %>%
  mutate(I=ifelse(is.na(I), 1, I)) %>%
  mutate(I=lag(I)*I) %>%
  mutate(I=ifelse(is.na(I), 1, I))

ggplot(index_b)+
  geom_line(aes(year, I))




