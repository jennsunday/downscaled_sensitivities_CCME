#goal: produce maps with # of species with 10% change it at least one variable.
#repeat for 30%.

# notes: this is going well, but I should go back to original study-based data 
# and separate pelagic vs. benthic data, 
# so that I have a better representation across these regions.
# is there a column for region? if not make one. meta-analysis need to be within regions.
# then in files 6,7,8 I should make a vector for # responses > 10% and > 30% and total,
# so that it is easy to plot them together after that. 

#read in libraries:
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(cowplot)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

#first, bottom data 2km
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
oxybot_responses<-read_csv("processed_data/oxybot_responses_2km.csv")
CO2bot_responses<-read_csv("processed_data/CO2bot_responses_2km.csv")
tempbot_responses<-read_csv("processed_data/tempbot_responses_2km.csv")

num_species<-c(unique(oxybot_responses$species), 
               unique(CO2bot_responses$species),  
               unique(tempbot_responses$species)) %>%
  unique(.) %>%
  length(.)


species_over_bot<-rbind(oxybot_responses, 
                           CO2bot_responses, 
                           tempbot_responses) %>%
  group_by(lat, long) %>%
  summarize(number_species_over_10=length(which(number_over_10>1)),# how many species have at least one response > 10?
            number_species_over_20=length(which(number_over_20>1)),# how many species have at least one response > 10?
            number_species_over_30=length(which(number_over_30>1)),# how many species have at least one response > 10?
            nodata=sum(is.na(number_over_10))) %>%
  mutate(number_species_over_10=ifelse(nodata>0, NA, number_species_over_10),
         number_species_over_20=ifelse(nodata>0, NA, number_species_over_20),
         number_species_over_30=ifelse(nodata>0, NA, number_species_over_30))


colourCount = length(unique(as.factor(species_over_bot$number_species_over_10)))
species_over_bot %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with greater than 10% response 9") +
  theme_classic()
ggsave("figures/num_species_10percent_2km.pdf", height = 6, width = 7)

colourCount = length(unique(as.factor(species_over_bot$number_species_over_20)))
species_over_bot %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_20))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with greater than 20% response 9") +
  theme_classic()
ggsave("figures/num_species_20percent_2km.pdf", height = 6, width = 7)


#colourCount = length(unique(as.factor(species_over_bot$number_species_over_30)))
species_over_bot %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_30))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with >30% response") +
  theme_classic()
ggsave("figures/num_species_30percent_2km.pdf", height = 6, width = 7)


#second, 200 data 2km
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
oxy200_responses<-read_csv("processed_data/oxy200_responses_2km.csv")
CO2200_responses<-read_csv("processed_data/CO2200_responses_2km.csv")
temp200_responses<-read_csv("processed_data/temp200_responses_2km.csv")

num_species<-c(unique(oxy200_responses$species), 
               unique(CO2200_responses$species),  
               unique(temp200_responses$species)) %>%
  unique(.) %>%
  length(.)

species_over_200<-rbind(oxy200_responses, 
                           CO2200_responses, 
                           temp200_responses) %>%
  group_by(lat, long) %>%
  summarize(number_species_over_10=length(which(number_over_10>1)),# how many species have at least one response > 10?
            number_species_over_30=length(which(number_over_30>1)),# how many species have at least one response > 10?
            nodata=sum(is.na(number_over_10))) %>%
  mutate(number_species_over_10=ifelse(nodata>0, NA, number_species_over_10),
         number_species_over_30=ifelse(nodata>0, NA, number_species_over_30))


colourCount = length(unique(as.factor(species_over_200$number_species_over_10)))
species_over_200 %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with greater than 10% response 3") +
  theme_classic()


#next, bottom data 12km
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
oxybot_responses_12<-read_csv("processed_data/oxybot_responses_12km.csv")
CO2bot_responses_12<-read_csv("processed_data/CO2bot_responses_12km.csv")
tempbot_responses_12<-read_csv("processed_data/tempbot_responses_12km.csv")

species_over_bot_12<-rbind(oxybot_responses_12, 
                        CO2bot_responses_12, 
                        tempbot_responses_12) %>%
  group_by(lat, long) %>%
  summarize(number_species_over_10=length(which(number_over_10>1)),# how many species have at least one response > 10?
            number_species_over_30=length(which(number_over_30>1)),# how many species have at least one response > 10?
            nodata=sum(is.na(number_over_10))) %>%
  mutate(number_species_over_10=ifelse(nodata>0, NA, number_species_over_10),
         number_species_over_30=ifelse(nodata>0, NA, number_species_over_30))


#colourCount = length(unique(as.factor(species_over_bot_12$number_species_over_10)))
species_over_bot_12 %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with greater than 10% response 9") +
  coord_cartesian(xlim=c(200,300), ylim=c(80, 380))
ggsave("figures/num_species_10percent_12km.pdf", height = 6, width = 7)

#colourCount = length(unique(as.factor(species_over_bot_12$number_species_over_30)))
species_over_bot_12 %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_30))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with greater than 30% response 9") +
  coord_cartesian(xlim=c(200,300), ylim=c(80, 380))
ggsave("figures/num_species_30percent_12km.pdf", height = 6, width = 7)


#200 data 12km
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
oxy200_responses<-read_csv("processed_data/oxy200_responses_12km.csv")
CO2200_responses<-read_csv("processed_data/CO2200_responses_12km.csv")
temp200_responses<-read_csv("processed_data/temp200_responses_12km.csv")

species_over_200<-rbind(oxy200_responses, 
                        CO2200_responses, 
                        temp200_responses) %>%
  group_by(lat, long) %>%
  summarize(number_species_over_10=length(which(number_over_10>1)),# how many species have at least one response > 10?
            number_species_over_30=length(which(number_over_30>1)),# how many species have at least one response > 10?
            nodata=sum(is.na(number_over_10))) %>%
  mutate(number_species_over_10=ifelse(nodata>0, NA, number_species_over_10),
         number_species_over_30=ifelse(nodata>0, NA, number_species_over_30))

colourCount = length(unique(as.factor(species_over_200$number_species_over_10)))
species_over_200 %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with greater than 10% response 3") +
  coord_cartesian(xlim=c(200,300), ylim=c(80, 380))
