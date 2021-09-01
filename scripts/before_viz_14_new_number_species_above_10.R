#goal: produce one map from each model with # of species with 10% change it at least one variable.
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

#first, data from 2km
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
oxy_responses_2km<-read_csv("processed_data/oxyrel_greater_than_10percent.csv")
CO2_responses_2km<-read_csv("processed_data/CO2rel_greater_than_10percent.csv")
temp_responses_2km<-read_csv("processed_data/temprel_greater_than_10percent.csv")

num_species<-c(unique(oxybot_responses$species), 
               unique(CO2bot_responses$species),  
               unique(tempbot_responses$species)) %>%
  unique(.) %>%
  length(.)

#combine all environmental variables
species_responses<-rbind(oxy_responses_2km, 
                        CO2_responses_2km, 
                        temp_responses_2km) %>%
    group_by(lat, long, species, no_env_data) %>%
    summarize(number_responses_over_10=length(which(abs_number_over_10>=1))) %>%
    mutate(number_responses_over_10=as.numeric(ifelse(
    no_env_data=="no_data", NA, number_responses_over_10))) %>%
    group_by(lat, long, no_env_data) %>%
    summarize(number_species_over_10=length(which(number_responses_over_10>=1))) %>%
    mutate(number_species_over_10=as.numeric(ifelse(
        no_env_data=="no_data", NA, number_species_over_10)))

filter(species_responses, no_env_data=="no_data")
    
species_responses$number_species_over_10

colourCount = length(unique(as.factor(species_responses$number_species_over_10)))
species_responses %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with greater than 10% responses") +
  theme_classic()
ggsave("figures/num_species_10percent_2km.pdf", height = 6, width = 7)

#next, 12km
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
oxy_responses_12<-read_csv("processed_data/oxyrel_greater_than_10percent_big.csv")
CO2_responses_12<-read_csv("processed_data/CO2rel_greater_than_10percent_big.csv")
temp_responses_12<-read_csv("processed_data/temprel_greater_than_10percent_big.csv")

species_responses_12<-rbind(oxy_responses_12, 
                            CO2_responses_12, 
                            temp_responses_12) %>%
  group_by(lat, long, species, no_env_data) %>% #first figure out # responses >10 within species (across factors)
  summarize(number_responses_over_10=length(which(abs_number_over_10>=1))) %>%
  mutate(number_responses_over_10=as.numeric(ifelse(
    no_env_data=="no_data", NA, number_responses_over_10))) %>%
  group_by(lat, long, no_env_data) %>% #next figure out # responses >10 across species
  summarize(number_species_over_10=length(which(number_responses_over_10>=1))) %>%
  mutate(number_species_over_10=as.numeric(ifelse(
    no_env_data=="no_data", NA, number_species_over_10)))

#colourCount = length(unique(as.factor(species_over_bot_12$number_species_over_10)))
species_responses_12 %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  labs(fill="number of species 
       with greater than 10% responses") +
  coord_cartesian(xlim=c(200,300), ylim=c(80, 380))
ggsave("figures/num_species_10percent_12km.pdf", height = 6, width = 7)
