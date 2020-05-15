# Goal: 
# was: where is there enough change to elicit 10% change in 1,2,3, .. 8 biological responses?
# but that doesn't account for the fact that some species are better studied than others
# So: In each grid cell, what proportion of biological responses investigated changed by more than 10%? 

# approach
# A) for each species in relevant layer
# first ask, for each grid cell, how many absolute biological responses are greater than 10% on the temperature field?
# next ask, for each grid cell, how many absolute biological responses are greater than 10% on the CO2 field?
# third, for each grid cell, ask how many absolute biological responses are greater than 10% in the O2 field?
# 4-6, also need to ask, how many total studies were done in each category for each species?
# seventh, add the number of responses across all three layers and devide by total studies. Make a heat map of this.

# B) one map: add up how many reponses are greater than 10% across all species and variables
# C) three maps: add up how many reponses are greater than 10% across all species for each variable

#libraries
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(cowplot)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

# go through and recalculate change by zone. ***
#read in sensitivity by study
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")

#read in Sam's maps 
littleCO2200<-read_csv("processed_data/2km_delta_pCO2_200m.csv", col_names=F)
littleCO2bot<-read_csv("processed_data/2km_delta_pCO2_bot.csv", col_names=F)

#reshape these into a long dataframe - 200 layer
littleCO2200.df<-melt(littleCO2200) %>%
  mutate(lat=rep(1:dim(littleCO2200)[1], dim(littleCO2200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))


#reshape these into a long dataframe - bottom layer
littleCO2bot.df<-melt(littleCO2bot) %>%
  mutate(lat=rep(1:dim(littleCO2bot)[1], dim(littleCO2bot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#CO2 - extract species with CO2 response data and add rows for other species
#and change species names so they will work as column names 
CO2_sens<-sensitivity_by_group %>%
  filter(treatment_var=="CO2") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
CO2_sens<-CO2_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of CO2 change, so every unique response has a lat_long
all_CO2_responses<-expand.grid(unique_response=CO2_sens$unique_response, 
                                latlong=littleCO2bot.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_botCO2<-left_join(all_CO2_responses, littleCO2bot.df)
join_response_and_botCO2<-left_join(join_grid_and_botCO2,CO2_sens)
head(join_response_and_botCO2)

join_grid_and_200CO2<-left_join(all_CO2_responses, littleCO2200.df)
join_response_and_200CO2<-left_join(join_grid_and_200CO2,CO2_sens)
head(join_response_and_200CO2)

#make new column with % change
join_response_and_botCO2<-join_response_and_botCO2%>%
  mutate(percentchangeCO2=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeCO2=="NaN", "no_data", "yes_data"))

join_response_and_200CO2<-join_response_and_200CO2%>%
  mutate(percentchangeCO2=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeCO2=="NaN", "no_data", "yes_data"))

#how many responses observed for each species?
CO2_N_responses_by_species<-CO2_sens %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species) %>%
  summarize(number_responses=n())


#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
CO2bot_greater_than_10percent<-join_response_and_botCO2 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeCO2)>10))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  left_join(.,CO2_N_responses_by_species) %>%
  mutate(proportion_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", number_over_10/number_responses))) #get NAs in



CO2200_greater_than_10percent<-join_response_and_200CO2 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(latlong, species, no_env_data) %>%
  summarize(number_over_10=length(which(abs(percentchangeCO2)>10))) %>% # this is not working
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  left_join(.,CO2_N_responses_by_species) %>%
  mutate(proportion_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", number_over_10/number_responses))) #get NAs in


CO2bot_greater_than_10percent %>% 
  #filter(species=="sablefish") %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = proportion_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)
ggsave("figures/CO2bot_greater_than_10percent.pdf", height = 6, width = 9)


CO2200_greater_than_10percent %>% 
  #filter(species=="sablefish") %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = proportion_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)
ggsave("figures/CO2200_greater_than_10percent.pdf", height = 6, width = 9)

write_csv(CO2200_greater_than_10percent, "processed_data/CO2200_greater_than_10percent.csv")
write_csv(CO2bot_greater_than_10percent, "processed_data/CO2bot_greater_than_10percent.csv")
