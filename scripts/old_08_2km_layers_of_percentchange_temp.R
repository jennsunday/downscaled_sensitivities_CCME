# Goal: 
# where is there enough change to elicit 10% change in one biological response?
# where is there enough change to elicit 10% change in two biological responses?
# where is there enough change to elict 10% change in three biological responses?
# where is there enough change to elict 10% change in four biological responses?

# approach
# A) for each species in relevant layer
# first ask, for each grid cell, how many absolute biological responses are greater than 10% on the temperature field?
# next ask, for each grid cell, how many absolute biological responses are greater than 10% on the CO2 field?
# third, for each grid cell, ask how many absolute biological responses are greater than 10% in the O2 field?
# fourth, add the number of responses across all three layers. Make a heat map of this.

# B) one map: add up how many reponses are greater than 10% across all species and variables
# C) three maps: add up how many reponses are greater than 10% across all species for each variable

#libraries
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(cowplot)
library(maps)
library(reshape2)
library(mapdata)
library(gridExtra)
library(RColorBrewer)


#read in sensitivity by study
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")
names(sensitivity_by_group)
#read in Sam's maps
littletemp200<-read_csv("processed_data/2km_delta_temp_200m.csv", col_names=F)
littletempbot<-read_csv("processed_data/2km_delta_temp_bot.csv", col_names=F)


#reshape these into a long dataframe
littletemp200.df<-melt(littletemp200) %>%
  mutate(lat=rep(1:dim(littletemp200)[1], dim(littletemp200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) 

#land values are "NaN"


#reshape these into a long dataframe
littletempbot.df<-melt(littletempbot) %>%
  mutate(lat=rep(1:dim(littletempbot)[1], dim(littletempbot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#temp - extract species with temp response data and add rows for other species
#and change species names so they will work as column names 
temp_sens<-sensitivity_by_group %>%
  filter(treatment_var=="temperature") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
temp_sens<-temp_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>%
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of temperature change
all_temp_responses<-expand.grid(unique_response=temp_sens$unique_response, 
                                latlong=littletempbot.df$latlong)

head(filter(littletempbot.df, value==0))
#merge grid with data from two dataframes
join_grid_and_bottemp<-left_join(all_temp_responses, littletempbot.df)
join_response_and_bottemp<-left_join(join_grid_and_bottemp,temp_sens)
head(join_response_and_bottemp)


join_grid_and_200temp<-left_join(all_temp_responses, littletemp200.df)
join_response_and_200temp<-left_join(join_grid_and_200temp,temp_sens)
head(join_response_and_200temp)


#make new column with % change
join_response_and_bottemp<-join_response_and_bottemp%>%
  mutate(percentchangetemp=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangetemp=="NaN", "no_data", "yes_data"))

join_response_and_200temp<-join_response_and_200temp%>%
  mutate(percentchangetemp=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangetemp=="NaN", "no_data", "yes_data"))

#how many responses observed for each species?
temp_N_responses_by_species<-temp_sens %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species) %>%
  summarize(number_responses=n())

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
#also ask where environmental value is 0 to demark land
tempbot_greater_than_10percent<-join_response_and_bottemp %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(latlong, species, no_env_data) %>%
  summarize(number_over_10=length(which(abs(percentchangetemp)>10))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  left_join(., temp_N_responses_by_species) %>%
  mutate(proportion_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", number_over_10/number_responses))) #get NAs in
head(filter(tempbot_greater_than_10percent, no_env_data=="no_data"))

temp200_greater_than_10percent<-join_response_and_200temp %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(latlong, species, no_env_data) %>%
  summarize(number_over_10=length(which(abs(percentchangetemp)>10))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  left_join(., temp_N_responses_by_species) %>%
  mutate(proportion_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", number_over_10/number_responses))) #get NAs in
dim(filter(temp200_greater_than_10percent, no_env_data=="no_data"))


tempbot_greater_than_10percent %>% 
  #filter(species=="sablefish") %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = proportion_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)
ggsave("figures/tempbot_greater_than_10percent.pdf", height = 6, width = 9)


temp200_greater_than_10percent %>% 
  #filter(species=="sablefish") %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = proportion_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)
ggsave("figures/temp200_greater_than_10percent.pdf", height = 6, width = 9)

write_csv(temp200_greater_than_10percent, "processed_data/temp200_greater_than_10percent.csv")
write_csv(tempbot_greater_than_10percent, "processed_data/tempbot_greater_than_10percent.csv")
