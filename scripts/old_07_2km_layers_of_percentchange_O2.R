# Goal: 
# where is there enough change to elicit 10% change in one biological response?
# where is there enough change to elicit 10% change in two biological responses?
# where is there enough change to elict 10% change in three biological responses?
# where is there enough change to elict 10% change in four biological responses?

# approach
# A) for each species in relevant layer
# first ask, for each grid cell, how many absolute biological responses are greater than 10% on the oxyerature field?
# next ask, for each grid cell, how many absolute biological responses are greater than 10% on the oxy field?
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
littleoxy200<-read_csv("processed_data/2km_delta_oxy_200m.csv", col_names=F)
littleoxybot<-read_csv("processed_data/2km_delta_oxy_bot.csv", col_names=F)

#reshape these into a long dataframe
littleoxy200.df<-melt(littleoxy200) %>%
  mutate(lat=rep(1:dim(littleoxy200)[1], dim(littleoxy200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))


#reshape these into a long dataframe
littleoxybot.df<-melt(littleoxybot) %>%
  mutate(lat=rep(1:dim(littleoxybot)[1], dim(littleoxybot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#oxy - extract species with oxy response data and add rows for other species
#and change species names so they will work as column names 
oxy_sens<-sensitivity_by_group %>%
  filter(treatment_var=="oxygen") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
oxy_sens<-oxy_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>%
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of oxyerature change
all_oxy_responses<-expand.grid(unique_response=oxy_sens$unique_response, 
                                latlong=littleoxybot.df$latlong)

#merge grid with data from two dataframes
join_grid_and_botoxy<-left_join(all_oxy_responses, littleoxybot.df)
join_response_and_botoxy<-left_join(join_grid_and_botoxy,oxy_sens)
head(join_response_and_botoxy)

join_grid_and_200oxy<-left_join(all_oxy_responses, littleoxy200.df)
join_response_and_200oxy<-left_join(join_grid_and_200oxy,oxy_sens)
head(join_response_and_200oxy)

#make new column with % change
join_response_and_botoxy<-join_response_and_botoxy%>%
  mutate(percentchangeoxy=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeoxy=="NaN", "no_data", "yes_data"))

join_response_and_200oxy<-join_response_and_200oxy%>%
  mutate(percentchangeoxy=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeoxy=="NaN", "no_data", "yes_data"))

#how many responses observed for each species?
oxy_N_responses_by_species<-oxy_sens %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species) %>%
  summarize(number_responses=n())

head(filter(join_response_and_botoxy, no_env_data=="no_data"))
#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
oxybot_greater_than_10percent<-join_response_and_botoxy %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(latlong, species, no_env_data) %>%
  summarize(number_over_10=length(which(abs(percentchangeoxy)>10))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  left_join(., oxy_N_responses_by_species) %>%
  mutate(proportion_over_10=as.numeric(ifelse(no_env_data=="no_data", "NA", number_over_10/number_responses))) #get NAs in


oxy200_greater_than_10percent<-join_response_and_200oxy %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(latlong, species, no_env_data) %>%
  summarize(number_over_10=length(which(abs(percentchangeoxy)>10))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  left_join(., oxy_N_responses_by_species) %>%
  mutate(proportion_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", number_over_10/number_responses))) #get NAs in


oxybot_greater_than_10percent %>% 
  #filter(species=="sablefish") %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = proportion_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)
ggsave("figures/oxybot_greater_than_10percent.pdf", height = 6, width = 6)


oxy200_greater_than_10percent %>% 
  #filter(species=="sablefish") %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = proportion_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)
ggsave("figures/oxy200_greater_than_10percent.pdf", height = 6, width = 6)

write_csv(oxy200_greater_than_10percent, "processed_data/oxy200_greater_than_10percent.csv")
write_csv(oxybot_greater_than_10percent, "processed_data/oxybot_greater_than_10percent.csv")
