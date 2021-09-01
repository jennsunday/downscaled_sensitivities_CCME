# Goal: 
# was: where is there enough change to elicit 10% change in 1,2,3, .. 9 species in at least one biological response?
# in 30%?

#libraries
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

#read in sensitivity by study
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")

#read in benthic map 2km
littleoxybot<-read_csv("raw_data/downscaled_climate_data/2km_delta_oxy_bot.csv", col_names=F)

#reshape these into a long dataframe - bot layer
littleoxybot.df<-melt(littleoxybot) %>%
  mutate(lat=rep(1:dim(littleoxybot)[1], dim(littleoxybot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#oxy - extract benthic species with oxy response data and add rows for other species
#and change species names so they will work as column names 
oxy_sens<-sensitivity_by_group %>%
  filter(treatment_var=="oxygen",
         zone=="benthic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
oxy_sens<-oxy_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of oxy change, so every unique response has a lat_long
all_oxy_responses<-expand.grid(unique_response=oxy_sens$unique_response, 
                               latlong=littleoxybot.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_botoxy<-left_join(all_oxy_responses, littleoxybot.df)
join_response_and_botoxy<-left_join(join_grid_and_botoxy,oxy_sens)
head(join_response_and_botoxy)

#make new column with % change
join_response_and_botoxy<-join_response_and_botoxy%>%
  mutate(percentchangeoxy=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeoxy=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
oxybot_responses<-join_response_and_botoxy %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeoxy)>10)),
            number_over_20=length(which(abs(percentchangeoxy)>20)),
            number_over_30=length(which(abs(percentchangeoxy)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_20)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_30)))

oxybot_responses %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_20)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(oxybot_responses, "processed_data/oxybot_responses_2km.csv")

#read in 200 map
littleoxy200<-read_csv("processed_data/2km_delta_oxy_200m.csv", col_names=F)
#
#
#
#reshape these into a long dataframe - 200 layer
littleoxy200.df<-melt(littleoxy200) %>%
  mutate(lat=rep(1:dim(littleoxy200)[1], dim(littleoxy200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#oxy - extract benthic species with oxy response data and add rows for other species
#and change species names so they will work as column names 
oxy_sens<-sensitivity_by_group %>%
  filter(treatment_var=="oxygen",
         zone=="pelagic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
oxy_sens<-oxy_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of oxy change, so every unique response has a lat_long
all_oxy_responses<-expand.grid(unique_response=oxy_sens$unique_response, 
                               latlong=littleoxy200.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_200oxy<-left_join(all_oxy_responses, littleoxy200.df)
join_response_and_200oxy<-left_join(join_grid_and_200oxy,oxy_sens)
head(join_response_and_200oxy)

#make new column with % change
join_response_and_200oxy<-join_response_and_200oxy%>%
  mutate(percentchangeoxy=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeoxy=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
oxy200_responses<-join_response_and_200oxy %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeoxy)>10)),
            number_over_20=length(which(abs(percentchangeoxy)>20)),
            number_over_30=length(which(abs(percentchangeoxy)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_20)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_30)))

oxy200_responses %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(oxy200_responses, "processed_data/oxy200_responses_2km.csv")

#
#
#
#read in 12km bottom
oxybot_12<-read_csv("processed_data/12km_delta_oxy_bot.csv", col_names=F)

#reshape these into a long dataframe - bot layer
oxybot_12.df<-melt(oxybot_12) %>%
  mutate(lat=rep(1:dim(oxybot_12)[1], dim(oxybot_12)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#oxy - extract benthic species with oxy response data and add rows for other species
#and change species names so they will work as column names 
oxy_sens<-sensitivity_by_group %>%
  filter(treatment_var=="oxygen",
         zone=="benthic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
oxy_sens<-oxy_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of oxy change, so every unique response has a lat_long
all_oxy_responses_12<-expand.grid(unique_response=oxy_sens$unique_response, 
                               latlong=oxybot_12.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_botoxy_12<-left_join(all_oxy_responses_12, oxybot_12.df)
join_response_and_botoxy_12<-left_join(join_grid_and_botoxy_12,oxy_sens)
head(join_response_and_botoxy_12)

#make new column with % change
join_response_and_botoxy_12<-join_response_and_botoxy_12%>%
  mutate(percentchangeoxy=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeoxy=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
oxybot_responses_12<-join_response_and_botoxy_12 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeoxy)>10)),
            number_over_20=length(which(abs(percentchangeoxy)>20)),
            number_over_30=length(which(abs(percentchangeoxy)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                                   "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                                     "NA", number_over_20)),
         number_over_30=as.numeric(ifelse(no_env_data=="no_data", 
                                                     "NA", number_over_30)))

oxybot_responses_12 %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(oxybot_responses_12, "processed_data/oxybot_responses_12km.csv")
#
#
#
#read in 12km 200tom
oxy200_12<-read_csv("processed_data/12km_delta_oxy_200m.csv", col_names=F)

#reshape these into a long dataframe - 200 layer
oxy200_12.df<-melt(oxy200_12) %>%
  mutate(lat=rep(1:dim(oxy200_12)[1], dim(oxy200_12)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#oxy - extract benthic species with oxy response data and add rows for other species
#and change species names so they will work as column names 
oxy_sens<-sensitivity_by_group %>%
  filter(treatment_var=="oxygen",
         zone=="pelagic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
oxy_sens<-oxy_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of oxy change, so every unique response has a lat_long
all_oxy_responses_12<-expand.grid(unique_response=oxy_sens$unique_response, 
                                  latlong=oxy200_12.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_200oxy_12<-left_join(all_oxy_responses_12, oxy200_12.df)
join_response_and_200oxy_12<-left_join(join_grid_and_200oxy_12,oxy_sens)
head(join_response_and_200oxy_12)

#make new column with % change
join_response_and_200oxy_12<-join_response_and_200oxy_12%>%
  mutate(percentchangeoxy=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeoxy=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
oxy200_responses_12<-join_response_and_200oxy_12 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeoxy)>10)),
            number_over_20=length(which(abs(percentchangeoxy)>20)),
            number_over_30=length(which(abs(percentchangeoxy)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long))  %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
           number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                            "NA", number_over_20)),
           number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                            "NA", number_over_30)))

oxy200_responses_12 %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(oxy200_responses_12, "processed_data/oxy200_responses_12km.csv")
