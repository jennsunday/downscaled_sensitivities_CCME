# Goal: 
# was: where is there enough change to elicit 10% change in 1,2,3, .. 9 species in at least one biological response?
# in 30%?

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

#read in sensitivity by study
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")

#read in benthic map 2km
littletempbot<-read_csv("processed_data/2km_delta_temp_bot.csv", col_names=F)

#reshape these into a long dataframe - bot layer
littletempbot.df<-melt(littletempbot) %>%
  mutate(lat=rep(1:dim(littletempbot)[1], dim(littletempbot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#temp - extract benthic species with temp response data and add rows for other species
#and change species names so they will work as column names 
temp_sens<-sensitivity_by_group %>%
  filter(treatment_var=="temperature",
         zone=="benthic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
temp_sens<-temp_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of temp change, so every unique response has a lat_long
all_temp_responses<-expand.grid(unique_response=temp_sens$unique_response, 
                               latlong=littletempbot.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_bottemp<-left_join(all_temp_responses, littletempbot.df)
join_response_and_bottemp<-left_join(join_grid_and_bottemp,temp_sens)
head(join_response_and_bottemp)

#make new column with % change
join_response_and_bottemp<-join_response_and_bottemp%>%
  mutate(percentchangetemp=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangetemp=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
tempbot_responses<-join_response_and_bottemp %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangetemp)>10)),
            number_over_20=length(which(abs(percentchangetemp)>20)),
            number_over_30=length(which(abs(percentchangetemp)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_20)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_30)))

tempbot_responses %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(tempbot_responses, "processed_data/tempbot_responses_2km.csv")

#read in 200 map
littletemp200<-read_csv("processed_data/2km_delta_temp_200m.csv", col_names=F)

#reshape these into a long dataframe - 200 layer
littletemp200.df<-melt(littletemp200) %>%
  mutate(lat=rep(1:dim(littletemp200)[1], dim(littletemp200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#temp - extract benthic species with temp response data and add rows for other species
#and change species names so they will work as column names 
temp_sens<-sensitivity_by_group %>%
  filter(treatment_var=="temperature",
         zone=="pelagic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
temp_sens<-temp_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of temp change, so every unique response has a lat_long
all_temp_responses<-expand.grid(unique_response=temp_sens$unique_response, 
                               latlong=littletemp200.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_200temp<-left_join(all_temp_responses, littletemp200.df)
join_response_and_200temp<-left_join(join_grid_and_200temp,temp_sens)
head(join_response_and_200temp)

#make new column with % change
join_response_and_200temp<-join_response_and_200temp%>%
  mutate(percentchangetemp=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangetemp=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
temp200_responses<-join_response_and_200temp %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangetemp)>10)),
            number_over_20=length(which(abs(percentchangetemp)>20)),
            number_over_30=length(which(abs(percentchangetemp)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_20)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_30)))

temp200_responses %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(temp200_responses, "processed_data/temp200_responses_2km.csv")

#
#
#
#read in 12km bottom
tempbot_12<-read_csv("processed_data/12km_delta_temp_bot.csv", col_names=F)

#reshape these into a long dataframe - bot layer
tempbot_12.df<-melt(tempbot_12) %>%
  mutate(lat=rep(1:dim(tempbot_12)[1], dim(tempbot_12)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#temp - extract benthic species with temp response data and add rows for other species
#and change species names so they will work as column names 
temp_sens<-sensitivity_by_group %>%
  filter(treatment_var=="temperature",
         zone=="benthic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
temp_sens<-temp_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of temp change, so every unique response has a lat_long
all_temp_responses_12<-expand.grid(unique_response=temp_sens$unique_response, 
                               latlong=tempbot_12.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_bottemp_12<-left_join(all_temp_responses_12, tempbot_12.df)
join_response_and_bottemp_12<-left_join(join_grid_and_bottemp_12,temp_sens)
head(join_response_and_bottemp_12)

#make new column with % change
join_response_and_bottemp_12<-join_response_and_bottemp_12%>%
  mutate(percentchangetemp=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangetemp=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
tempbot_responses_12<-join_response_and_bottemp_12 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangetemp)>10)),
            number_over_20=length(which(abs(percentchangetemp)>20)),
            number_over_30=length(which(abs(percentchangetemp)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                                   "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                                     "NA", number_over_20)),
         number_over_30=as.numeric(ifelse(no_env_data=="no_data", 
                                                     "NA", number_over_30)))

tempbot_responses_12 %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(tempbot_responses_12, "processed_data/tempbot_responses_12km.csv")
#
#
#
#read in 12km 200tom
temp200_12<-read_csv("processed_data/12km_delta_temp_200m.csv", col_names=F)

#reshape these into a long dataframe - 200 layer
temp200_12.df<-melt(temp200_12) %>%
  mutate(lat=rep(1:dim(temp200_12)[1], dim(temp200_12)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#temp - extract benthic species with temp response data and add rows for other species
#and change species names so they will work as column names 
temp_sens<-sensitivity_by_group %>%
  filter(treatment_var=="temperature",
         zone=="pelagic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
temp_sens<-temp_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of temp change, so every unique response has a lat_long
all_temp_responses_12<-expand.grid(unique_response=temp_sens$unique_response, 
                                  latlong=temp200_12.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_200temp_12<-left_join(all_temp_responses_12, temp200_12.df)
join_response_and_200temp_12<-left_join(join_grid_and_200temp_12,temp_sens)
head(join_response_and_200temp_12)

#make new column with % change
join_response_and_200temp_12<-join_response_and_200temp_12%>%
  mutate(percentchangetemp=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangetemp=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
temp200_responses_12<-join_response_and_200temp_12 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangetemp)>10)),
            number_over_20=length(which(abs(percentchangetemp)>20)),
            number_over_30=length(which(abs(percentchangetemp)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long))  %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
           number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                            "NA", number_over_20)),
           number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                            "NA", number_over_30)))

temp200_responses_12 %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(temp200_responses_12, "processed_data/temp200_responses_12km.csv")
