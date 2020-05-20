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
library(tidyverse)
library(broom)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

#read in sensitivity by study
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")
names(sensitivity_by_group)
#read in Sam's maps
bigoxy200<-read_csv("raw_data/downscaled_climate_data/12km_delta_oxy_200m.csv", col_names=F)
bigoxybot<-read_csv("raw_data/downscaled_climate_data/12km_delta_oxy_bot.csv", col_names=F)
bigoxysurf<-read_csv("raw_data/downscaled_climate_data/12km_delta_oxy_surf.csv", col_names=F)

#reshape these into a long dataframe - 200 layer
bigoxy200.df<-melt(bigoxy200) %>%
  mutate(lat=rep(1:dim(bigoxy200)[1], dim(bigoxy200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

#looks like 0's were used rather than NaN over land forthe other 2 layers, so
land<-filter(bigoxy200.df, value=="NaN") # make a vector of land cells

#reshape these into a long dataframe - bottom layer
bigoxybot.df<-melt(bigoxybot) %>%
  mutate(lat=rep(1:dim(bigoxybot)[1], dim(bigoxybot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  mutate(value=ifelse(latlong %in% land$latlong, NaN, value)) #this dataset has 0's instead of NaN over land
  
#filter(bigoxybot.df, latlong=="259_188") #test

#reshape these into a long dataframe - surface layer
bigoxysurf.df<-melt(bigoxysurf) %>%
  mutate(lat=rep(1:dim(bigoxysurf)[1], dim(bigoxysurf)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  mutate(value=ifelse(latlong %in% land$latlong, NaN, value)) #land
#filter(bigoxysurf.df, latlong=="259_188") #test

# calculate delta response for each species
#oxy - extract species with oxy response data and add rows for other species
#and change species names so they will work as column names 
oxy_sens<-sensitivity_by_group %>%
  filter(treatment_var=="oxygen") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity, species name and respone type, and zone
oxy_sens<-oxy_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>%
  select(unique_response, weighted_sensitivity, zone) 

#expand out these responses to the entire dataframe of oxygen change
all_oxy_responses<-expand.grid(unique_response=oxy_sens$unique_response, 
                                latlong=bigoxybot.df$latlong)

#merge empty grid with environmental change data in a particular zone
#then merge with sensitivities for data filtered to that zone
#start with benthic
join_grid_and_botoxy<-left_join(all_oxy_responses, bigoxybot.df)
join_response_and_botoxy<-left_join(join_grid_and_botoxy,filter(oxy_sens, zone=="benthic"))
head(join_response_and_botoxy)

#then mesopelagic
join_grid_and_200oxy<-left_join(all_oxy_responses, bigoxy200.df)
join_response_and_200oxy<-left_join(join_grid_and_200oxy, filter(oxy_sens, zone=="mesopelagic"))
head(join_response_and_200oxy)

#then pelagic
join_grid_and_surfoxy<-left_join(all_oxy_responses, bigoxysurf.df)
join_response_and_surfoxy<-left_join(join_grid_and_surfoxy, filter(oxy_sens, zone=="pelagic"))
head(join_response_and_200oxy)

#combine responses into relevant layers
oxy_10_relevant_layer_big<-rbind(join_response_and_botoxy %>%
                                   filter(zone == "benthic"),
                                 join_response_and_200oxy %>%
                                   filter(zone == "mesopelagic"),
                                 join_response_and_surfoxy %>%
                                   filter(zone == "pelagic"))

#make new column with % change
oxy_10_relevant_layer_response_big<-oxy_10_relevant_layer_big%>%
  mutate(percentchangeoxy=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeoxy=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
oxyrel_greater_than_10percent_big<-oxy_10_relevant_layer_response_big %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(pos_number_over_10=length(which(percentchangeoxy>10)),
            neg_number_over_10=length(which(percentchangeoxy<(-10))),
            abs_number_over_10=length(which(abs(percentchangeoxy)>10)),
            num_responses=length(which(percentchangeoxy!="NA"))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(pos_number_over_10=as.numeric(ifelse(   #get NAs in
    no_env_data=="no_data", "NA", pos_number_over_10))) %>%
  mutate(neg_number_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", neg_number_over_10))) %>%
  mutate(abs_number_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", abs_number_over_10)))

filter(oxyrel_greater_than_10percent_big, no_env_data=="no_data")

write_csv(oxyrel_greater_than_10percent_big, "processed_data/oxyrel_greater_than_10percent_big.csv")

