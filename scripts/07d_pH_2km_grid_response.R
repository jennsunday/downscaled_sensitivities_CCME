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
library(tidyverse)
library(broom)
library(reshape2)


#read in sensitivity by study
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")
names(sensitivity_by_group)

#read in Sam's maps
littlepH200<-read_csv("raw_data/downscaled_climate_data/2km_delta_pH_200m.csv", col_names=F)
littlepHbot<-read_csv("raw_data/downscaled_climate_data/2km_delta_pH_bot.csv", col_names=F)
littlepHsurf<-read_csv("raw_data/downscaled_climate_data/2km_delta_pH_surf.csv", col_names=F)

#reshape these into a long dataframe
littlepH200.df<-melt(littlepH200) %>%
  mutate(lat=rep(1:dim(littlepH200)[1], dim(littlepH200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))


#reshape these into a long dataframe
littlepHbot.df<-melt(littlepHbot) %>%
  mutate(lat=rep(1:dim(littlepHbot)[1], dim(littlepHbot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_"))

#reshape these into a long dataframe - surface layer
littlepHsurf.df<-melt(littlepHsurf) %>%
  mutate(lat=rep(1:dim(littlepHsurf)[1], dim(littlepHsurf)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_"))


# calculate delta response for each species
#pH - extract species with pH response data and add rows for other species
#and change species names so they will work as column names 
pH_sens<-sensitivity_by_group %>%
  filter(treatment_var=="pH") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
pH_sens<-pH_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>%
  select(unique_response, weighted_sensitivity, zone) 

#expand out these responses to the entire dataframe of pHerature change
all_pH_responses<-expand.grid(unique_response=pH_sens$unique_response, 
                               latlong=littlepHbot.df$latlong)

#merge grid with data from two dataframes
join_grid_and_botpH<-left_join(all_pH_responses, littlepHbot.df)
join_response_and_botpH<-left_join(join_grid_and_botpH,filter(pH_sens, zone=="benthic"))
head(join_response_and_botpH)

join_grid_and_200pH<-left_join(all_pH_responses, littlepH200.df)
join_response_and_200pH<-left_join(join_grid_and_200pH,filter(pH_sens, zone=="mesopelagic"))
head(join_response_and_200pH)

join_grid_and_surfpH<-left_join(all_pH_responses, littlepHsurf.df)
join_response_and_surfpH<-left_join(join_grid_and_surfpH,filter(pH_sens, zone=="pelagic"))
head(join_response_and_200pH)


#combine responses into relevant layers
pH_10_relevant_layer<-rbind(join_response_and_botpH %>%
                               filter(zone == "benthic"),
                             join_response_and_200pH %>%
                               filter(zone == "mesopelagic"),
                             join_response_and_surfpH %>%
                               filter(zone == "pelagic"))

#make new column with % change
pH_10_relevant_layer_response<-pH_10_relevant_layer%>%
  mutate(percentchangepH=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangepH=="NaN", "no_data", "yes_data"))



#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
pHrel_greater_than_10percent<-pH_10_relevant_layer_response %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(latlong, species, no_env_data) %>%
  summarize(pos_number_over_10=length(which(percentchangepH>10)),
          neg_number_over_10=length(which(percentchangepH<(-10))),
          abs_number_over_10=length(which(abs(percentchangepH)>10)),
          num_responses=length(which(percentchangepH!="NA"))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(pos_number_over_10=as.numeric(ifelse(   #get NAs in
    no_env_data=="no_data", "NA", pos_number_over_10))) %>%
  mutate(neg_number_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", neg_number_over_10))) %>%
  mutate(abs_number_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", abs_number_over_10)))

#how many responses observed for each species?
write_csv(pHrel_greater_than_10percent, "processed_data/pHrel_greater_than_10percent.csv")
