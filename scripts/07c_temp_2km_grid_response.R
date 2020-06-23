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
library(gridExtra)
library(RColorBrewer)

#read in sensitivity by study
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")
names(sensitivity_by_group)

#read in Sam's maps
littletemp200<-read_csv("raw_data/downscaled_climate_data/2km_delta_temp_200m.csv", col_names=F)
littletempbot<-read_csv("raw_data/downscaled_climate_data/2km_delta_temp_bot.csv", col_names=F)
littletempsurf<-read_csv("raw_data/downscaled_climate_data/2km_delta_temp_surf.csv", col_names=F)

#reshape these into a long dataframe
littletemp200.df<-melt(littletemp200) %>%
  mutate(lat=rep(1:dim(littletemp200)[1], dim(littletemp200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))


#reshape these into a long dataframe
littletempbot.df<-melt(littletempbot) %>%
  mutate(lat=rep(1:dim(littletempbot)[1], dim(littletempbot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_"))

#reshape these into a long dataframe - surface layer
littletempsurf.df<-melt(littletempsurf) %>%
  mutate(lat=rep(1:dim(littletempsurf)[1], dim(littletempsurf)[2])) %>%
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
  select(unique_response, weighted_sensitivity, zone) 

#expand out these responses to the entire dataframe of temperature change
all_temp_responses<-expand.grid(unique_response=temp_sens$unique_response, 
                               latlong=littletempbot.df$latlong)

#merge grid with data from two dataframes
join_grid_and_bottemp<-left_join(all_temp_responses, littletempbot.df)
join_response_and_bottemp<-left_join(join_grid_and_bottemp,filter(temp_sens, zone=="benthic"))
head(join_response_and_bottemp)

join_grid_and_200temp<-left_join(all_temp_responses, littletemp200.df)
join_response_and_200temp<-left_join(join_grid_and_200temp,filter(temp_sens, zone=="mesopelagic"))
head(join_response_and_200temp)

join_grid_and_surftemp<-left_join(all_temp_responses, littletempsurf.df)
join_response_and_surftemp<-left_join(join_grid_and_surftemp,filter(temp_sens, zone=="pelagic"))
head(join_response_and_200temp)


#combine responses into relevant layers
temp_10_relevant_layer<-rbind(join_response_and_bottemp %>%
                               filter(zone == "benthic"),
                             join_response_and_200temp %>%
                               filter(zone == "mesopelagic"),
                             join_response_and_surftemp %>%
                               filter(zone == "pelagic"))

#make new column with % change
temp_10_relevant_layer_response<-temp_10_relevant_layer%>%
  mutate(percentchangetemp=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangetemp=="NaN", "no_data", "yes_data"))



#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
temprel_greater_than_percent<-temp_10_relevant_layer_response %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(latlong, species, no_env_data) %>%
  summarise(pos_number_over_10=length(which(percentchangetemp>=10)),
            neg_number_over_10=length(which(percentchangetemp<=(-10))),
            abs_number_over_10=length(which(abs(percentchangetemp)>=10)),
            pos_number_over_20=length(which(percentchangetemp>=20)),
            neg_number_over_20=length(which(percentchangetemp<=(-20))),
            abs_number_over_20=length(which(abs(percentchangetemp)>=20)),
            pos_number_over_30=length(which(percentchangetemp>=30)),
            neg_number_over_30=length(which(percentchangetemp<=(-30))),
            abs_number_over_30=length(which(abs(percentchangetemp)>=30)),
            num_responses=length(which(percentchangetemp!="NA"))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) 


#make 0's NA in grid cells with no data
temprel_greater_than_percent[which(temprel_greater_than_percent$no_env_data=="no_data"),
                            str_which(names(temprel_greater_than_percent), "number_over")]<-NA


#how many responses observed for each species?
write_csv(temprel_greater_than_percent, "processed_data/temprel_greater_than_percent.csv")
