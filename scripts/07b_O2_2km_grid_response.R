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
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")
names(sensitivity_by_group)

#oxygen conversion from mmol/m3 to ml/l
conv_oxy<-(1000/1026)*(1+26.8/1000)*22.414/1000

#read in Sam's maps
littleoxy200<-read_csv("raw_data/downscaled_climate_data/2km_delta_oxy_200m.csv", col_names=F)*conv_oxy
littleoxybot<-read_csv("raw_data/downscaled_climate_data/2km_delta_oxy_bot.csv", col_names=F)*conv_oxy
littleoxysurf<-read_csv("raw_data/downscaled_climate_data/2km_delta_oxy_surf.csv", col_names=F)*conv_oxy


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

#reshape these into a long dataframe - surface layer
littleoxysurf.df<-melt(littleoxysurf) %>%
  mutate(lat=rep(1:dim(littleoxysurf)[1], dim(littleoxysurf)[2])) %>%
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
  select(unique_response, weighted_sensitivity, zone) 

#expand out these responses to the entire dataframe of oxyerature change
all_oxy_responses<-expand.grid(unique_response=oxy_sens$unique_response, 
                                latlong=littleoxybot.df$latlong)

#merge grid with data from two dataframes
join_grid_and_botoxy<-left_join(all_oxy_responses, littleoxybot.df)
join_response_and_botoxy<-left_join(join_grid_and_botoxy,filter(oxy_sens, zone=="benthic"))
head(join_response_and_botoxy)

join_grid_and_200oxy<-left_join(all_oxy_responses, littleoxy200.df)
join_response_and_200oxy<-left_join(join_grid_and_200oxy,filter(oxy_sens, zone=="mesopelagic"))
head(join_response_and_200oxy)

join_grid_and_surfoxy<-left_join(all_oxy_responses, littleoxysurf.df)
join_response_and_surfoxy<-left_join(join_grid_and_surfoxy,filter(oxy_sens, zone=="pelagic"))
head(join_response_and_200oxy)


#combine responses into relevant layers
oxy_10_relevant_layer<-rbind(join_response_and_botoxy %>%
                               filter(zone == "benthic"),
                             join_response_and_200oxy %>%
                               filter(zone == "mesopelagic"),
                             join_response_and_surfoxy %>%
                               filter(zone == "pelagic"))

#make new column with % change
oxy_10_relevant_layer_response<-oxy_10_relevant_layer%>%
  mutate(percentchangeoxy=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeoxy=="NaN", "no_data", "yes_data"))


#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
oxyrel_greater_than_percent<-oxy_10_relevant_layer_response %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(latlong, species, no_env_data) %>%
  summarise(pos_number_over_10=length(which(percentchangeoxy>=10)),
            neg_number_over_10=length(which(percentchangeoxy<=(-10))),
            abs_number_over_10=length(which(abs(percentchangeoxy)>=10)),
            pos_number_over_20=length(which(percentchangeoxy>=20)),
            neg_number_over_20=length(which(percentchangeoxy<=(-20))),
            abs_number_over_20=length(which(abs(percentchangeoxy)>=20)),
            pos_number_over_30=length(which(percentchangeoxy>=30)),
            neg_number_over_30=length(which(percentchangeoxy<=(-30))),
            abs_number_over_30=length(which(abs(percentchangeoxy)>=30)),
            num_responses=length(which(percentchangeoxy!="NA"))) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) 

#make 0's back into NA in grid cells with no data
oxyrel_greater_than_percent[which(oxyrel_greater_than_percent$no_env_data=="no_data"),
                            str_which(names(oxyrel_greater_than_percent), "number_over")]<-NA


write_csv(oxyrel_greater_than_percent, "processed_data/oxyrel_greater_than_percent.csv")

oxyrel_greater_than_percent %>% filter(species=="sablefish" &
                                         neg_number_over_20>0)
