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

#read in sensitivity by study
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")
sensitivity_by_group$treatment_var
#read in Sam's maps
bigpH200<-read_csv("raw_data/downscaled_climate_data/12km_delta_pH_200m.csv", col_names=F)
bigpHbot<-read_csv("raw_data/downscaled_climate_data/12km_delta_pH_bot.csv", col_names=F)
bigpHsurf<-read_csv("raw_data/downscaled_climate_data/12km_delta_pH_surf.csv", col_names=F)

#reshape these into a long dataframe - 200 layer
bigpH200.df<-melt(bigpH200) %>%
  mutate(lat=rep(1:dim(bigpH200)[1], dim(bigpH200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

#looks like 0's were used rather than NaN over land forthe other 2 layers, so
land<-filter(bigpH200.df, value=="NaN") # make a vector of land cells

#reshape these into a long dataframe - bottom layer
bigpHbot.df<-melt(bigpHbot) %>%
  mutate(lat=rep(1:dim(bigpHbot)[1], dim(bigpHbot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  mutate(value=ifelse(latlong %in% land$latlong, NaN, value)) #this dataset has 0's instead of NaN over land
  
#filter(bigpHbot.df, latlong=="259_188") #test

#reshape these into a long dataframe - surface layer
bigpHsurf.df<-melt(bigpHsurf) %>%
  mutate(lat=rep(1:dim(bigpHsurf)[1], dim(bigpHsurf)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  mutate(value=ifelse(latlong %in% land$latlong, NaN, value)) #land
#filter(bigpHsurf.df, latlong=="259_188") #test

# calculate delta response for each species
#pH - extract species with pH response data and add rows for other species
#and change species names so they will work as column names 
pH_sens<-sensitivity_by_group %>%
  filter(treatment_var=="pH") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity, species name and respone type, and zone
pH_sens<-pH_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>%
  select(unique_response, weighted_sensitivity, zone) 

#expand out these responses to the entire dataframe of pHgen change
all_pH_responses<-expand.grid(unique_response=pH_sens$unique_response, 
                                latlong=bigpHbot.df$latlong)

#merge empty grid with environmental change data in a particular zone
#then merge with sensitivities for data filtered to that zone
#start with benthic
join_grid_and_botpH<-left_join(all_pH_responses, bigpHbot.df)
join_response_and_botpH<-left_join(join_grid_and_botpH,filter(pH_sens, zone=="benthic"))
head(join_response_and_botpH)

#then mesopelagic
join_grid_and_200pH<-left_join(all_pH_responses, bigpH200.df)
join_response_and_200pH<-left_join(join_grid_and_200pH, filter(pH_sens, zone=="mesopelagic"))
head(join_response_and_200pH)

#then pelagic
join_grid_and_surfpH<-left_join(all_pH_responses, bigpHsurf.df)
join_response_and_surfpH<-left_join(join_grid_and_surfpH, filter(pH_sens, zone=="pelagic"))
head(join_response_and_200pH)

#combine responses into relevant layers
pH_10_relevant_layer_big<-rbind(join_response_and_botpH %>%
                                   filter(zone == "benthic"),
                                 join_response_and_200pH %>%
                                   filter(zone == "mesopelagic"),
                                 join_response_and_surfpH %>%
                                   filter(zone == "pelagic"))

#make new column with % change
pH_10_relevant_layer_response_big<-pH_10_relevant_layer_big%>%
  mutate(percentchangepH=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangepH=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
pHrel_greater_than_percent_big<-pH_10_relevant_layer_response_big %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(pos_number_over_10=length(which(percentchangepH>=10)),
            neg_number_over_10=length(which(percentchangepH<=(-10))),
            abs_number_over_10=length(which(abs(percentchangepH)>=10)),
            pos_number_over_20=length(which(percentchangepH>=20)),
            neg_number_over_20=length(which(percentchangepH<=(-20))),
            abs_number_over_20=length(which(abs(percentchangepH)>=20)),
            pos_number_over_30=length(which(percentchangepH>=30)),
            neg_number_over_30=length(which(percentchangepH<=(-30))),
            abs_number_over_30=length(which(abs(percentchangepH)>=30)),
            num_responses=length(which(percentchangepH!="NA"))) %>%
  separate(latlong, sep="_", into=c("lat", "long"))

#make 0's when there is no data into NA
pHrel_greater_than_percent_big[which(pHrel_greater_than_percent_big$no_env_data=="no_data"),
                                str_which(names(pHrel_greater_than_percent_big), "number_over")]<-NA


write_csv(pHrel_greater_than_percent_big, "processed_data/pHrel_greater_than_percent_big.csv")

