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
library(tidyverse)
library(broom)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

#read in sensitivity by study
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")
names(sensitivity_by_group)
#read in Sam's maps
bigCO2200<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pCO2_200m.csv", col_names=F)
bigCO2bot<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pCO2_bot.csv", col_names=F)
bigCO2surf<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pCO2_surf.csv", col_names=F)


#reshape these into a long dataframe - 200 layer
bigCO2200.df<-melt(bigCO2200) %>%
  mutate(lat=rep(1:dim(bigCO2200)[1], dim(bigCO2200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

#reshape these into a long dataframe - bottom layer
bigCO2bot.df<-melt(bigCO2bot) %>%
  mutate(lat=rep(1:dim(bigCO2bot)[1], dim(bigCO2bot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long)) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(latlong=paste(long, lat, sep="_"))


#reshape these into a long dataframe - surface layer
bigCO2surf.df<-melt(bigCO2surf) %>%
  mutate(lat=rep(1:dim(bigCO2surf)[1], dim(bigCO2surf)[2])) %>%
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

#make a dataframe with weighted sensitivity, species name and respone type, and zone
CO2_sens<-CO2_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity, zone)  

#expand out these responses to the entire dataframe of CO2 change, 
#so every unique response has a lat_long 
all_CO2_responses<-expand.grid(unique_response=CO2_sens$unique_response, 
                                latlong=bigCO2bot.df$latlong)

#merge empty grid with environmental change data in a particular zone
#then merge with sensitivities for data filtered to that zone
#start with benthic
join_grid_and_botCO2<-left_join(all_CO2_responses, bigCO2bot.df)
join_response_and_botCO2<-left_join(join_grid_and_botCO2,filter(CO2_sens, zone=="benthic"))

#then mesopelagic
join_grid_and_200CO2<-left_join(all_CO2_responses, bigCO2200.df)
join_response_and_200CO2<-left_join(join_grid_and_200CO2, filter(CO2_sens, zone=="mesopelagic"))
head(join_response_and_200CO2)

#then pelagic
join_grid_and_surfCO2<-left_join(all_CO2_responses, bigCO2surf.df)
join_response_and_surfCO2<-left_join(join_grid_and_surfCO2, filter(CO2_sens, zone=="pelagic"))
head(join_response_and_surfCO2)


#combine responses into relevant layers
CO2_10_relevant_layer_big<-rbind(join_response_and_botCO2 %>%
                               filter(zone == "benthic"),
                             join_response_and_200CO2 %>%
                               filter(zone == "mesopelagic"),
                             join_response_and_surfCO2 %>%
                               filter(zone == "pelagic"))

#make new column with % change
CO2_10_relevant_layer_response_big<-CO2_10_relevant_layer_big%>%
  mutate(percentchangeCO2=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeCO2=="NaN", "no_data", "yes_data"))


#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
CO2rel_greater_than_percent_big<-CO2_10_relevant_layer_response_big %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(pos_number_over_10=length(which(percentchangeCO2>=10)),
            neg_number_over_10=length(which(percentchangeCO2<=(-10))),
            abs_number_over_10=length(which(abs(percentchangeCO2)>=10)),
            pos_number_over_20=length(which(percentchangeCO2>=20)),
            neg_number_over_20=length(which(percentchangeCO2<=(-20))),
            abs_number_over_20=length(which(abs(percentchangeCO2)>=20)),
            pos_number_over_30=length(which(percentchangeCO2>=30)),
            neg_number_over_30=length(which(percentchangeCO2<=(-30))),
            abs_number_over_30=length(which(abs(percentchangeCO2)>=30)),
            num_responses=length(which(percentchangeCO2!="NA"))) %>%
  separate(latlong, sep="_", into=c("lat", "long"))


#make 0's when there is no data into NA
CO2rel_greater_than_percent_big[which(CO2rel_greater_than_percent_big$no_env_data=="no_data"),
                            str_which(names(CO2rel_greater_than_percent_big), "number_over")]<-NA

#check that it worked
CO2rel_greater_than_percent_big %>%
  filter(no_env_data=="no_data")
CO2rel_greater_than_percent_big %>%
  filter(abs_number_over_30>0)

write_csv(CO2rel_greater_than_percent_big, "processed_data/CO2rel_greater_than_percent_big.csv")

