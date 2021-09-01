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
littleCO2bot<-read_csv("processed_data/2km_delta_pCO2_bot.csv", col_names=F)

#reshape these into a long dataframe - bot layer
littleCO2bot.df<-melt(littleCO2bot) %>%
  mutate(lat=rep(1:dim(littleCO2bot)[1], dim(littleCO2bot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#CO2 - extract benthic species with CO2 response data and add rows for other species
#and change species names so they will work as column names 
CO2_sens<-sensitivity_by_group %>%
  filter(treatment_var=="CO2",
         zone=="benthic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
CO2_sens<-CO2_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of CO2 change, so every unique response has a lat_long
all_CO2_responses<-expand.grid(unique_response=CO2_sens$unique_response, 
                               latlong=littleCO2bot.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_botCO2<-left_join(all_CO2_responses, littleCO2bot.df)
join_response_and_botCO2<-left_join(join_grid_and_botCO2,CO2_sens)
head(join_response_and_botCO2)

#make new column with % change
join_response_and_botCO2<-join_response_and_botCO2%>%
  mutate(percentchangeCO2=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeCO2=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
CO2bot_responses<-join_response_and_botCO2 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeCO2)>10)),
            number_over_20=length(which(abs(percentchangeCO2)>20)),
            number_over_30=length(which(abs(percentchangeCO2)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_20)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_30)))

CO2bot_responses %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(CO2bot_responses, "processed_data/CO2bot_responses_2km.csv")

#
#
#read in 200 map
littleCO2200<-read_csv("processed_data/2km_delta_pCO2_200m.csv", col_names=F)

#reshape these into a long dataframe - 200 layer
littleCO2200.df<-melt(littleCO2200) %>%
  mutate(lat=rep(1:dim(littleCO2200)[1], dim(littleCO2200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#CO2 - extract benthic species with CO2 response data and add rows for other species
#and change species names so they will work as column names 
CO2_sens<-sensitivity_by_group %>%
  filter(treatment_var=="CO2",
         zone=="pelagic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
CO2_sens<-CO2_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of CO2 change, so every unique response has a lat_long
all_CO2_responses<-expand.grid(unique_response=CO2_sens$unique_response, 
                               latlong=littleCO2200.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_200CO2<-left_join(all_CO2_responses, littleCO2200.df)
join_response_and_200CO2<-left_join(join_grid_and_200CO2,CO2_sens)
head(join_response_and_200CO2)

#make new column with % change
join_response_and_200CO2<-join_response_and_200CO2%>%
  mutate(percentchangeCO2=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeCO2=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
CO2200_responses<-join_response_and_200CO2 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeCO2)>10)),
            number_over_20=length(which(abs(percentchangeCO2)>20)),
            number_over_30=length(which(abs(percentchangeCO2)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_20)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_30)))

CO2200_responses %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(CO2200_responses, "processed_data/CO2200_responses_2km.csv")


#
#
#
#read in 12km bottom
CO2bot_12<-read_csv("processed_data/12km_delta_pCO2_bot.csv", col_names=F)

#reshape these into a long dataframe - bot layer
CO2bot_12.df<-melt(CO2bot_12) %>%
  mutate(lat=rep(1:dim(CO2bot_12)[1], dim(CO2bot_12)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#CO2 - extract benthic species with CO2 response data and add rows for other species
#and change species names so they will work as column names 
CO2_sens<-sensitivity_by_group %>%
  filter(treatment_var=="CO2",
         zone=="benthic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
CO2_sens<-CO2_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of CO2 change, so every unique response has a lat_long
all_CO2_responses_12<-expand.grid(unique_response=CO2_sens$unique_response, 
                               latlong=CO2bot_12.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_botCO2_12<-left_join(all_CO2_responses_12, CO2bot_12.df)
join_response_and_botCO2_12<-left_join(join_grid_and_botCO2_12,CO2_sens)
head(join_response_and_botCO2_12)

#make new column with % change
join_response_and_botCO2_12<-join_response_and_botCO2_12%>%
  mutate(percentchangeCO2=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeCO2=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
CO2bot_responses_12<-join_response_and_botCO2_12 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeCO2)>10)),
            number_over_20=length(which(abs(percentchangeCO2)>20)),
            number_over_30=length(which(abs(percentchangeCO2)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                                   "NA", number_over_10)),
         number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                                     "NA", number_over_20)),
         number_over_30=as.numeric(ifelse(no_env_data=="no_data", 
                                                     "NA", number_over_30)))

CO2bot_responses_12 %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)

write_csv(CO2bot_responses_12, "processed_data/CO2bot_responses_12km.csv")
#
#
#
#read in 12km 200tom
CO2200_12<-read_csv("processed_data/12km_delta_pCO2_200m.csv", col_names=F)

#reshape these into a long dataframe - 200 layer
CO2200_12.df<-melt(CO2200_12) %>%
  mutate(lat=rep(1:dim(CO2200_12)[1], dim(CO2200_12)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

# calculate delta response for each species
#CO2 - extract benthic species with CO2 response data and add rows for other species
#and change species names so they will work as column names 
CO2_sens<-sensitivity_by_group %>%
  filter(treatment_var=="CO2",
         zone=="pelagic") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
CO2_sens<-CO2_sens %>%
  unite(., col=unique_response, English_Name, response_type, sep="_and_") %>% # make a new column comibining two columns, called unique response
  select(unique_response, weighted_sensitivity) 

#expand out these responses to the entire dataframe of CO2 change, so every unique response has a lat_long
all_CO2_responses_12<-expand.grid(unique_response=CO2_sens$unique_response, 
                                  latlong=CO2200_12.df$latlong)

#merge empty grid with environmental data, so all unique responses have lat_long and environemntal change
join_grid_and_200CO2_12<-left_join(all_CO2_responses_12, CO2200_12.df)
join_response_and_200CO2_12<-left_join(join_grid_and_200CO2_12,CO2_sens)
head(join_response_and_200CO2_12)

#make new column with % change
join_response_and_200CO2_12<-join_response_and_200CO2_12%>%
  mutate(percentchangeCO2=value*weighted_sensitivity*100) %>%
  mutate(no_env_data=ifelse(percentchangeCO2=="NaN", "no_data", "yes_data"))

#Now group by lat_long and species, then ask how many of each group are greater than 10% (need zeros)
CO2200_responses_12<-join_response_and_200CO2_12 %>%
  separate(unique_response, sep="_and_", into=c("species", "response")) %>%
  group_by(species, latlong, no_env_data) %>%
  summarise(number_over_10=length(which(abs(percentchangeCO2)>10)),
            number_over_20=length(which(abs(percentchangeCO2)>20)),
            number_over_30=length(which(abs(percentchangeCO2)>30)),
            number_total=n()) %>%
  separate(latlong, sep="_", into=c("lat", "long")) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long))  %>%
  mutate(number_over_10=as.numeric(ifelse(no_env_data=="no_data", 
                                          "NA", number_over_10)),
           number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                            "NA", number_over_20)),
           number_over_20=as.numeric(ifelse(no_env_data=="no_data", 
                                            "NA", number_over_30)))

CO2200_responses_12 %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = number_over_10)) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species, nrow=2)


write_csv(CO2200_responses_12, "processed_data/CO2200_responses_12km.csv")
