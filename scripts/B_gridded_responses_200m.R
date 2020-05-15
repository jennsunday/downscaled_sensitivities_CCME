#goal: calculte response by grid, stressor, and response type.

#librarieslibrary(nlme) 

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

#read in sensitivity by study
setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
sensitivity_by_group<-read_csv("processed_data/sensitivity_by_group.csv")
sensitivity_by_species<-read_csv("processed_data/sensitivity_by_species.csv")


#read in one of Sam's maps
littletemp<-read_csv("processed_data/2km_delta_temp_200m.csv", col_names=F)
bigtemp<-read_csv("processed_data/12km_delta_temp_200m.csv", col_names=F)


#reshape these into a long dataframe
littletemp.df<-melt(littletemp) %>%
  mutate(lat=rep(1:dim(littletemp)[1], dim(littletemp)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

dim(littletemp.df) # have a look

#reshape these into a long dataframe
bigtemp.df<-melt(bigtemp) %>%
  mutate(lat=rep(1:dim(bigtemp)[1], dim(bigtemp)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

dim(bigtemp.df) # have a look

#plot
littletemp.df %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = value))
bigtemp.df %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = value))


# calculate delta response for each species
head(littletemp.df)

#temp - extract species with temp response data and add rows for other species
#and change species names so they will work as column names 

temp_sens<-sensitivity_by_species %>%
  filter(treatment_var=="temperature") %>%
  add_row(English_Name=setdiff(unique(sensitivity_by_species$English_Name),unique(.$English_Name)),
          treatment_var="temperature") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

temp_sens$English_Name

#combined <-expand.grid(lat=littletemp.df$lat, species=temp_sens$English_Name) 
#fill_combined <- merge(combined, littletemp.df)

#make a vector with weighted sensitivity titled by species name
temp_sens<-temp_sens %>%
  select(English_Name, weighted_sensitivity) %>%
  spread(., English_Name, weighted_sensitivity)

littletemp_responses<-littletemp.df %>%
  mutate(seagrass=temp_sens$seagrass*value,
         CanopyformingKelp=temp_sens$CanopyformingKelp*value,
         Ochrestar=temp_sens$Ochrestar*value,
         Dungenesscrab=temp_sens$Dungenesscrab*value,
         Redurchin=temp_sens$Redurchin*value,
         RazorClam=temp_sens$RazorClam*value,
         sablefish=temp_sens$sablefish*value,
         blueblackrockfish=temp_sens$blueblackrockfish*value,
         PinkSalmon=temp_sens$PinkSalmon*value,
         copperquillbackrockfish=temp_sens$copperquillbackrockfish*value)


bigtemp_responses<-bigtemp.df %>%
  mutate(seagrass=temp_sens$seagrass*value,
         CanopyformingKelp=temp_sens$CanopyformingKelp*value,
         Ochrestar=temp_sens$Ochrestar*value,
         Dungenesscrab=temp_sens$Dungenesscrab*value,
         Redurchin=temp_sens$Redurchin*value,
         RazorClam=temp_sens$RazorClam*value,
         sablefish=temp_sens$sablefish*value,
         blueblackrockfish=temp_sens$blueblackrockfish*value,
         PinkSalmon=temp_sens$PinkSalmon*value,
         copperquillbackrockfish=temp_sens$copperquillbackrockfish*value)

#try to make these data "long"

littletemp_responses_long<-gather(littletemp_responses, species, response, seagrass:copperquillbackrockfish)
dim(littletemp_responses_long)

bigtemp_responses_long<-gather(bigtemp_responses, species, response, seagrass:copperquillbackrockfish)
dim(bigtemp_responses_long)

limits<-c(-0.3, 0.3)

littletemp_responses_long %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species, nrow=2)
ggsave("figures/littletemp_responses.pdf", height = 6, width = 9)


bigtemp_responses_long %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species, nrow=2) + coord_cartesian(xlim=c(200, 300))
ggsave("figures/bigtemp_responses.pdf", height = 7, width = 9)


#CO2

#read in one of Sam's maps
littleCO2<-read_csv("processed_data/2km_delta_pCO2_200m.csv", col_names=F)
bigCO2<-read_csv("processed_data/12km_delta_pCO2_200m.csv", col_names=F)


#reshape it into a long dataframe
littleCO2.df<-melt(littleCO2) %>%
  mutate(lat=rep(1:dim(littleCO2)[1], dim(littleCO2)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

dim(littleCO2.df)


#reshape it into a long dataframe
bigCO2.df<-melt(bigCO2) %>%
  mutate(lat=rep(1:dim(bigCO2)[1], dim(bigCO2)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))


#CO2 - extract species with CO2 response data and add rows for other species
#and change species names so they will work as column names 

CO2_sens<-sensitivity_by_species %>%
  filter(treatment_var=="CO2") %>%
  add_row(English_Name=setdiff(unique(sensitivity_by_species$English_Name),unique(.$English_Name)),
          treatment_var="CO2") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
CO2_sens<-CO2_sens %>%
  select(English_Name, weighted_sensitivity) %>%
  spread(., English_Name, weighted_sensitivity)

littleCO2_responses<-littleCO2.df %>%
  mutate(seagrass=CO2_sens$seagrass*value,
         CanopyformingKelp=CO2_sens$CanopyformingKelp*value,
         Ochrestar=CO2_sens$Ochrestar*value,
         Dungenesscrab=CO2_sens$Dungenesscrab*value,
         Redurchin=CO2_sens$Redurchin*value,
         RazorClam=CO2_sens$RazorClam*value,
         sablefish=CO2_sens$sablefish*value,
         blueblackrockfish=CO2_sens$blueblackrockfish*value,
         PinkSalmon=CO2_sens$PinkSalmon*value,
         copperquillbackrockfish=CO2_sens$copperquillbackrockfish*value)

bigCO2_responses<-bigCO2.df %>%
  mutate(seagrass=CO2_sens$seagrass*value,
         CanopyformingKelp=CO2_sens$CanopyformingKelp*value,
         Ochrestar=CO2_sens$Ochrestar*value,
         Dungenesscrab=CO2_sens$Dungenesscrab*value,
         Redurchin=CO2_sens$Redurchin*value,
         RazorClam=CO2_sens$RazorClam*value,
         sablefish=CO2_sens$sablefish*value,
         blueblackrockfish=CO2_sens$blueblackrockfish*value,
         PinkSalmon=CO2_sens$PinkSalmon*value,
         copperquillbackrockfish=CO2_sens$copperquillbackrockfish*value)
names(bigCO2_responses)

#try to make these data "long"

littleCO2_responses_long<-gather(littleCO2_responses, species, response, seagrass:copperquillbackrockfish)
dim(littleCO2_responses_long)

bigCO2_responses_long<-gather(bigCO2_responses, species, response, seagrass:copperquillbackrockfish)
dim(bigCO2_responses_long)

limits<-c(-1, 1)

littleCO2_responses_long %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species, nrow=2)
ggsave("figures/littleCO2_responses.pdf", height = 6, width = 9)


bigCO2_responses_long %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species, nrow=2) + coord_cartesian(xlim=c(200, 300))
ggsave("figures/bigCO2_responses.pdf", height = 7, width = 9)


#
#
#
#oxy

#read in one of Sam's maps
littleoxy<-read_csv("processed_data/2km_delta_oxy_200m.csv", col_names=F)
bigoxy<-read_csv("processed_data/12km_delta_oxy_200m.csv", col_names=F)


#reshape it into a long dataframe
littleoxy.df<-melt(littleoxy) %>%
  mutate(lat=rep(1:dim(littleoxy)[1], dim(littleoxy)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

dim(littleoxy.df)


#reshape it into a long dataframe
bigoxy.df<-melt(bigoxy) %>%
  mutate(lat=rep(1:dim(bigoxy)[1], dim(bigoxy)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#oxy - extract species with oxy response data and add rows for other species
#and change species names so they will work as column names 

oxy_sens<-sensitivity_by_species %>%
  filter(treatment_var=="oxygen") %>%
  add_row(English_Name=setdiff(unique(sensitivity_by_species$English_Name),unique(.$English_Name)),
          treatment_var="oxy") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))

#make a vector with weighted sensitivity titled by species name
oxy_sens<-oxy_sens %>%
  select(English_Name, weighted_sensitivity) %>%
  spread(., English_Name, weighted_sensitivity)

littleoxy_responses<-littleoxy.df %>%
  mutate(seagrass=oxy_sens$seagrass*value,
         CanopyformingKelp=oxy_sens$CanopyformingKelp*value,
         Ochrestar=oxy_sens$Ochrestar*value,
         Dungenesscrab=oxy_sens$Dungenesscrab*value,
         Redurchin=oxy_sens$Redurchin*value,
         RazorClam=oxy_sens$RazorClam*value,
         sablefish=oxy_sens$sablefish*value,
         blueblackrockfish=oxy_sens$blueblackrockfish*value,
         PinkSalmon=oxy_sens$PinkSalmon*value,
         copperquillbackrockfish=oxy_sens$copperquillbackrockfish*value)

bigoxy_responses<-bigoxy.df %>%
  mutate(seagrass=oxy_sens$seagrass*value,
         CanopyformingKelp=oxy_sens$CanopyformingKelp*value,
         Ochrestar=oxy_sens$Ochrestar*value,
         Dungenesscrab=oxy_sens$Dungenesscrab*value,
         Redurchin=oxy_sens$Redurchin*value,
         RazorClam=oxy_sens$RazorClam*value,
         sablefish=oxy_sens$sablefish*value,
         blueblackrockfish=oxy_sens$blueblackrockfish*value,
         PinkSalmon=oxy_sens$PinkSalmon*value,
         copperquillbackrockfish=oxy_sens$copperquillbackrockfish*value)
names(bigoxy_responses)

#try to make these data "long"

littleoxy_responses_long<-gather(littleoxy_responses, species, response, seagrass:copperquillbackrockfish)
dim(littleoxy_responses_long)

bigoxy_responses_long<-gather(bigoxy_responses, species, response, seagrass:copperquillbackrockfish)
dim(bigoxy_responses_long)

limits<-c(-6, 3)
#hist(bigoxy_responses_long$response)

littleoxy_responses_long %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species, nrow=2)
ggsave("figures/littleoxy_responses.pdf", height = 6, width = 9)


bigoxy_responses_long %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species, nrow=2) + coord_cartesian(xlim=c(200, 300))
ggsave("figures/bigoxy_responses.pdf", height = 7, width = 9)
