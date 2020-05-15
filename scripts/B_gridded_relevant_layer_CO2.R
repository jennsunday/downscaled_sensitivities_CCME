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
littleCO2200<-read_csv("processed_data/2km_delta_pCO2_200m.csv", col_names=F)
bigCO2200<-read_csv("processed_data/12km_delta_pCO2_200m.csv", col_names=F)
littleCO2bot<-read_csv("processed_data/2km_delta_pCO2_bot.csv", col_names=F)
bigCO2bot<-read_csv("processed_data/12km_delta_pCO2_bot.csv", col_names=F)


#reshape these into a long dataframe
littleCO2200.df<-melt(littleCO2200) %>%
  mutate(lat=rep(1:dim(littleCO2200)[1], dim(littleCO2200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
bigCO2200.df<-melt(bigCO2200) %>%
  mutate(lat=rep(1:dim(bigCO2200)[1], dim(bigCO2200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
littleCO2bot.df<-melt(littleCO2bot) %>%
  mutate(lat=rep(1:dim(littleCO2bot)[1], dim(littleCO2bot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
bigCO2bot.df<-melt(bigCO2bot) %>%
  mutate(lat=rep(1:dim(bigCO2bot)[1], dim(bigCO2bot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))



# calculate delta response for each species

#CO2 - extract species with CO2 response data and add rows for other species
#and change species names so they will work as column names 

CO2_sens<-sensitivity_by_species %>%
  filter(treatment_var=="CO2") %>%
  add_row(English_Name=setdiff(unique(sensitivity_by_species$English_Name),unique(.$English_Name)),
          treatment_var="CO2") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))


#combined <-expand.grid(lat=littleCO2.df$lat, species=CO2_sens$English_Name) 
#fill_combined <- merge(combined, littleCO2.df)

#make a vector with weighted sensitivity titled by species name
CO2_sens<-CO2_sens %>%
  select(English_Name, weighted_sensitivity) %>%
  spread(., English_Name, weighted_sensitivity)

littleCO2200_responses<-littleCO2200.df %>%
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

littleCO2bot_responses<-littleCO2bot.df %>%
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

bigCO2200_responses<-bigCO2200.df %>%
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

bigCO2bot_responses<-bigCO2bot.df %>%
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


#make these data "long"
littleCO2200_responses_long<-gather(littleCO2200_responses, species, 
                                  response, seagrass:copperquillbackrockfish)
littleCO2bot_responses_long<-gather(littleCO2bot_responses, species, 
                                      response, seagrass:copperquillbackrockfish)
bigCO2200_responses_long<-gather(bigCO2200_responses, species, 
                                     response, seagrass:copperquillbackrockfish)
bigCO2bot_responses_long<-gather(bigCO2bot_responses, species, 
                                     response, seagrass:copperquillbackrockfish)

littleCO2_relevant_layer<-rbind(littleCO2bot_responses_long %>%
                      filter(species %in% c("seagrass", 
                                            "CanopyformingKelp", 
                                            "Ochrestar",
                                            "Dungenesscrab",
                                            "Redurchin",
                                            "RazorClam",
                                            "copperquillbackrockfish")) %>%
                        mutate(layer="bottom"),
                      littleCO2200_responses_long %>%
                        filter(species %in% c("sablefish",  
                                              "blueblackrockfish",
                                              "PinkSalmon")) %>%
                        mutate(layer="200"))

bigCO2_relevant_layer<-rbind(bigCO2bot_responses_long %>%
                               filter(species %in% c("seagrass", 
                                                     "CanopyformingKelp", 
                                                     "Ochrestar",
                                                     "Dungenesscrab",
                                                     "Redurchin",
                                                     "RazorClam",
                                                     "copperquillbackrockfish")) %>%
                               mutate(layer="bottom"),
                          bigCO2200_responses_long %>%
                               filter(species %in% c("sablefish", 
                                                     "blueblackrockfish",
                                                     "PinkSalmon")) %>%
                               mutate(layer="200"))


head(bigCO2_relevant_layer)

limits<-c(-1, 1)

littleCO2_relevant_layer %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species+layer, nrow=2)
ggsave("figures/littleCO2_layer_response.pdf", height = 6, width = 9)


bigCO2_relevant_layer %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species+layer, nrow=2) + coord_cartesian(xlim=c(200, 300))
ggsave("figures/bigCO2_layer_response.pdf", height = 7, width = 9)

