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


#read in Sam's maps
littletemp200<-read_csv("processed_data/2km_delta_temp_200m.csv", col_names=F)
bigtemp200<-read_csv("processed_data/12km_delta_temp_200m.csv", col_names=F)
littletempbot<-read_csv("processed_data/2km_delta_temp_bot.csv", col_names=F)
bigtempbot<-read_csv("processed_data/12km_delta_temp_bot.csv", col_names=F)


#reshape these into a long dataframe
littletemp200.df<-melt(littletemp200) %>%
  mutate(lat=rep(1:dim(littletemp200)[1], dim(littletemp200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
bigtemp200.df<-melt(bigtemp200) %>%
  mutate(lat=rep(1:dim(bigtemp200)[1], dim(bigtemp200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
littletempbot.df<-melt(littletempbot) %>%
  mutate(lat=rep(1:dim(littletempbot)[1], dim(littletempbot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
bigtempbot.df<-melt(bigtempbot) %>%
  mutate(lat=rep(1:dim(bigtempbot)[1], dim(bigtempbot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))



# calculate delta response for each species

#temp - extract species with temp response data and add rows for other species
#and change species names so they will work as column names 

temp_sens<-sensitivity_by_species %>%
  filter(treatment_var=="temperature") %>%
  add_row(English_Name=setdiff(unique(sensitivity_by_species$English_Name),unique(.$English_Name)),
          treatment_var="temperature") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))


#combined <-expand.grid(lat=littletemp.df$lat, species=temp_sens$English_Name) 
#fill_combined <- merge(combined, littletemp.df)

#make a vector with weighted sensitivity titled by species name
temp_sens<-temp_sens %>%
  select(English_Name, weighted_sensitivity) %>%
  spread(., English_Name, weighted_sensitivity)

littletemp200_responses<-littletemp200.df %>%
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

littletempbot_responses<-littletempbot.df %>%
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

bigtemp200_responses<-bigtemp200.df %>%
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

bigtempbot_responses<-bigtempbot.df %>%
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


#make these data "long"
littletemp200_responses_long<-gather(littletemp200_responses, species, 
                                  response, seagrass:copperquillbackrockfish)
littletempbot_responses_long<-gather(littletempbot_responses, species, 
                                      response, seagrass:copperquillbackrockfish)
bigtemp200_responses_long<-gather(bigtemp200_responses, species, 
                                     response, seagrass:copperquillbackrockfish)
bigtempbot_responses_long<-gather(bigtempbot_responses, species, 
                                     response, seagrass:copperquillbackrockfish)

littletemp_relevant_layer<-rbind(littletempbot_responses_long %>%
                      filter(species %in% c("seagrass", 
                                            "CanopyformingKelp", 
                                            "Ochrestar",
                                            "Dungenesscrab",
                                            "Redurchin",
                                            "RazorClam", 
                                            "copperquillbackrockfish")) %>%
                        mutate(layer="bottom"),
                      littletemp200_responses_long %>%
                        filter(species %in% c("sablefish", 
                                              "blueblackrockfish",
                                              "PinkSalmon")) %>%
                        mutate(layer="200"))

bigtemp_relevant_layer<-rbind(bigtempbot_responses_long %>%
                               filter(species %in% c("seagrass", 
                                                     "CanopyformingKelp", 
                                                     "Ochrestar",
                                                     "Dungenesscrab",
                                                     "Redurchin",
                                                     "RazorClam",
                                                     "copperquillbackrockfish")) %>%
                               mutate(layer="bottom"),
                          bigtemp200_responses_long %>%
                               filter(species %in% c("sablefish", 
                                                     "blueblackrockfish",
                                                     "PinkSalmon")) %>%
                               mutate(layer="200"))



limits<-c(-0.3, 0.3)

littletemp_relevant_layer %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species+layer, nrow=2)
ggsave("figures/littletemp_layer_response.pdf", height = 6, width = 9)


bigtemp_relevant_layer %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species+layer, nrow=2) + coord_cartesian(xlim=c(200, 300))
ggsave("figures/bigtemp_layer_response.pdf", height = 7, width = 9)

