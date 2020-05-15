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
littleoxy200<-read_csv("processed_data/2km_delta_oxy_200m.csv", col_names=F)
bigoxy200<-read_csv("processed_data/12km_delta_oxy_200m.csv", col_names=F)
littleoxybot<-read_csv("processed_data/2km_delta_oxy_bot.csv", col_names=F)
bigoxybot<-read_csv("processed_data/12km_delta_oxy_bot.csv", col_names=F)


#reshape these into a long dataframe
littleoxy200.df<-melt(littleoxy200) %>%
  mutate(lat=rep(1:dim(littleoxy200)[1], dim(littleoxy200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
bigoxy200.df<-melt(bigoxy200) %>%
  mutate(lat=rep(1:dim(bigoxy200)[1], dim(bigoxy200)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
littleoxybot.df<-melt(littleoxybot) %>%
  mutate(lat=rep(1:dim(littleoxybot)[1], dim(littleoxybot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

#reshape these into a long dataframe
bigoxybot.df<-melt(bigoxybot) %>%
  mutate(lat=rep(1:dim(bigoxybot)[1], dim(bigoxybot)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))



# calculate delta response for each species

#oxy - extract species with oxy response data and add rows for other species
#and change species names so they will work as column names 

oxy_sens<-sensitivity_by_species %>%
  filter(treatment_var=="oxygen") %>%
  add_row(English_Name=setdiff(unique(sensitivity_by_species$English_Name),unique(.$English_Name)),
          treatment_var="oxygen") %>%
  mutate(English_Name=gsub(" ","", English_Name)) %>%
  mutate(English_Name=gsub("&","", English_Name)) %>%
  mutate(English_Name=gsub("-","", English_Name))


#combined <-expand.grid(lat=littleoxy.df$lat, species=oxy_sens$English_Name) 
#fill_combined <- merge(combined, littleoxy.df)

#make a vector with weighted sensitivity titled by species name
oxy_sens<-oxy_sens %>%
  select(English_Name, weighted_sensitivity) %>%
  spread(., English_Name, weighted_sensitivity)

littleoxy200_responses<-littleoxy200.df %>%
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

littleoxybot_responses<-littleoxybot.df %>%
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

bigoxy200_responses<-bigoxy200.df %>%
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

bigoxybot_responses<-bigoxybot.df %>%
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


#make these data "long"
littleoxy200_responses_long<-gather(littleoxy200_responses, species, 
                                  response, seagrass:copperquillbackrockfish)
littleoxybot_responses_long<-gather(littleoxybot_responses, species, 
                                      response, seagrass:copperquillbackrockfish)
bigoxy200_responses_long<-gather(bigoxy200_responses, species, 
                                     response, seagrass:copperquillbackrockfish)
bigoxybot_responses_long<-gather(bigoxybot_responses, species, 
                                     response, seagrass:copperquillbackrockfish)

littleoxy_relevant_layer<-rbind(littleoxybot_responses_long %>%
                      filter(species %in% c("seagrass", 
                                            "CanopyformingKelp", 
                                            "Ochrestar",
                                            "Dungenesscrab",
                                            "Redurchin",
                                            "RazorClam",
                                            "copperquillbackrockfish")) %>%
                        mutate(layer="bottom"),
                      littleoxy200_responses_long %>%
                        filter(species %in% c("sablefish", 
                                              "blueblackrockfish",
                                              "PinkSalmon")) %>%
                        mutate(layer="200"))

bigoxy_relevant_layer<-rbind(bigoxybot_responses_long %>%
                               filter(species %in% c("seagrass", 
                                                     "CanopyformingKelp", 
                                                     "Ochrestar",
                                                     "Dungenesscrab",
                                                     "Redurchin",
                                                     "RazorClam",
                                                     "copperquillbackrockfish")) %>%
                               mutate(layer="bottom"),
                          bigoxy200_responses_long %>%
                               filter(species %in% c("sablefish", 
                                                     "blueblackrockfish",
                                                     "PinkSalmon")) %>%
                               mutate(layer="200"))


head(bigoxy_relevant_layer)

limits<-c(-1, 1)

littleoxy_relevant_layer %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species+layer, nrow=2)
ggsave("figures/littleoxy_layer_response.pdf", height = 6, width = 9)


bigoxy_relevant_layer %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = response)) + 
  scale_fill_gradient2(limits=limits) +
  facet_wrap(~species+layer, nrow=2) + coord_cartesian(xlim=c(200, 300))
ggsave("figures/bigoxy_layer_response.pdf", height = 7, width = 9)

