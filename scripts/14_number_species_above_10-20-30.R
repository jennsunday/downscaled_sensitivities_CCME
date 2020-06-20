#goal: produce one map from each model with # of species with 10% change it at least one variable.
#repeat for 30%.

# notes: this is going well, but I should go back to original study-based data 
# and separate pelagic vs. benthic data, 
# so that I have a better representation across these regions.
# is there a column for region? if not make one. meta-analysis need to be within regions.
# then in files 6,7,8 I should make a vector for # responses > 10% and > 30% and total,
# so that it is easy to plot them together after that. 

#read in libraries:
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

#first, data from 2km
oxy_responses_2km<-read_csv("processed_data/oxyrel_greater_than_10percent.csv")
CO2_responses_2km<-read_csv("processed_data/CO2rel_greater_than_10percent.csv")
temp_responses_2km<-read_csv("processed_data/temprel_greater_than_10percent.csv")
#read in 500m mask
little_shelf_mask<-read_csv("raw_data/downscaled_climate_data/mask_500m_2km.csv", col_names=F)

num_species<-c(unique(oxy_responses_2km$species), 
               unique(CO2_responses_2km$species),  
               unique(temp_responses_2km$species)) %>%
  unique(.) %>%
  length(.)

#reshape shelf mask into long data
#reshape these into a long dataframe - 200 layer
little_shelf_contour<-melt(little_shelf_mask) %>%
  mutate(long=rep(1:dim(little_shelf_mask)[1], dim(little_shelf_mask)[2])) %>%
  separate(variable, c(NA, "lat"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(shelf=value)

#combine all environmental variables
species_responses<-rbind(oxy_responses_2km, 
                        CO2_responses_2km, 
                        temp_responses_2km) %>%
    group_by(lat, long, species, no_env_data) %>%
    summarize(number_responses_over_10=length(which(abs_number_over_10>=1)),
              number_responses_over_20=length(which(abs_number_over_20>=1)),
              number_responses_over_30=length(which(abs_number_over_30>=1))) %>%
    group_by(lat, long, no_env_data) %>%
    summarize(number_species_over_10=length(which(number_responses_over_10>=1)),
              number_species_over_20=length(which(number_responses_over_20>=1)),
              number_species_over_30=length(which(number_responses_over_30>=1)))


#make a mask of the coastline - 1 indicates land
#make coastline line mask only contain coastline cells#
coastline_mask<-species_responses %>%
  mutate(land=ifelse(no_env_data=="no_data", 1, 0)) %>%
  select(lat, long, land) %>%
  filter(land==1)

#remove data outside of shelf or realm of model
#need to join by lat_long so that shelf is applied to whole dataset
#make response data only consist of shelf cells in the ocean
species_responses<-left_join(species_responses, little_shelf_contour, 
                                 by = c("lat", "long")) %>%
                  filter(shelf==1, 
                         no_env_data=="yes_data")



#############
colourCount = length(unique(as.factor(species_responses$number_species_over_30)))
mycolscale<-(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount)) 
mycolscale[1]<-"#d9d9d9" # make 0 value = grey

species_responses %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values=mycolscale) +  
  geom_tile(data=coastline_mask, fill="grey") +
  labs(fill="number of species 
       with greater than 10% responses") +
  theme_classic()
ggsave("figures/num_species_10percent_2km.png", height = 6, width = 7)


species_responses %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_20))) + 
  scale_fill_manual(values=mycolscale) +  
  geom_tile(data=coastline_mask, fill="grey") +
  labs(fill="number of species 
       with greater than 20% responses") +
  theme_classic()
ggsave("figures/num_species_20percent_2km.png", height = 6, width = 7)


species_responses %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_30))) + 
  scale_fill_manual(values=mycolscale) +  
  geom_tile(data=coastline_mask, fill="grey") +
  labs(fill="number of species 
       with greater than 30% responses") +
  theme_classic()
ggsave("figures/num_species_30percent_2km.png", height = 6, width = 7)


#next, 12km
oxy_responses_12km<-read_csv("processed_data/oxyrel_greater_than_percent_big.csv")
CO2_responses_12km<-read_csv("processed_data/CO2rel_greater_than_percent_big.csv")
temp_responses_12km<-read_csv("processed_data/temprel_greater_than_percent_big.csv")

#read in 500m mask
big_shelf_mask<-read_csv("raw_data/downscaled_climate_data/mask_500m_12km.csv", col_names=F)

num_species<-c(unique(oxy_responses_12km$species), 
               unique(CO2_responses_12km$species),  
               unique(temp_responses_12km$species)) %>%
  unique(.) %>%
  length(.)

#reshape shelf mask into long data
#reshape these into a long dataframe - 200 layer
big_shelf_contour<-melt(big_shelf_mask) %>%
  mutate(long=rep(1:dim(big_shelf_mask)[1], dim(big_shelf_mask)[2])) %>%
  separate(variable, c(NA, "lat"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(shelf=value)

#combine all environmental variables
species_responses_12<-rbind(oxy_responses_12km, 
                         CO2_responses_12km, 
                         temp_responses_12km) %>%
  group_by(lat, long, species, no_env_data) %>%
  summarize(number_responses_over_10=length(which(abs_number_over_10>=1)),
            number_responses_over_20=length(which(abs_number_over_20>=1)),
            number_responses_over_30=length(which(abs_number_over_30>=1))) %>%
  group_by(lat, long, no_env_data) %>%
  summarize(number_species_over_10=length(which(number_responses_over_10>=1)),
            number_species_over_20=length(which(number_responses_over_20>=1)),
            number_species_over_30=length(which(number_responses_over_30>=1)))


#make a mask of the coastline - 1 indicates land
#make coastline line mask only contain coastline cells#
coastline_mask_12<-species_responses_12 %>%
  mutate(land=ifelse(no_env_data=="no_data", 1, 0)) %>%
  select(lat, long, land) %>%
  filter(land==1)

#remove data outside of shelf or realm of model
#need to join by lat_long so that shelf is applied to whole dataset
#make response data only consist of shelf cells in the ocean
species_responses_12<-left_join(species_responses_12, big_shelf_contour, 
                             by = c("lat", "long")) %>%
  filter(shelf==1, 
         no_env_data=="yes_data")

#############
mycolscale<-(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(10)) 
mycolscalefixed <- c("1" = mycolscale[1], 
                     "2" = mycolscale[2], "3" = mycolscale[3], 
                     "4" = mycolscale[4],"5" = mycolscale[5], 
                     "6" = mycolscale[6], "7" = mycolscale[7], 
                     "8" = mycolscale[8],"9" = mycolscale[9], 
                     "10" = mycolscale[10], "0" = "#d9d9d9")
#2km

species_responses %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values=mycolscalefixed) +  
  geom_tile(data=coastline_mask, fill="grey") +
  labs(fill="number of species 
       with greater than 10% responses") +
  theme_classic()  + theme(axis.title.x=element_blank(),
                                           axis.text.x=element_blank(),
                                           axis.ticks.x=element_blank(),
                                           axis.title.y=element_blank(),
                                           axis.text.y=element_blank(),
                                           axis.ticks.y=element_blank()) 
ggsave("figures/num_species_10percent_2km.png", height = 6, width = 5)

#12km
species_responses_12 %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values=mycolscalefixed) +  
  geom_tile(data=coastline_mask_12, fill="grey") +
  labs(fill="number of species 
       with greater than 10% responses") +
  theme_classic() + 
  coord_cartesian(ylim=c(80, 380)) + theme(axis.title.x=element_blank(),
                                           axis.text.x=element_blank(),
                                           axis.ticks.x=element_blank(),
                                           axis.title.y=element_blank(),
                                           axis.text.y=element_blank(),
                                           axis.ticks.y=element_blank()) 
ggsave("figures/num_species_10percent_12km.png", height = 6, width = 7)

#20%
species_responses %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_20))) + 
  scale_fill_manual(values=mycolscalefixed) +  
  geom_tile(data=coastline_mask, fill="grey") +
  labs(fill="number of species 
       with greater than 20% responses") +
  theme_classic() + theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank()) 
ggsave("figures/num_species_20percent_2km.png", height = 6, width = 7)

species_responses_12 %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_10))) + 
  scale_fill_manual(values=mycolscalefixed) +  
  geom_tile(data=coastline_mask_12, fill="grey") +
  labs(fill="number of species 
       with greater than 20% responses") +
  theme_classic() +
  coord_cartesian(ylim=c(80, 380)) + theme(axis.title.x=element_blank(),
                                           axis.text.x=element_blank(),
                                           axis.ticks.x=element_blank(),
                                           axis.title.y=element_blank(),
                                           axis.text.y=element_blank(),
                                           axis.ticks.y=element_blank()) 
ggsave("figures/num_species_20percent_12km.png", height = 6, width = 5)

#30%
species_responses %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_30))) + 
  scale_fill_manual(values=mycolscalefixed) +  
  geom_tile(data=coastline_mask, fill="grey") +
  labs(fill="number of species 
       with greater than 30% responses") +
  theme_classic() +  theme(axis.title.x=element_blank(),
                           axis.text.x=element_blank(),
                           axis.ticks.x=element_blank(),
                           axis.title.y=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks.y=element_blank()) 
ggsave("figures/num_species_30percent_2km.png", height = 6, width = 7)

species_responses_12 %>%
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(number_species_over_30))) + 
  scale_fill_manual(values=mycolscalefixed) +  
  geom_tile(data=coastline_mask_12, fill="grey") +
  labs(fill="number of species 
       with greater than 30% responses") +
  theme_classic() +
  coord_cartesian(ylim=c(80, 380)) + theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank(),
                                          axis.ticks.x=element_blank(),
                                          axis.title.y=element_blank(),
                                          axis.text.y=element_blank(),
                                          axis.ticks.y=element_blank()) 
ggsave("figures/num_species_30percent_12km.png", height = 6, width = 5)


