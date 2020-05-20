#goal: first, combine results across stressors but for each species
#then, combine results across species but for each stressor

library(tidyverse)
library(broom)
library(reshape2)
library(mapdata)
library(gridExtra)
library(RColorBrewer)



oxy200_greater_than_10percent<-read_csv("processed_data/oxy200_greater_than_30percent.csv")
oxybot_greater_than_10percent<-read_csv("processed_data/oxybot_greater_than_30percent.csv")
CO2200_greater_than_10percent<-read_csv("processed_data/CO2200_greater_than_30percent.csv")
CO2bot_greater_than_10percent<-read_csv("processed_data/CO2bot_greater_than_30percent.csv")
temp200_greater_than_10percent<-read_csv("processed_data/temp200_greater_than_30percent.csv")
tempbot_greater_than_10percent<-read_csv("processed_data/tempbot_greater_than_30percent.csv")


#combine all of the variables, group by lat, long, and species, sum responses greater than 10%
#take proportion of responses >10%
#first for 200 layer
all_vars_together_200<-rbind(mutate(oxy200_greater_than_10percent, variable="oxy"),
                    mutate(CO2200_greater_than_10percent, variable="CO2"),
                    mutate(temp200_greater_than_10percent, variable="temp")) %>%
                  group_by(lat, long, species, no_env_data) %>%
                  summarize(all_var_number_over_10=sum(number_over_10), all_var_num_responses=sum(number_responses)) %>%
                  ungroup() %>%
                  mutate(prop_over_10=as.numeric(ifelse(
                    no_env_data=="no_data", "NA", all_var_number_over_10/all_var_num_responses)))

#next for bottom layer
all_vars_together_bot<-rbind(mutate(oxybot_greater_than_10percent, variable="oxy"),
                             mutate(CO2bot_greater_than_10percent, variable="CO2"),
                             mutate(tempbot_greater_than_10percent, variable="temp")) %>%
  group_by(lat, long, species, no_env_data) %>%
  summarize(all_var_number_over_10=sum(number_over_10), all_var_num_responses=sum(number_responses)) %>%
  ungroup() %>%
  mutate(prop_over_10=as.numeric(ifelse(
    no_env_data=="no_data", "NA", all_var_number_over_10/all_var_num_responses))) 

#plot each ocean layer separately
all_vars_together_bot$species<-as.factor(all_vars_together_bot$species)

#change names back to publishable names
all_vars_together_bot$species2<-plyr::mapvalues(all_vars_together_bot$species, 
          from = c("CanopyformingKelp","seagrass","RazorClam","Dungenesscrab",
                   "Ochrestar","Redurchin",
                   "PinkSalmon","sablefish",
                   "copperquillbackrockfish","blueblackrockfish"), 
          to = c("canopy-forming kelp","seagrass","razor clam","Dungeness crab",
                 "ochre star","red urchin",
                 "pink salmon","sablefish",
                 "copper & quillback rockfish","blue & black rockfish"))

all_vars_together_200$species2<-plyr::mapvalues(all_vars_together_200$species, 
                                                from = c("CanopyformingKelp","seagrass","RazorClam","Dungenesscrab",
                                                         "Ochrestar","Redurchin",
                                                         "PinkSalmon","sablefish",
                                                         "copperquillbackrockfish","blueblackrockfish"), 
                                                to = c("canopy-forming kelp","seagrass","razor clam","Dungeness crab",
                                                       "ochre star","red urchin",
                                                       "pink salmon","sablefish",
                                                       "copper & quillback rockfish","blue & black rockfish"))


#now reorder
#arrange order of species
levels(all_vars_together_bot$species2)
order_desired<-c("pink salmon", "blue & black rockfish", 
                 "copper & quillback rockfish", "sablefish", "razor clam", "red urchin", "Dungeness crab", 
                 "ochre star", "canopy-forming kelp", "seagrass")


all_vars_together_bot<-all_vars_together_bot %>%
  mutate(category =  factor(species2, levels = order_desired)) %>%
  arrange(category)  

all_vars_together_bot$species2 <- factor(all_vars_together_bot$species2, 
                                            levels = rev(order_desired))



all_vars_together_200<-all_vars_together_200 %>%
  mutate(category =  factor(species2, levels = order_desired)) %>%
  arrange(category)  

all_vars_together_200$species2 <- factor(all_vars_together_200$species2, 
                                         levels = rev(order_desired))



#make dataframe of just number of studies per species for easy annotation
num_studs<-all_vars_together_bot %>%
  group_by(species) %>%
  slice(n())
  
#continuous scale
all_vars_together_bot %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = (prop_over_10))) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=all_var_num_responses)) +
  labs(fill="proportion of responses 
studied altered by >10%")
ggsave("figures/all_vars_together_bot_10percent.pdf", height = 6, width = 9)


all_vars_together_200 %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = (prop_over_10))) + 
  scale_fill_distiller(palette="YlOrRd", direction=1, na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=all_var_num_responses)) +
  labs(fill="proportion of responses 
studied altered by >10%")
ggsave("figures/all_vars_together_200_10percent.pdf", height = 6, width = 9)

#figure out the coordinates
max(all_vars_together_200$long)*5/100
max(all_vars_together_200$lat)*5/100

#discrete scale
colourCount = length(unique(as.factor(all_vars_together_bot$prop_over_10)))
all_vars_together_bot %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(prop_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=all_var_num_responses)) +
  labs(fill="proportion of responses 
       studied altered by >30%")
ggsave("figures/all_vars_together_bot_30percent_dis.pdf", height = 6, width = 9)

colourCount = length(unique(as.factor(all_vars_together_200$prop_over_10)))
all_vars_together_200 %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(prop_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=all_var_num_responses)) +
  labs(fill="proportion of responses 
       studied altered by >30%")
ggsave("figures/all_vars_together_200_30percent_dis.pdf", height = 6, width = 9)

#
# code to plot species at "relevant" layer 
#
all_vars_together_relevant_layer<-rbind(all_vars_together_bot %>%
                                          filter(species %in% c("seagrass", 
                                                                "CanopyformingKelp", 
                                                                "Ochrestar",
                                                                "Dungenesscrab",
                                                                "Redurchin",
                                                                "RazorClam",
                                                                "copperquillbackrockfish")) %>%
                                          mutate(layer="bottom"),
                                        all_vars_together_200 %>%
                                          filter(species %in% c("sablefish",  
                                                                "blueblackrockfish",
                                                                "PinkSalmon")) %>%
                                          mutate(layer="200"))

colourCount = length(unique(as.factor(all_vars_together_relevant_layer$prop_over_10)))
all_vars_together_relevant_layer %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(prop_over_10))) + 
  facet_wrap(~species+layer, nrow=2) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount))
ggsave("figures/all_vars_together_rel_10percent.pdf", height = 6, width = 9)


#

