#goal: first, combine results across stressors but for each species
#then, combine results across species but for each stressor

library(tidyverse)
library(broom)
library(reshape2)
library(gridExtra)
library(RColorBrewer)


#read in data
oxyrel_greater_than_10percent_12km<-read_csv("processed_data/oxyrel_greater_than_10percent_big.csv")
CO2rel_greater_than_10percent_12km<-read_csv("processed_data/CO2rel_greater_than_10percent_big.csv")
temprel_greater_than_10percent_12km<-read_csv("processed_data/temprel_greater_than_10percent_big.csv")


all_vars_together_rel_12km<-rbind(mutate(oxyrel_greater_than_10percent_12km, variable="oxy"),
                    mutate(CO2rel_greater_than_10percent_12km, variable="CO2"),
                    mutate(temprel_greater_than_10percent_12km, variable="temp")) %>%
                  group_by(lat, long, species, no_env_data) %>%
                  summarize(abs_number_over_10=sum(abs_number_over_10), 
                            pos_number_over_10=sum(pos_number_over_10), 
                            neg_number_over_10=sum(neg_number_over_10), 
                            num_responses=sum(num_responses)) %>%
                  ungroup() %>%
                  mutate(abs_number_over_10=as.numeric(ifelse(
                    no_env_data=="no_data", "NA", abs_number_over_10))) %>%
                  mutate(pos_number_over_10=as.numeric(ifelse(
                    no_env_data=="no_data", "NA", pos_number_over_10))) %>%
                  mutate(neg_prop_over_10=as.numeric(ifelse(
                    neg_number_over_10 =="no_data", "NA", neg_number_over_10))) %>%
                  mutate(num_responses=as.numeric(ifelse(
                    no_env_data=="no_data", "NA", num_responses)))


#plot each ocean layer separately
#make species a factor
all_vars_together_rel_12km$species<-as.factor(all_vars_together_rel_12km$species)

#change names back to publishable names
all_vars_together_rel_12km$species2<-plyr::mapvalues(all_vars_together_rel_12km$species, 
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
levels(all_vars_together_rel_12km$species2)
order_desired<-c("pink salmon", "blue & black rockfish", 
                 "copper & quillback rockfish", "sablefish", "razor clam", "red urchin", "Dungeness crab", 
                 "ochre star", "canopy-forming kelp", "seagrass")


all_vars_together_rel_12km<-all_vars_together_rel_12km %>%
  mutate(category =  factor(species2, levels = order_desired)) %>%
  arrange(category)  

all_vars_together_rel_12km$species2 <- factor(all_vars_together_rel_12km$species2, 
                                            levels = rev(order_desired))

#make dataframe of just number of studies per species for easy annotation
num_studs<-all_vars_together_rel_12km %>%
  group_by(species2) %>%
  summarize(num= max(num_responses),
            max10= max(as.numeric(na.omit(abs_number_over_10))))


#figure out the coordinates
max(all_vars_together_rel_12km$long)*5/100
max(all_vars_together_rel_12km$lat)*5/100

#plot - absolute number of responses greater than 10
colourCount = length(unique(as.factor(all_vars_together_rel_12km$abs_number_over_10)))
all_vars_together_rel_12km %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(abs_number_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  coord_cartesian(xlim=c(200,300), ylim=c(80, 380)) +
  theme_classic() + theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())
ggsave("figures/abs_10percent_12km.pdf", height = 6, width = 9)

#negative changes
colourCount = length(unique(as.factor(neg_vars_together_rel_12km$abs_number_over_10)))
all_vars_together_rel_12km %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(neg_number_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  coord_cartesian(xlim=c(200,300), ylim=c(80, 380)) +
  theme_classic() + theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank())
ggsave("figures/neg_10percent_12km.pdf", height = 6, width = 9)

#positive changes
colourCount = length(unique(as.factor(pos_vars_together_rel_12km$abs_number_over_10)))
all_vars_together_rel_12km %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(pos_number_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  coord_cartesian(xlim=c(200,300), ylim=c(80, 380)) +
  theme_classic() + theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank())
ggsave("figures/pos_10percent_12km.pdf", height = 6, width = 9)

