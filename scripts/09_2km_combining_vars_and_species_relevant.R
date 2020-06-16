#goal: first, combine results across stressors but for each species
#then, combine results across species but for each stressor

library(tidyverse)
library(broom)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
#change legend label

#read in data
oxyrel_greater_than_10percent<-read_csv("processed_data/oxyrel_greater_than_10percent.csv")
CO2rel_greater_than_10percent<-read_csv("processed_data/CO2rel_greater_than_10percent.csv")
temprel_greater_than_10percent<-read_csv("processed_data/temprel_greater_than_10percent.csv")
pHrel_greater_than_10percent<-read_csv("processed_data/pHrel_greater_than_10percent.csv")


all_vars_together_rel<-rbind(mutate(oxyrel_greater_than_10percent, variable="oxy"),
                    mutate(CO2rel_greater_than_10percent, variable="CO2"),
                    mutate(temprel_greater_than_10percent, variable="temp"),
                    mutate(pHrel_greater_than_10percent, variable="pH")) %>%
                  group_by(lat, long, species, no_env_data) %>%
                  summarize(abs_number_over_10=sum(abs_number_over_10), 
                            pos_number_over_10=sum(pos_number_over_10), 
                            neg_number_over_10=sum(neg_number_over_10), 
                            pos_number_over_20=sum(pos_number_over_20),
                            neg_number_over_20=sum(neg_number_over_20),
                            abs_number_over_20=sum(abs_number_over_20),
                            pos_number_over_30=sum(pos_number_over_30),
                            neg_number_over_30=sum(neg_number_over_30),
                            abs_number_over_30=sum(abs_number_over_30),
                            num_responses=sum(num_responses)) %>%
                  ungroup() 

#make 0's back into NA in grid cells with no data
all_vars_together_rel[which(all_vars_together_rel$no_env_data=="no_data"),
                            str_which(names(all_vars_together_rel), "number_over")]<-NA

#plot each ocean layer separately
#make species a factor
all_vars_together_rel$species<-as.factor(all_vars_together_rel$species)

#change names back to publishable names
all_vars_together_rel$species2<-plyr::mapvalues(all_vars_together_rel$species, 
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
levels(all_vars_together_rel$species2)
order_desired<-c("pink salmon", "blue & black rockfish", 
                 "copper & quillback rockfish", "sablefish", "razor clam", "red urchin", "Dungeness crab", 
                 "ochre star", "canopy-forming kelp", "seagrass")


all_vars_together_rel<-all_vars_together_rel %>%
  mutate(category =  factor(species2, levels = order_desired)) %>%
  arrange(category)  

all_vars_together_rel$species2 <- factor(all_vars_together_rel$species2, 
                                            levels = rev(order_desired))

#make a separate dataframe of just number of studies per species for easy annotation
num_studs<-all_vars_together_rel %>%
  group_by(species2) %>%
  summarize(num= max(num_responses),
            max10= max(as.numeric(na.omit(abs_number_over_10))),
            max20=max(as.numeric(na.omit(abs_number_over_20))),
            max30=max(as.numeric(na.omit(abs_number_over_30))))


#figure out the coordinates
max(all_vars_together_rel$long)*5/100
max(all_vars_together_rel$lat)*5/100


#plot##############

###10 %##############
#absolute number of responses greater than 10
colourCount = length(unique(as.factor(all_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(abs_number_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.line.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank(),
                          axis.line.y=element_blank())  +
  labs(fill="number of response 
types altered by > 10%")
ggsave("figures/abs_10percent_2km.pdf", height = 6, width = 9)
ggsave("figures/abs_10percent_2km.png", height = 6, width = 9)

#negative changes
#colourCount = length(unique(as.factor(all_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(neg_number_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  labs(fill="number of response 
types decreased by > 10%")
ggsave("figures/neg_10percent_2km.pdf", height = 6, width = 9)
ggsave("figures/neg_10percent_2km.png", height = 6, width = 9)

#positive changes
#colourCount = length(unique(as.factor(pos_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(pos_number_over_10))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  labs(fill="number of response 
types increased by > 10%")
ggsave("figures/pos_10percent_2km.pdf", height = 6, width = 9)
ggsave("figures/pos_10percent_2km.png", height = 6, width = 9)


###20 %##############
#absolute number of responses greater than 20
#colourCount = length(unique(as.factor(all_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(abs_number_over_20))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  labs(fill="number of response 
types altered by > 20%")
ggsave("figures/abs_20percent_2km.pdf", height = 6, width = 9)
ggsave("figures/abs_20percent_2km.png", height = 6, width = 9)

#negative changes
#colourCount = length(unique(as.factor(all_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(neg_number_over_20))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  labs(fill="number of response 
       types altered by decreased 20%")
ggsave("figures/neg_20percent_2km.pdf", height = 6, width = 9)
ggsave("figures/neg_20percent_2km.png", height = 6, width = 9)

#positive changes
#colourCount = length(unique(as.factor(pos_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(pos_number_over_20))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  labs(fill="number of response 
       types increased by > 20%")
ggsave("figures/pos_20percent_2km.pdf", height = 6, width = 9)
ggsave("figures/pos_20percent_2km.png", height = 6, width = 9)

#plot##############

###30 %##############
#absolute number of responses greater than 30
#colourCount = length(unique(as.factor(all_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(abs_number_over_30))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  labs(fill="number of response 
       types altered by > 30%")
ggsave("figures/abs_30percent_2km.pdf", height = 6, width = 9)
ggsave("figures/abs_30percent_2km.png", height = 6, width = 9)

#negative changes
#colourCount = length(unique(as.factor(all_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(neg_number_over_30))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  labs(fill="number of response 
       types decreased by > 30%")
ggsave("figures/neg_30percent_2km.pdf", height = 6, width = 9)
ggsave("figures/neg_30percent_2km.png", height = 6, width = 9)

#positive changes
#colourCount = length(unique(as.factor(pos_vars_together_rel$abs_number_over_10)))
all_vars_together_rel %>% 
  ggplot(aes(x = lat, y = long)) + geom_tile(aes(fill = as.factor(pos_number_over_30))) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(colourCount), na.value="grey") +
  facet_wrap(~species2, nrow=2) +
  geom_text(data=num_studs, aes(x=165, y=15, label=num)) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  labs(fill="number of response 
       types increased by > 30%")
ggsave("figures/pos_30percent_2km.pdf", height = 6, width = 9)
ggsave("figures/pos_30percent_2km.png", height = 6, width = 9)

