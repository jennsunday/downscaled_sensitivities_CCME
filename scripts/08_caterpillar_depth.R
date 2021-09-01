#goal: calculate a mean sensitivity from each observed response 
# multiplied by the delta of the relevant environmental variable in each grid cell
# only include deltas from relevant occupied depths 
#make caterpillar plot of responses

library(tidyverse)
library(broom)
library(PNWColors)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(biscale)
library(cowplot)

all_deltas_2km<-read_csv("processed_data/all_deltas_2km.csv") 
depth_2km<-read_csv("processed_data/depth_2km.csv")
depth_range<-read_csv("raw_data/depth_distribution.csv")
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")

#set seagrass to lower_depth of 30m instead of 10m to see more coverage
depth_range<-depth_range %>%
  mutate(lower_depth=ifelse(common_name=="seagrass", 30, lower_depth))

#left join sensitivity data to depth based on species names 
sensitivity_by_study<-left_join(sensitivity_by_study, depth_range, by="common_name")
#left join depth data to delta data for subsetting
all_deltas_2km<-left_join(all_deltas_2km, depth_2km, by=c("lat", "long", "latlong"))


#adjust the names of the zones to set up the left_join
sensitivity_by_study<-sensitivity_by_study %>%
  mutate (modelzone = case_when(modelzone=="bottom" ~ "bot", 
            modelzone=="surface" ~ "surf",
            modelzone=="200m" ~ "200m",
            TRUE ~ "999"))


all_deltas_2km<-all_deltas_2km %>%
  mutate (treatment_var = case_when(treatment_var=="temp" ~ "temperature", 
                                treatment_var=="oxy" ~ "oxygen",
                                treatment_var=="CO2" ~ "CO2",
                                treatment_var=="pH" ~ "pH",
                                TRUE ~ "999"))

#expand sensitivity data and gridded delta data
#make a new percentchange and percentchangeSE for every combination of response and delta
#don't include cells where depth is irrevelent for that species
#calculate a mean sensitivity from each observed response
study_response<-left_join(sensitivity_by_study, all_deltas_2km, by=c("treatment_var", "modelzone")) %>%
  drop_na(latlong) %>%
  drop_na(delta) %>%
  filter(adult_zone != "benthic" | lower_depth>depth) %>% #filter out instances when ocean depth is beyond a species' lower depth
  filter(upper_depth<depth) %>% #filter out instances when ocean depth is above a species' upper depth
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95C
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>%
  mutate(pos_neg=ifelse(percentchange>0, "pos", "neg")) %>%
  group_by(common_name, unique_study, treatment_var, response_type, Life_stage_category) %>%
  summarize(mean_percentchange=mean(percentchange), #get mean mean, mean low, and mean high, across grid cells
            mean_percentchange_lo_95=mean(percentchange_lo_95),
            mean_percentchange_hi_95=mean(percentchange_hi_95)) %>%
  ungroup()
  


###############
#plot data
###############


#replace order according to above
study_response<-left_join(study_response, species_order.df, by="common_name")

label_species<- sensitivity_by_study %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(percentchange)) %>%
  ungroup %>%
  arrange(mean_change, percentchange) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(mean_order=mean(compound_order), 
            mean_response=mean(percentchange), 
            min_response=min(percentchange), 
            max_response=max(percentchange)) %>%
  mutate(x_position=ifelse(mean_response>(3), -40, 55)) %>%
  mutate(x_position=ifelse(common_name %in% c("blue & black rockfish", "copper & quillback rockfish"),
                           70, x_position))

study_response %>%
  arrange(order, mean_percentchange) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup()
  
sensitivity_by_study %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(percentchange)) %>%
  ungroup %>%
  arrange(mean_change, percentchange) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  mutate(treatment_var2=ifelse(treatment_var %in% c("CO2", "pH"), "CO2 & pH", treatment_var)) %>%
  ggplot(aes(x=percentchange, y=compound_order, shape=response_type, col=treatment_var2, 
             fill=interaction(treatment_var2, Life_stage_category))) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-75,180)) +
  geom_point(size=2) + theme_classic() +
  scale_shape_manual(values=c(21, 22, 23, 24, 25))+
  geom_path(aes(group=common_name)) +
  scale_color_manual(values = c("#2c7bb6","#fdae61", "#d7191c")) +
  scale_fill_manual(values = c("#2c7bb6", "#fdae61", "#d7191c","#ffffff", "#ffffff", "#ffffff", "#ffffff")) +
  labs(y = "Response", x="Percent change in biological rate", 
       col = "treatment", shape="response type") +
  geom_errorbarh(aes(xmin=percentchange_lo_95, 
                     xmax=percentchange_hi_95), height=0) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_label(data=label_species, inherit.aes = F, aes(x=80, y=mean_order, label=common_name))
ggsave("figures/caterpillar_by_species.pdf", height=8, width=8)

#####add species for which sensitivities were estimated from Metabolic Index
#Anchovy MI: -19.4%
#Shrimp MI: -29.7%
#make a dataframe to plot Evan's species' responses.
MI_index_response<-data.frame(English_Name=c("anchovy","shrimp"), 
                              percentchange=c(-19.4, -29.7),
                              response_type="growth")
MI_index_response %>%
  ggplot(aes(x=percentchange, y=English_Name)) +
  theme_bw() +
  labs(y = "Species", x="Percent change in biological rate") +
  coord_cartesian(xlim=c(-75,180)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_point(size=2, shape=0) + theme_classic() +
  geom_label(aes(x=80, y=English_Name, label=English_Name)) +
  geom_vline(xintercept = 0, col="grey")
ggsave("figures/MI_sensitivities.pdf", width = 8, height = 1)

