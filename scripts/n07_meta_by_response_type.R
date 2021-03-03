#goal calculate weighted mean within species and response types
#make catepillar plot of meta-analyzed responses

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
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")
depth_range<-read_csv("raw_data/depth_distribution.csv")
species_order.df<-read_csv("processed_data/species_order.csv")


#set seagrass to lower_depth of 30m instead of 10m to see more coverage
depth_range<-depth_range %>%
  mutate(lower_depth=ifelse(common_name=="seagrass", 30, lower_depth))

#left join sensitivity data to depth based on species names
sensitivity_by_study<-left_join(sensitivity_by_study, depth_range, by="common_name")



#adjust the names of the zones to set up the left_join
sensitivity_by_study<-sensitivity_by_study %>%
  mutate (modelzone = case_when(modelzone=="bottom" ~ "bot", 
                                modelzone=="surface" ~ "surf",
                                modelzone=="200m" ~ "200m",
                                TRUE ~ "999")) %>%
  mutate (treatment_var2 = ifelse(treatment_var %in% c("pH", "CO2"), "pH & CO2", treatment_var))
unique(all_deltas_2km$treatment_var)

#left join depth data to delta data for subsetting, and change variables to set up the left_join
all_deltas_2km<-left_join(all_deltas_2km, depth_2km, by=c("lat", "long", "latlong")) %>%
  mutate (treatment_var = case_when(treatment_var=="temp" ~ "temperature", 
                                    treatment_var=="oxy" ~ "oxygen",
                                    treatment_var=="CO2" ~ "CO2",
                                    treatment_var=="pH" ~ "pH",
                                    TRUE ~ "999"))


#expand sensitivity data and gridded delta data
#make a new percentchange and percentchangeSE for every combination of response and delta
meta_responses<-left_join(sensitivity_by_study, all_deltas_2km, by=c("treatment_var", "modelzone")) %>%
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
  ungroup() %>%  #now, take a weighted mean per response type and species
  mutate(variance=((mean_percentchange_hi_95-mean_percentchange)/1.96)^2, weight=1/(variance+0.00000001)) %>%
  group_by(common_name, response_type) %>%
  dplyr::summarize(weighted_response=weighted.mean(mean_percentchange, w=weight), 
                   n=n(), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_response-1.96*SE_wmean, hi_95=weighted_response+1.96*SE_wmean) %>%
  ungroup()


###############
#plot data
###############

#replace order according to above
meta_responses<-left_join(meta_responses, species_order.df, by="common_name")

label_species<- meta_responses %>%
  group_by(common_name) %>%
  mutate(species_order=mean(weighted_response)) %>%
  ungroup() %>%
  arrange(species_order, weighted_response) %>%
  ungroup() %>%
  mutate(compound_order = c(1:n())) %>%
  group_by(common_name) %>%
  summarize(mean_order=mean(compound_order), 
            mean_response=mean(weighted_response), 
            min_response=min(weighted_response), 
            max_response=max(weighted_response)) %>%
  mutate(x_position=max_response+30) %>%
ungroup()


meta_responses %>%
  group_by(common_name) %>%
  mutate(species_order=mean(weighted_response)) %>%
  ungroup() %>%
  arrange(species_order, weighted_response) %>%
  ungroup() %>%
  mutate(compound_order = c(1:n())) %>%
  ggplot(aes(x=weighted_response, y=compound_order, shape=response_type, col=response_type)) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-45,45)) +
  geom_point(size=2) + theme_classic() +
  geom_path(aes(group=common_name)) +
  scale_color_manual(values = pnw_palette("Bay",5)) +
  labs(y = "Response", x="Percent change in biological rate", 
       col = "response type", shape="response type") +
  geom_errorbarh(aes(xmin=lo_95, 
                     xmax=hi_95), height=0) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(data=label_species, inherit.aes = F, hjust = 0, nudge_x= 6, nudge_y=-0.3, 
             aes(x=mean_response, y=mean_order, label=common_name), size=3) 
ggsave("figures/caterpillar_by_species_mata.png", height=8, width=8)

