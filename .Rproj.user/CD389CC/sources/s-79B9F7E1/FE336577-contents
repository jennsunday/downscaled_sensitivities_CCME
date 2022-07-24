#goal calculate weighted mean within species and response types
#make caterpillar plot of meta-analyzed responses

library(tidyverse)
library(broom)
library(PNWColors)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(biscale)
library(ggridges)
library("scales")

all_deltas_2km<-read_csv("processed_data/all_deltas_2km.csv") 
depth_2km<-read_csv("processed_data/depth_2km.csv")
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")
depth_range<-read_csv("raw_data/depth_distribution.csv")
#species_order.df<-read_csv("processed_data/species_order.csv")


#set seagrass to lower_depth of 30m instead of 10m to see more coverage
depth_range<-depth_range %>%
  mutate(lower_depth=ifelse(common_name=="seagrass", 30, lower_depth)) %>%
  mutate(common_name=ifelse(common_name=="Canopy−forming Kelp", "Canopy-forming Kelp", common_name)) %>% #fix a very cryptic difference in dashes
  rename(English_Name=common_name) 

#left join sensitivity data to depth based on species names
sensitivity_by_study<-left_join(sensitivity_by_study, depth_range, by="English_Name") 



#adjust the names of the zones to set up the left_join
sensitivity_by_study<-sensitivity_by_study %>%
  mutate (modelzone = case_when(modelzone=="bottom" ~ "bot", 
                                modelzone=="surface" ~ "surf",
                                modelzone=="200m" ~ "200m",
                                TRUE ~ "999")) %>%
  mutate (treatment_var2 = ifelse(treatment_var %in% c("pH", "CO2"), "pH & CO2", treatment_var))


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
  filter(adult_zone != "bottom" | lower_depth>depth) %>%  #filter out instances when ocean depth is beyond a species' lower depth
  filter(upper_depth<depth) %>% #filter out instances when ocean depth is above a species' upper depth
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95C
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>%
  mutate(pos_neg=ifelse(percentchange>0, "pos", "neg")) %>%
  group_by(English_Name, unique_study, treatment_var, response_type, Life_stage_category) %>%
  summarize(mean_percentchange=mean(percentchange), #get mean mean, mean low, and mean high, across grid cells
            mean_percentchange_lo_95=mean(percentchange_lo_95),
            mean_percentchange_hi_95=mean(percentchange_hi_95)) %>%
  ungroup() %>%  #now, take a weighted mean per response type and species
  mutate(variance=((mean_percentchange_hi_95-mean_percentchange)/1.96)^2, weight=1/(variance+0.00000001)) %>%
  group_by(English_Name, response_type) %>%
  dplyr::summarize(weighted_response=weighted.mean(mean_percentchange, w=weight), 
                   n=n(), weight=mean(weight), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_response-1.96*SE_wmean, hi_95=weighted_response+1.96*SE_wmean) %>%
  ungroup()


###############
#plot data
###############

meta_responses %>%
  group_by(English_Name) %>%
  mutate(species_order=mean(weighted_response)) %>%
  ungroup() %>%
  arrange(species_order) %>%
  ungroup() %>%
  mutate(compound_order = as.factor(species_order)) %>%
  group_by(English_Name, compound_order) %>%
  summarise(mean_order=mean(species_order),
            max_response=max(weighted_response)) -> label_species

dodge <- position_dodge(.5)

meta_plot<-meta_responses %>%
  group_by(English_Name) %>%
  mutate(species_order=mean(weighted_response)) %>%
  ungroup() %>%
  arrange(species_order) %>%
  ungroup() %>%
  mutate(compound_order = as.factor(species_order)) %>%
  ggplot(aes(x=weighted_response, y=compound_order, shape=response_type,
             col=response_type, fill=response_type)) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-45,95)) +
  theme_classic() +
  geom_path(aes(group=English_Name), col=grey(0.8), lwd=1) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25))+
  scale_color_manual(values = pnw_palette("Bay",7)) +
  scale_fill_manual(values = pnw_palette("Bay",7)) +
  geom_point(aes(size=1/SE_wmean), position = dodge) +
  geom_errorbarh(aes(xmin=lo_95, 
                     xmax=hi_95), height=0, position = dodge) +
  #geom_pointrange(aes(xmin=lo_95, 
  #                    xmax=hi_95),
  #                position=position_jitter(w = 0, h = 0.2), 
  #                fatten=rescale(1/meta_responses$SE_wmean, to = c(2, 9))) + 
  labs(col = "response type", shape="response type", fill="response type") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.position = "none") +
  geom_text(data=label_species, inherit.aes = F, hjust = 0, nudge_x= 6, nudge_y=-0.2, 
            aes(x=max_response, y=compound_order, label=English_Name), size=3) 



summary_type<-meta_responses %>%
  ggplot(aes(x=weighted_response, 
             fill=response_type, col=response_type, 
             shape=response_type, 
             y=response_type)) +
  geom_vline(xintercept = 0, col="grey") +
  geom_point(aes(size=1/SE_wmean), position = dodge) +
  geom_errorbarh(aes(xmin=lo_95, 
                     xmax=hi_95), height=0, position = dodge) +
  #geom_pointrange(aes(xmin=lo_95, 
  #                    xmax=hi_95), 
  #                position=position_jitter(w = 0.2, h = 0.2), 
  #                fatten=rescale(1/meta_responses$SE_wmean, to = c(2, 7))) + 
  theme_classic() +
  scale_y_discrete(limits = rev(levels(as.factor(meta_responses$response_type)))) +
  scale_color_manual(values = pnw_palette("Bay",7)) +
  scale_fill_manual(values = pnw_palette("Bay",7)) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  coord_cartesian(xlim=c(-45,95)) +
  labs(x="Percent change in biological rate", x="weighted response", 
       col = "response type", fill="response type", shape="response type")

#have a look
ggdraw() +
  draw_plot(meta_plot, 0, 0.3, 1, 0.7) +
  draw_label("b", 0.03, 0.95) +
  draw_plot(summary_type, 0, 0, 1, 0.3) +
  draw_label("c", 0.03, 0.25)
  

###
bigcat<-meta_responses %>%
  arrange(weighted_response) %>%
  mutate(new_order = c(1:n())) %>%
  ggplot(aes(x=weighted_response, y=new_order, 
             shape=response_type, col=response_type, fill=response_type)) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-45,95)) +
  geom_point(aes(size=1/SE_wmean)) + theme_classic() +
  #geom_path(aes(group=1)) +
  scale_color_manual(values = pnw_palette("Bay",7)) +
  scale_fill_manual(values = c(pnw_palette("Bay",7), pnw_palette("Bay",2))) +
  labs(y = "Response", x="Percent change in biological rate", 
       col = "response type", shape="response type", fill="response type") +
  scale_shape_manual(values=c(21, 22, 23, 24, 25))+
  geom_errorbarh(aes(xmin=lo_95, 
                     xmax=hi_95), height=0) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(inherit.aes=F, hjust = 0, nudge_x= 3, nudge_y=-0.3,
            aes(label=English_Name, x=weighted_response, y=new_order), size=2)

#bring them together
ggdraw() +
  draw_plot(bigcat, 0, 0, 0.55, 1) +
  draw_plot(meta_plot, 0.55, 0.3, 0.4, 0.7) +
  draw_plot(summary_type, 0.55, 0, 0.4, 0.3) +
  draw_label("a", 0.03, 0.97) +
  draw_label("b", 0.58, 0.97) +
  draw_label("c", 0.58, 0.27)
ggsave("figures/full_response_meta_fig.png", height=6, width=9) #save
ggsave("figures/full_response_meta_fig.pdf", height=6, width=9) #save
