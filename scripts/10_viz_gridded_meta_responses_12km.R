#goal subset to speices present at the depth of that grid cell <- must await Darren's depth layer
#calculate every response in every grid cell
#take weighted means at various levels, especially weighted mean of negative responses and of positive responses

library(tidyverse)
library(broom)
library(PNWColors)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(biscale)
library(cowplot)

all_deltas_12km<-read_csv("processed_data/all_deltas_12km.csv") 
depth_12km<-read_csv("processed_data/depth_12km.csv")
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")
depth_range<-read_csv("raw_data/depth_distribution.csv")

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
            TRUE ~ "999"))

#left join depth data to delta data for subsetting, and change variables to set up the left_join
all_cells_deltas_12km<-left_join(all_deltas_12km, depth_12km, by=c("lat", "long", "latlong")) %>%
  mutate (treatment_var = case_when(treatment_var=="temp" ~ "temperature", 
                                    treatment_var=="oxy" ~ "oxygen",
                                    treatment_var=="CO2" ~ "CO2",
                                    treatment_var=="pH" ~ "pH",
                                    TRUE ~ "999"))

#expand sensitivity data and gridded delta data
#make a new percentchange and percentchangeSE for every combination of response and delta
pos_neg_grid12<-left_join(sensitivity_by_study, all_cells_deltas_12km, by=c("treatment_var", "modelzone")) %>%
  drop_na(latlong) %>%
  drop_na(delta) %>%
  filter(adult_zone != "benthic" | lower_depth>depth) %>% #filter out instances when ocean depth is beyond a species' lower depth
  filter(upper_depth<depth) %>% #filter out instances when ocean depth is above a species' upper depth
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95C
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>% #now, take a weighted mean per response type and species
  mutate(variance=((percentchange_hi_95-percentchange)/1.96)^2, weight=1/(variance+0.00000001)) %>%
  group_by(common_name, response_type, lat, long) %>%
  dplyr::summarize(weighted_response=weighted.mean(percentchange, w=weight), 
                   n=n(), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_response-1.96*SE_wmean, hi_95=weighted_response+1.96*SE_wmean) %>%
  ungroup() %>%
  mutate(pos_neg=ifelse(weighted_response>0, "pos", "neg")) %>%
  group_by(pos_neg, lat, long) %>%
  summarize(mean_response=mean(weighted_response))

#get coastline mask
coastline_mask12<-read_csv("processed_data/coastline_mask_12km.csv")

#get shelf mask and reshape shelf mask into long data
shelf_mask_12km<-read_csv("raw_data/downscaled_climate_data/mask_500m_12km.csv", col_names=F)
shelf_contour_12km<-melt(shelf_mask_12km) %>%
  mutate(lat=rep(1:dim(shelf_mask_12km)[1], dim(shelf_mask_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(shelf=value)


pos_neg_grid_shelf12<-left_join(pos_neg_grid12, shelf_contour_12km, by=(c("lat", "long"))) %>%
  filter(shelf==1)


pos_neg_grid_shelf12 %>% 
  na.omit(pos_neg) %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = abs(mean_response))) +
  geom_tile(data=coastline_mask12, fill=grey(0.4)) +
  facet_wrap(~pos_neg, labeller = labeller(pos_neg=c("neg"="negative","pos"="positive"))) + 
  #scale_fill_gradient(low="yellow", high="blue") +
  scale_fill_gradient(low="#f2f0f7", high="#54278f") +
  theme_classic() + theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.line.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank(),
                          axis.line.y=element_blank(),
                          strip.background=element_blank()) +
  labs (fill="percent change") +
  theme(strip.text.x = element_text(size = 12))
ggsave("figures/pos_neg_beside_12km.png")

#biscale

#spread the pos_neg into columns
pos_neg_spread_12<-pos_neg_grid_shelf12 %>%
  drop_na(pos_neg) %>%
  select(lat, long, pos_neg, mean_response) %>%
  spread(pos_neg, mean_response) %>%
  mutate(neg=abs(neg))

# create classes
#data <- bi_class(pos_neg_spread, x = pos, y = neg, style = "equal", dim = 3)


#define thresholds for 3 classes
uppercut_neg<-8
lowercut_neg<-4
uppercut_pos<-8
lowercut_pos<-4

#codify classes so they will work with bi_class
data_12<- pos_neg_spread_12 %>%
  mutate(bi_class_x=case_when(pos<lowercut_pos~1,
                              pos>=lowercut_pos & pos<uppercut_pos~2,
                              pos>=uppercut_pos~3,
                              TRUE ~ -999)) %>%
  mutate(bi_class_y=case_when(neg<lowercut_neg~1,
                              neg>=lowercut_neg & neg<uppercut_neg~2,
                              neg>=uppercut_neg~3,
                              TRUE ~ -999)) %>%
  mutate(bi_class=paste(as.character(bi_class_x),as.character(bi_class_y), sep="-"))

custom_pal_alt <- bi_pal_manual(val_1_1 = "#d9d9d9", val_1_2 = "#fec5bb", val_1_3 = "#f25c54", 
                            val_2_1 = "#caf0f8", val_2_2 = "#8e7dbe", val_2_3= "#a01a58",
                            val_3_1 = "#56cfe1", val_3_2 = "#0077b6", val_3_3= "#3c1642")

custom_pal <- bi_pal_manual(val_1_1 = "#d9d9d9", val_1_2 = "#EDC939", val_1_3 = "#ED9106", 
                            val_2_1 = "#99C9C7", val_2_2 = "#73AA77", val_2_3= "#B5651E",
                            val_3_1 = "#0A7492", val_3_2 = "#055E80", val_3_3= "#19093D")


data_12 %>%
  ggplot(aes(y = lat, x = long)) + 
  geom_tile(aes(fill = bi_class), show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  geom_tile(data=coastline_mask12, aes(y = lat, x = long), fill=grey(0.7)) +
  bi_theme() +
  coord_cartesian(xlim=c(250,300), ylim=c(80, 380)) +
  theme(axis.title=element_blank())


ggsave("figures/pos_neg_biplot_12km.png", width=3, height=13)

