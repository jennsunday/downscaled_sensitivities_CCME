#goal: visualize the postitive and negative cumulative effects 
# for each grid cell in the 12km model

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
  mutate(common_name=ifelse(common_name=="Canopy−forming Kelp", "Canopy-forming Kelp", common_name)) %>% #fix a very cryptic difference in dashes
  rename(English_Name=common_name)


#left join sensitivity data to depth based on species names
sensitivity_by_study<-left_join(sensitivity_by_study, depth_range, by="English_Name")


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
  filter(adult_zone != "bottom" | lower_depth>depth) %>% #filter out instances when ocean depth is beyond a species' lower depth
  filter(upper_depth<depth) %>% #filter out instances when ocean depth is above a species' upper depth
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95C
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>% #now, take a weighted mean per response type and species
  mutate(variance=((percentchange_hi_95-percentchange)/1.96)^2, weight=1/(variance+0.00000001)) %>%
  group_by(English_Name, response_type, lat, long) %>%
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
shelf_mask_12km<-read_csv("raw_data/new_downscaled_climate_data/mask_500m_12km.csv", col_names=F)
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
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = (mean_response))) +
  #geom_tile(data=coastline_mask12, fill=grey(0.4)) +
  #geom_tile(data=coastline_mask12, alpha = 0.0, color = "black", size = 1, linejoin = "round") +
  geom_tile(data=coastline_mask12, alpha = 1, fill="grey") +
  facet_wrap(~pos_neg, labeller = labeller(pos_neg=c("neg"="negative","pos"="positive"))) + 
  #scale_fill_gradient(low="yellow", high="blue") +
  scale_fill_gradient2(low = "darkred", mid = grey(0.9), high ="darkblue") +
  #scale_fill_gradient2()+
  #scale_fill_gradient(low="#f2f0f7", high="red") +
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
ggsave("figures/pos_neg_beside_12km.pdf", width=5, height=10)

#biscale

#spread the pos_neg into columns
pos_neg_spread_12<-pos_neg_grid_shelf12 %>%
  drop_na(pos_neg) %>%
  select(lat, long, pos_neg, mean_response) %>%
  spread(pos_neg, mean_response) %>%
  mutate(neg=abs(neg))
plot(pos_neg_spread_12$pos)

# create classes
# create density distribution of data in order to decide on 3 categories
pos_neg_spread_12 %>%
  ggplot(aes(x=neg)) +
  geom_density()
pos_neg_spread_12 %>%
  ggplot(aes(x=pos)) +
  geom_density()

#define thresholds for 3 classes, based roughly on interquartiles but fixed for pos and neg differences
uppercut_neg<-8
lowercut_neg<-4
highcut_pos<-12
uppercut_pos<-8
lowercut_pos<-4


#codify classes so they will work with bi_class
data_12<- pos_neg_spread_12 %>%
  mutate(bi_class_x=case_when(pos<lowercut_pos~1,
                              pos>=lowercut_pos & pos<uppercut_pos~2,
                              pos>=uppercut_pos & pos<highcut_pos~3,
                              pos>=highcut_pos~4,
                              TRUE ~ -999)) %>%
  mutate(bi_class_y=case_when(neg<lowercut_neg~1,
                              neg>=lowercut_neg & neg<uppercut_neg~2,
                              neg>=uppercut_neg~3,
                              TRUE ~ -999)) %>%
  mutate(bi_class=paste(as.character(bi_class_x),as.character(bi_class_y), sep="-"))%>%
  mutate(bi_class_fac=as.factor(bi_class))
# positive then negative

cols <- c("1_1" = "#E8E8E8", "1-2" = "#E2B8C0", "2_1" = "#BBD1EB", "2_2" = "#B7A3B8", "2_3"= "#984962", "3_2"="#8D8CB0", "3_3"= "#80396B", "4_2"= "#624D8D", "4_3"="#630E74")


data_12 %>%
  ggplot(aes(y = lat, x = long)) + 
  geom_tile(aes(fill = bi_class)) +
  scale_fill_manual(values=c("#E8E8E8", "#E2B8C0", "#BBD1EB", "#B7A3B8", "#984962", "#8D8CB0", "#80396B", "#624D8D", "#630E74")) +
  #bi_scale_fill(pal = custom_pal_alt, dim = 3) +
  #geom_tile(data=coastline_mask12, aes(y = lat, x = long), fill=grey(0.7)) +
  geom_tile(data=coastline_mask12, alpha = 0.0, color = "black", size = 1, linejoin = "round") +
  geom_tile(data=coastline_mask12, alpha = 1, fill=grey(0.95)) +
  bi_theme() +
  coord_cartesian(xlim=c(250,300), ylim=c(80, 380)) +
  #theme(legend.position = "none") +
  theme(axis.title=element_blank())


ggsave("figures/pos_neg_biplot_12km.png", width=4.5, height=13)
ggsave("figures/pos_neg_biplot_12km.pdf", width=4.5, height=13)

