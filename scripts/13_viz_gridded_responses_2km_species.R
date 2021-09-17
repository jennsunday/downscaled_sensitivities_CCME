#goal: visualize the postitive and negative cumulative effects 
# separated by species, for the 2km model

library(tidyverse)
library(broom)
library(PNWColors)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(biscale)

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


sensitivity_by_study$lower_depth
unique(depth_range$common_name)
#adjust the names of the zones to set up the left_join
sensitivity_by_study<-sensitivity_by_study %>%
  mutate (modelzone = case_when(modelzone=="bottom" ~ "bot", 
            modelzone=="surface" ~ "surf",
            modelzone=="200m" ~ "200m",
            TRUE ~ "999"))

all_cells_deltas_2km<-all_deltas_2km %>%
  mutate (treatment_var = case_when(treatment_var=="temp" ~ "temperature", 
                                treatment_var=="oxy" ~ "oxygen",
                                treatment_var=="CO2" ~ "CO2",
                                treatment_var=="pH" ~ "pH",
                                TRUE ~ "999"))


pos_neg_grid_species<-left_join(sensitivity_by_study, all_cells_deltas_2km, by=c("treatment_var", "modelzone")) %>%
  drop_na(latlong) %>%
  drop_na(delta) %>%
  filter(adult_zone != "benthic" | lower_depth>depth) %>% #filter out instances when ocean depth is beyond a species' lower depth
  filter(upper_depth<depth) %>% #filter out instances when ocean depth is above a species' upper depth
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95C
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>% 
  mutate(variance=((percentchange_hi_95-percentchange)/1.96)^2, weight=1/(variance+0.00000001)) %>%
  group_by(common_name, response_type, lat, long) %>% #now, take a weighted mean per response type and species inside each grid cell
  dplyr::summarize(weighted_response=weighted.mean(percentchange, w=weight), 
                   n=n(), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_response-1.96*SE_wmean, hi_95=weighted_response+1.96*SE_wmean) %>%
  ungroup() %>%
  mutate(pos_neg=ifelse(weighted_response>0, "pos", "neg")) %>%
  group_by(pos_neg, lat, long, common_name) %>%
  summarize(mean_response=mean(weighted_response))


#get coastline mask
coastline_mask2<-read_csv("processed_data/coastline_mask_2km.csv")

#remove coastline 
#get shelf mask and reshape shelf mask into long data
#shelf_mask_2km<-read_csv("raw_data/downscaled_climate_data/mask_500m_2km.csv", col_names=F)
#shelf_contour_2km<-melt(shelf_mask_2km) %>%
#  mutate(lat=rep(1:dim(shelf_mask_2km)[1], dim(shelf_mask_2km)[2])) %>%
#  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
#  mutate(long=as.numeric(long))%>%
#  mutate(lat=as.numeric(lat))%>%
#  mutate(latlong=paste(long, lat, sep="_")) %>%
#  rename(shelf=value)


#pos_neg_grid_shelf<-left_join(pos_neg_grid, shelf_contour_2km, by=(c("lat", "long"))) %>%
#  filter(shelf!=0)

# New facet label names for dose variable
species.labs <- pos_neg_grid_species %>%
  group_by(common_name, lat, long) %>%
  summarize(mean_ws=(mean(mean_response))) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(order=(mean(mean_ws))) %>%
  .$common_name

species_order<-pos_neg_grid_species %>%
  group_by(common_name, lat, long) %>%
  summarize(mean_ws=(mean(mean_response))) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(order=(mean(mean_ws))) %>%
  .$order

#prep a data from to replace labels
names(species.labs) <- species_order 

#prep a dataframe to left_join for a new order
species_order.df<-pos_neg_grid_species %>%
  group_by(common_name, lat, long) %>%
  summarize(mean_ws=(mean(mean_response))) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(order=(mean(mean_ws)))
write_csv(species_order.df, "processed_data/species_order.csv")

#replace order according to above
pos_neg_grid_species<-left_join(pos_neg_grid_species, species_order.df, by="common_name")

pos_neg_grid_species %>% 
  #filter(common_name=="razor clam") %>%
  na.omit(pos_neg) %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = abs(mean_response))) +
  geom_tile(data=coastline_mask2, fill=grey(0.4)) +
  facet_grid(pos_neg~order, labeller=labeller(order = species.labs,
                                              pos_neg=c("neg"="negative","pos"="positive"))) + 
  scale_fill_gradient(low="#ffeda0", high="#d7191c") +
  #scale_fill_distiller() +
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
  theme(strip.text.x = element_text(size = 6))
ggsave("figures/pos_neg_species_beside_2km.png", height=4, width=10)

