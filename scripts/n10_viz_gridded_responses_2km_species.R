#goal subset to species present at the depth of that grid cell <- must await Darren's depth layer
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

all_deltas_2km<-read_csv("processed_data/all_deltas_2km.csv") 

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")


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
pos_neg_grid<-left_join(sensitivity_by_study, all_deltas_2km, by=c("treatment_var", "modelzone")) %>%
  drop_na(latlong) %>%
  drop_na(delta) %>%
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95CI
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>%
  mutate(pos_neg=ifelse(percentchange>0, "pos", "neg")) %>%
  group_by(lat, long, pos_neg, common_name) %>%
  mutate(variance=percentchangeSE^2, weight=1/(variance+0.00000001)) %>%
  dplyr::summarize(weighted_sensitivity=weighted.mean(percentchange, w=weight), 
                   n=n(), 
                   weight_sum=sum(weight), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_sensitivity-1.96*SE_wmean, hi_95=weighted_sensitivity+1.96*SE_wmean) %>%
  ungroup()


#get coastline mask
coastline_mask<-read_csv("processed_data/coastline_mask_2km.csv")

#get shelf mask and reshape shelf mask into long data
shelf_mask_2km<-read_csv("raw_data/downscaled_climate_data/mask_500m_2km.csv", col_names=F)
shelf_contour_2km<-melt(shelf_mask_2km) %>%
  mutate(lat=rep(1:dim(shelf_mask_2km)[1], dim(shelf_mask_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(shelf=value)


pos_neg_grid_shelf<-left_join(pos_neg_grid, shelf_contour_2km, by=(c("lat", "long"))) %>%
  filter(shelf!=0)

# New facet label names for dose variable
species.labs <- pos_neg_grid_shelf  %>%
  group_by(common_name, latlong) %>%
  summarize(mean_ws=(mean(weighted_sensitivity))) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(order=(mean(mean_ws))) %>%
  .$common_name

species_order<-pos_neg_grid_shelf %>%
  group_by(common_name, latlong) %>%
  summarize(mean_ws=(mean(weighted_sensitivity))) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(order=(mean(mean_ws))) %>%
  .$order

names(species.labs) <- species_order

species_order.df<-data.frame(common_name=unique(pos_neg_grid_shelf$common_name), order=species_order)

pos_neg_grid_shelf<-left_join(select(pos_neg_grid_shelf, -order), species_order.df)

pos_neg_grid_shelf %>% 
  #filter(common_name=="razor clam") %>%
  na.omit(pos_neg) %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = abs(weighted_sensitivity))) +
  geom_tile(data=coastline_mask, fill=grey(0.4)) +
  facet_grid(pos_neg~order, labeller = labeller(pos_neg=c("neg"="negative","pos"="positive"),
                                                order=species.labs)) + 
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



