#goal: visualize the postitive and negative cumulative effects 
# for each grid cell in the 2km model

library(tidyverse)
library(broom)
library(PNWColors)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(biscale)
library(cowplot)

all_deltas_2km<-read_csv("processed_data/all_deltas_2km.csv") # this is made in script 07
depth_2km<-read_csv("processed_data/depth_2km.csv")
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")
depth_range<-read_csv("raw_data/depth_distribution.csv")

#set seagrass to lower_depth of 30m instead of 10m to see more coverage
depth_range<-depth_range %>%
  mutate(lower_depth=ifelse(common_name=="seagrass", 30, lower_depth)) %>%
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
all_cells_deltas_2km<-left_join(all_deltas_2km, depth_2km, by=c("lat", "long", "latlong")) %>%
  mutate (treatment_var = case_when(treatment_var=="temp" ~ "temperature", 
                                    treatment_var=="oxy" ~ "oxygen",
                                    treatment_var=="CO2" ~ "CO2",
                                    treatment_var=="pH" ~ "pH",
                                    TRUE ~ "999"))


#expand sensitivity data and gridded delta data
#make a new percentchange and percentchangeSE for every combination of response and delta
pos_neg_grid<-left_join(sensitivity_by_study, all_cells_deltas_2km, by=c("treatment_var", "modelzone")) %>%
  drop_na(latlong) %>%
  drop_na(delta) %>%
  filter(adult_zone != "bottom" | lower_depth>depth) %>% #filter out instances when ocean depth is beyond a species' lower depth
  filter(upper_depth<depth) %>% #filter out instances when ocean depth is above a species' upper depth
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95C
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>%
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
coastline_mask2<-read_csv("processed_data/coastline_mask_2km.csv")

#get shelf mask and reshape shelf mask into long data
shelf_mask_2km<-read_csv("raw_data/new_downscaled_climate_data/mask_500m_2km.csv", col_names=F)
shelf_contour_2km<-melt(shelf_mask_2km) %>%
  mutate(lat=rep(1:dim(shelf_mask_2km)[1], dim(shelf_mask_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(shelf=value)

pos_neg_grid_shelf<-left_join(pos_neg_grid, shelf_contour_2km, by=(c("lat", "long"))) %>%
  filter(shelf==1)

df<-data.frame(lat=c(241, 128, 106), long=c(80, 80, 80), text=c("a", "b", "c"), text_col=c("1", "1", "2"))

pos_neg_grid_shelf %>% 
  na.omit(pos_neg) %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = abs(mean_response))) +
  geom_tile(data=coastline_mask2, fill=grey(0.4)) +
  geom_text(data=df, aes(label=text, col=text_col), size=7) +
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
#  geom_tile(data=df, colour = grey(0.1), lwd=0.1, aes(width=3, height=3)) +
  labs (fill="percent change") +
  theme(strip.text.x = element_text(size = 12))
ggsave("figures/pos_neg_beside_2km.png", height=7, width=7)

#biscale

#spread the pos_neg into columns
pos_neg_spread_2<-pos_neg_grid_shelf %>%
  drop_na(pos_neg) %>%
  select(lat, long, pos_neg, mean_response) %>%
  spread(pos_neg, mean_response) %>%
  mutate(neg=abs(neg))

# create classes
data <- bi_class(pos_neg_spread_2, y = pos, x = neg, style = "equal", dim = 3, keep_factors=T)
plot(pos_neg_spread_2$neg)

#define thresholds for 3 classes, based roughly on interquartiles but fixed for pos and neg differences
uppercut_neg<-8
lowercut_neg<-4
uppercut_pos<-8
lowercut_pos<-4



#codify classes so they will work with bi_class
data_2<- pos_neg_spread_2 %>%
  mutate(bi_class_x=case_when(pos<lowercut_pos~1,
                              pos>=lowercut_pos & pos<uppercut_pos~2,
                              pos>=uppercut_pos~3,
                             TRUE ~ -999)) %>%
 mutate(bi_class_y=case_when(neg<lowercut_neg~1,
                            neg>=lowercut_neg & neg<uppercut_neg~2,
                            neg>=uppercut_neg~3,
                             TRUE ~ -999)) %>%
 mutate(bi_class=paste(as.character(bi_class_y),as.character(bi_class_x), sep="-"))


#c(pnw_palette("Bay",12,type="continuous"))


custom_pal <- bi_pal_manual(val_1_1 = "#d9d9d9", val_1_2 = "#EDC939", val_1_3 = "#ED9106", 
                            val_2_1 = "#99C9C7", val_2_2 = "#73AA77", val_2_3= "#B5651E",
                            val_3_1 = "#0A7492", val_3_2 = "#055E80", val_3_3= "#19093D")

custom_pal_alt_2 <- bi_pal_manual(val_3_1 = "#64acbe",  val_3_2= "#627f8c", val_3_3= "#574249", 
                                val_2_1 = "#b0d5df", val_2_2 = "#ad9ea5", val_2_3 = "#985356",
                               val_1_1 = "#e8e8e8", val_1_2 = "#e4acac", val_1_3 = "#c85a5a")

custom_pal_alt <- bi_pal_manual(val_1_3 = "#64acbe",  val_2_3= "#627f8c", val_3_3= "#574249", 
                                val_1_2 = "#b0d5df", val_2_2 = "#ad9ea5", val_3_2 = "#985356",
                                val_1_1 = "#e8e8e8", val_2_1 = "#e4acac", val_3_1 = "#c85a5a")

#set up data frame with "a,b,c" labels for map
df<-data.frame(lat=c(241, 128, 106), long=c(80, 80, 80), text=c("a", "b", "c"), text_col=c("1", "1", "2"))

map<-data_2 %>%
  ggplot(aes(y = lat, x = long)) + 
  geom_tile(aes(fill = bi_class), show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal_alt, dim = 3) +
  geom_tile(data=coastline_mask2, aes(y = lat, x = long), fill=grey(0.7)) +
  geom_text(data=df, aes(label=text, col=text_col), size=7) + #add text for the example cells
  scale_colour_manual(values = c("white", "black")) +
  bi_theme() +
  theme(legend.position = "none") +
  theme(axis.title=element_blank())


legend <- bi_legend(pal = custom_pal_alt,
                    dim = 3,
                    ylab = "positive responses",
                    xlab = "negative responses",
                    size = 12)

ggdraw() +
  draw_plot(map, 0.1, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.4, 0.4) +
  draw_label(uppercut_neg, 0.075, 0.25)+
  draw_label(lowercut_neg, 0.075, 0.18)+
  draw_label(lowercut_pos, 0.18, 0.10)+
  draw_label(uppercut_pos, 0.27, 0.10)+
  draw_label("0", 0.08, 0.105)

ggsave("figures/pos_neg_biplot_2km.pdf", height=10, width=7)
ggsave("figures/pos_neg_biplot_2km.png", height=10, width=7)
