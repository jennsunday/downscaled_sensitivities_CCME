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

all_deltas_2km<-read_csv("processed_data/all_deltas_2km.csv") 


sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")

#adjust the names of the zones to set up the left_join
sensitivity_by_study<-sensitivity_by_study %>%
  mutate (modelzone = case_when(modelzone=="bottom" ~ "bot", 
            modelzone=="surface" ~ "surf",
            modelzone=="200m" ~ "200m",
            TRUE ~ "999"))

#expand sensitivity data and gridded delta data
#make a new percentchange and percentchangeSE for every combination of response and delta
pos_neg_grid<-left_join(sensitivity_by_study, all_deltas_2km, by=c("treatment_var", "modelzone")) %>%
  drop_na(latlong) %>%
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95CI
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>%
  mutate(pos_neg=ifelse(percentchange>0, "pos", "neg")) %>%
  group_by(lat, long, pos_neg) %>%
  mutate(variance=percentchangeSE^2, weight=1/(variance+0.00000001)) %>%
  dplyr::summarize(weighted_sensitivity=weighted.mean(percentchange, w=weight), 
                   weight_test=sum(weight*percentchange)/sum(weight),
                   n=n(), 
                   weight_sum=sum(weight), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_sensitivity-1.96*SE_wmean, hi_95=weighted_sensitivity+1.96*SE_wmean)


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
  filter(shelf==1)


pos_neg_grid_shelf %>% 
  na.omit(pos_neg) %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = abs(weighted_sensitivity))) +
  geom_tile(data=coastline_mask, fill=grey(0.4)) +
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
ggsave("figures/pos_neg_beside_2km.png", height=7, width=7)

#biscale

#spread the pos_neg into columns
pos_neg_spread_2<-pos_neg_grid_shelf %>%
  drop_na(pos_neg) %>%
  select(lat, long, pos_neg, weighted_sensitivity) %>%
  spread(pos_neg, weighted_sensitivity) %>%
  mutate(neg=abs(neg))

# create classes
#data <- bi_class(pos_neg_spread_2, x = pos, y = neg, style = "equal", dim = 3, keep_factors=T)

plot(pos_neg_spread_2$neg)
#define thresholds for 3 classes
uppercut_neg<-5
lowercut_neg<-3
uppercut_pos<-10
lowercut_pos<-5

quantile(pos_neg_spread_2$pos, c(0.10, 0.90))

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
  mutate(bi_class=paste(as.character(bi_class_x),as.character(bi_class_y), sep="-"))


map_2<-data_2 %>%
  ggplot(aes(y = lat, x = long)) + 
  geom_tile(aes(fill = bi_class), show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  geom_tile(data=coastline_mask, aes(y = lat, x = long), fill=grey(0.3)) +
  bi_theme() +
  theme(axis.title=element_blank())

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "positive responses",
                    ylab = "negative responses",
                    size = 11)

ggdraw() +
  draw_plot(map_2, 0.1, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.4, 0.4) +
  draw_label(uppercut_neg, 0.075, 0.25)+
  draw_label(lowercut_neg, 0.075, 0.18)+
  draw_label(lowercut_pos, 0.18, 0.10)+
  draw_label(uppercut_pos, 0.27, 0.10)+
  draw_label("0", 0.08, 0.105)

ggsave("figures/pos_neg_biplot_2km.png", height=10, width=7)
