#goal subset to species present at the depth of that grid cell
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

unique(sensitivity_by_study$response_type)
#expand sensitivity data and gridded delta data
#make a new percentchange and percentchangeSE for every combination of response and delta
response_grid<-left_join(sensitivity_by_study, all_deltas_2km, by=c("treatment_var", "modelzone")) %>%
  drop_na(latlong) %>%
  drop_na(delta) %>%
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95CI
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>%
  mutate(pos_neg=ifelse(percentchange>0, "pos", "neg")) %>%
  group_by(lat, long, pos_neg, response_type) %>%
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


response_shelf<-left_join(response_grid, shelf_contour_2km, by=(c("lat", "long"))) %>%
  filter(shelf!=0)



response_shelf %>% 
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = abs(weighted_sensitivity))) +
  geom_tile(data=coastline_mask, fill=grey(0.4)) +
  facet_grid(pos_neg~response_type) + 
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
  scale_fill_gradient(low="#ffeda0", high="#d7191c") +
  theme(strip.text.x = element_text(size = 6))
ggsave("figures/pos_neg_responses_beside_2km.png", height=4, width=10)


#biscale

#spread the pos_neg into columns
pos_neg_spread<-response_shelf %>%
  drop_na(weighted_sensitivity) %>%
  select(lat, long, pos_neg, weighted_sensitivity, response_type) %>%
  spread(pos_neg, weighted_sensitivity) %>%
  mutate(neg=abs(neg))

#define thresholds for 3 classes
uppercut_neg<-10
lowercut_neg<-5
uppercut_pos<-10
lowercut_pos<-5

plot(pos_neg_spread$pos)
quantile(pos_neg_spread$neg, c(0.10, 0.90), na.rm=T)

#codify classes so they will work with bi_class
data<- pos_neg_spread %>%
  mutate(bi_class_x=case_when(pos<lowercut_pos~1,
                              pos>=lowercut_pos & pos<uppercut_pos~2,
                              pos>=uppercut_pos~3,
                              is.na(pos) ~ 1,
                              TRUE ~ -999)) %>%
  mutate(bi_class_y=case_when(neg<lowercut_neg~1,
                              neg>=lowercut_neg & neg<uppercut_neg~2,
                              neg>=uppercut_neg~3,
                              TRUE ~ -999)) %>%
  mutate(bi_class=paste(as.character(bi_class_x),as.character(bi_class_y), sep="-"))
unique(data$bi_class_x)

map<-data %>%
  #filter(common_name=="ochre star") %>%
  ggplot(aes(y = lat, x = long)) + 
  geom_tile(aes(fill = bi_class), show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  geom_tile(data=coastline_mask, aes(y = lat, x = long), fill=grey(0.3)) +
  theme_classic()+
  facet_wrap(~response_type, nrow=1) +
  theme(axis.title=element_blank())

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "positive responses",
                    ylab = "negative responses",
                    size = 10)

ggdraw() +
  draw_plot(map, 0, 0, 0.8, 1) +
  draw_plot(legend, 0.8, 0, 0.2, 0.4)

ggsave("figures/response_biplot_2km.png", height=4, width=8)
