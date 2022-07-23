#goal: for 3 illustrative grid cells in the domain, 
#display weighted mean within species and response types 
#for species that occupy that depth
#in catepillar plot

library(tidyverse)
library(broom)
library(PNWColors)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(biscale)
library(cowplot)
library(grid)
library(ggrepel)


all_deltas_2km<-read_csv("processed_data/all_deltas_2km.csv") 
depth_2km<-read_csv("processed_data/depth_2km.csv")
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")
depth_range<-read_csv("raw_data/depth_distribution.csv")

#correct an error
depth_range<-depth_range %>%
 mutate(common_name=ifelse(common_name=="Canopy−forming Kelp", "Canopy-forming Kelp", common_name)) %>% #fix a very cryptic difference in dashes
  rename(English_Name=common_name)

#left join sensitivity data to depth based on species names
sensitivity_by_study<-left_join(sensitivity_by_study, depth_range, by="English_Name")

#create dataframe of three example locations
#df<-data.frame(lat=c(191, 128, 108), long=c(95, 80, 80), text=c("a", "b", "c"), text_col=c("1", "1", "2"))
df<-data.frame(lat=c(241, 198, 115), long=c(83, 83, 83), text=c("a", "b", "c"), text_col=c("1", "1", "2"))


#adjust the names of the zones to set up the left_join
sensitivity_by_study<-sensitivity_by_study %>%
  mutate (modelzone = case_when(modelzone=="bottom" ~ "bot", 
                                modelzone=="surface" ~ "surf",
                                modelzone=="200m" ~ "200m",
                                TRUE ~ "999")) %>%
  mutate (treatment_var2 = ifelse(treatment_var %in% c("pH", "CO2"), "pH & CO2", treatment_var))

#left join depth data to delta data for subsetting, and change variables to set up the left_join
one_cell_deltas_2km<-left_join(all_deltas_2km, depth_2km, by=c("lat", "long", "latlong")) %>%
  mutate (treatment_var = case_when(treatment_var=="temp" ~ "temperature", 
                                    treatment_var=="oxy" ~ "oxygen",
                                    treatment_var=="CO2" ~ "CO2",
                                    treatment_var=="pH" ~ "pH",
                                    TRUE ~ "999")) %>%
  filter(latlong %in% c("83_241", "83_198", "83_115")) #filter to example 3 latlongs
  

#expand sensitivity data and gridded delta data
#make a new percentchange and percentchangeSE for every combination of response and delta
meta_responses_one_cell<-left_join(sensitivity_by_study, one_cell_deltas_2km, by=c("treatment_var", "modelzone")) %>%
  drop_na(delta) %>%
  filter(adult_zone != "bottom" | lower_depth>depth) %>% #filter out instances when ocean depth is beyond a species' lower depth
  filter(upper_depth<depth) %>% #filter out instances when ocean depth is above a species' upper depth
  mutate(percentchange=mean_estimate*delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*delta*100) %>% #calculate high 95C
  mutate(percentchangeSE=abs(se_estimate)*delta*100) %>%
  mutate(pos_neg=ifelse(percentchange>0, "pos", "neg")) %>%
  group_by(English_Name, unique_study, treatment_var, response_type, Life_stage_category, latlong) %>%
  summarize(mean_percentchange=mean(percentchange), #get mean mean, mean low, and mean high, across grid cells
            mean_percentchange_lo_95=mean(percentchange_lo_95),
            mean_percentchange_hi_95=mean(percentchange_hi_95)) %>%
  ungroup() %>%  #now, take a weighted mean per response type and species
  mutate(variance=((mean_percentchange_hi_95-mean_percentchange)/1.96)^2, weight=1/(variance+0.00000001)) %>%
  group_by(English_Name, response_type, latlong) %>%
  dplyr::summarize(weighted_response=weighted.mean(mean_percentchange, w=weight), 
                   n=n(), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_response-1.96*SE_wmean, hi_95=weighted_response+1.96*SE_wmean) %>%
  ungroup() 


###############
#plot data
###############

#make a new dataframe that summarizes mean positive and mean negative effects, for bar chart
pos_neg_one_cell<-meta_responses_one_cell %>%
  arrange(weighted_response) %>%
  ungroup()%>%
  mutate(new_order = c(1:n())) %>%
  mutate(pos_neg=ifelse(weighted_response>0, "pos", "neg")) %>%
  group_by(pos_neg, latlong) %>%
  summarize(mean_response=mean(weighted_response),
            min_order=min(new_order),
            max_order=max(new_order),
            mean_order=(max_order+min_order)/2,
            width_order=max_order-min_order) %>%
  ungroup() %>%
  mutate(xmin=rep(0, 6),
         ymax=c(rep(-1,3), rep(-3, 3)),
         ymin=c(rep(-3, 3), rep(-5, 3))) %>%
  mutate(new_order = c(1:n()),
         cell_example=case_when(latlong=="83_241"~"a",
                                latlong=="83_198"~"b",
                                latlong=="83_115"~"c"))



  
#make plot
indv_cells<-meta_responses_one_cell %>%
  arrange(weighted_response) %>%
  mutate(new_order = c(1:n()),
         cell_example=case_when(latlong=="83_241"~"a",
                                latlong=="83_198"~"b",
                                latlong=="83_115"~"c")) %>%
  ggplot(aes(x=weighted_response, y=new_order, 
             shape=response_type)) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-45,105)) +
  geom_point(aes(size=1/SE_wmean), col=grey(0.5), fill=grey(0.5)) + theme_classic() +
  geom_path(aes(group=1), col=grey(0.5)) +
  scale_color_manual(values = pnw_palette("Bay",7)) +
  scale_fill_manual(values = c(pnw_palette("Bay",7), pnw_palette("Bay",2))) +
  labs(y = "Response", x="Percent change in biological rate", 
       col = "response type", shape="response type", fill="response type") +
  scale_shape_manual(values=c(21, 22, 23, 24, 25))+
  geom_errorbarh(aes(xmin=lo_95, 
                     xmax=hi_95), height=0, col=grey(0.5)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(legend.position = "none")+
  geom_rect(data=pos_neg_one_cell, inherit.aes=F, 
            aes(xmax=mean_response, 
                xmin=xmin, ymin=ymin, ymax=ymax), 
            fill=case_when(pos_neg_one_cell$mean_response<0 ~  "#CD5A5A",
                           pos_neg_one_cell$mean_response>0 ~  "#56A7DA")) +
  facet_wrap(~cell_example, ncol=1) +
  theme(strip.text = element_text(colour = 'white', size=16)) +
  geom_label_repel(inherit.aes=F, hjust = 0, ##add labels for a supplementanry figure
            aes(label=English_Name, x=weighted_response, y=new_order), size=3,
            box.padding = unit(0.35, "lines"),
            point.padding = unit(0.3, "lines"), 
            min.segment.length = 0)

#
#adjust colour of facet labels
g <- ggplot_gtable(ggplot_build(indv_cells))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#B7A3B8","#80396B", "#630E74")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
ggdraw(g)
ggsave("figures/caterpillar_meta_3_cellsb.png", height=8, width=3.5)
ggsave("figures/caterpillar_meta_3_cellsb.pdf", height=8, width=3.5)
#ggsave("figures/caterpillar_meta_3_cellsb_labels.png", height=15, width=8)
#ggsave("figures/caterpillar_meta_3_cellsb_labels.pdf", height=15, width=8)

