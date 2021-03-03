#make figures of deltas

library(tidyverse)
library(broom)
library(PNWColors)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(biscale)
library(cowplot)

all_deltas_2km<-read_csv("processed_data/all_deltas_2km.csv") 
all_deltas_12km<-read_csv("processed_data/all_deltas_12km.csv") 

temp<-all_deltas_2km %>% 
  filter(modelzone=="200m") %>%
  filter(treatment_var=="temp") %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = delta)) +
  #geom_tile(data=coastline_mask, fill=grey(0.4)) +
  scale_fill_gradient(low="white", high="#d7191c") +
  #scale_fill_gradient(low="blue", high="red") +
  theme_classic() + 
  labs(fill="temperature, °C") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        strip.background=element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.position = c(0.8, 0.7))

oxy<-all_deltas_2km %>% 
  filter(modelzone=="200m") %>%
  filter(treatment_var=="oxy") %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = delta)) +
  #geom_tile(data=coastline_mask, fill=grey(0.4)) +
  scale_fill_gradient(high="#d7191c", low="white") +
  #scale_fill_gradient(low="blue", high="red") +
  theme_classic() + 
  labs(fill="oxygen, mg/ml") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        strip.background=element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.position = c(0.8, 0.7))


pH<-all_deltas_2km %>% 
  filter(modelzone=="200m") %>%
  filter(treatment_var=="pH") %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = delta)) +
  #geom_tile(data=coastline_mask, fill=grey(0.4)) +
  scale_fill_gradient(low="#d7191c", high="white") +
  #scale_fill_gradient(low="blue", high="red")+
  theme_classic() + 
  labs(fill="pH") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        strip.background=element_blank())+
  guides(color=guide_colorbar(barwidth = unit(.1,"in"),
                              barheight = unit(4,"in"),
                              ticks.colour = "black",
                              frame.colour = "black",
                              title.position = "top"))
        #legend.key.size = unit(0.2, "cm"),
        #legend.position = c(0.8, 0.8))

CO2<-all_deltas_2km %>% 
  filter(modelzone=="200m") %>%
  filter(treatment_var=="CO2") %>%
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = delta)) +
  #geom_tile(data=coastline_mask, fill=grey(0.4)) +
  scale_fill_gradient(low="white", high="#d7191c") +
  #scale_fill_gradient(low="blue", high="red") +
  theme_classic() + 
  labs(fill="CO2, ppm") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        strip.background=element_blank()) +
  guides(color=guide_colorbar(barwidth = unit(.1,"in"),
                              barheight = unit(4,"in"),
                              ticks.colour = "black",
                              frame.colour = "black",
                              title.position = "top"))

ggdraw() +
  draw_plot(temp,  0, 0, .25, 1) +
  draw_plot(oxy,  .25, 0, .25, 1)+
  draw_plot(pH,.5, 0, .25, 1) +
  draw_plot(CO2,  .75, 0, .25, 1) 



depth_2km %>% 
  ggplot(aes(y = lat, x = long)) + geom_tile(aes(fill = depth)) +
  geom_tile(data=coastline_mask, fill=grey(0.4)) +
  #scale_fill_gradient(low="yellow", high="blue") +
  scale_fill_gradient(low="#f2f0f7", high="#54278f") +
  theme_classic()
