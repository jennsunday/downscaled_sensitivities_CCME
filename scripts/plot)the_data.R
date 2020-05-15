#goal: plot the data

#librarieslibrary(nlme) 
detach(package:plyr)  
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(cowplot)

setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_cal.csv")


#arrange order of species
unique(sensitivity_by_study$English_Name)
order_desired<-c("Pink Salmon", "blue & black rockfish", 
                 "copper & quillback rockfish", "sablefish", "Razor Clam", "Red urchin", "Dungeness crab", 
                 "Ochre star", "Canopy-forming Kelp", "seagrass")

sensitivity_by_study<-sensitivity_by_study %>%
  mutate(category =  factor(English_Name, levels = order_desired)) %>%
  arrange(category)  

sensitivity_by_study$English_Name <- factor(sensitivity_by_study$English_Name, 
                                            levels = rev(order_desired))


#calculate meta-analyzed affect with SD
meta<-sensitivity_by_study %>%
  filter(treatment_var!="salinity") %>%
  filter(treatment_var!="oxygen") %>%
  group_by(English_Name, treatment_var) %>%
  summarise(slope=(sum(percentchange/(SEpercentchange)^2))/sum(1/(SEpercentchange)^2),
            SE_slope=(1/sum(1/(SEpercentchange)^2)))


meta %>%
  filter(treatment_var!="salinity") %>%
  filter(treatment_var!="oxygen") %>%
  ggplot(aes(x=abs(slope), y=English_Name)) + 
  facet_wrap(~treatment_var)  +
  geom_point() + theme_bw() +
  coord_cartesian(xlim=c(-52,52))  +
  labs(y = "Species", x="Change in performance") +
  geom_errorbarh(aes(xmin=(slope-SE_slope), 
                     xmax=(slope+SE_slope)), height=0) +
  geom_vline(xintercept = 0, linetype="dotted") 
