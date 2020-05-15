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


print(levels(sensitivity_by_study$English_Name))

#plot physiological sensitivities all together
sensitivity_by_study %>%
  #filter(treatment_var!="salinity") %>%
  #filter(treatment_var!="oxygen") %>%
  #filter(rate_or_biomass=="rate") %>%
  ggplot(aes(x=(percentchange), y=English_Name, shape=response_type, col=response_type)) + 
  facet_wrap(~treatment_var)  +
  geom_point() + theme_bw() +
  coord_cartesian(xlim=c(-52,52))  +
  labs(y = "Species", x="Percent change in performance") +
  geom_errorbarh(aes(xmin=(percentchange-SEpercentchange), 
                     xmax=(percentchange+SEpercentchange)), height=0) +
  geom_vline(xintercept = 0, linetype="dotted") 
#geom_point(data=filter(sensitivity_results, 
#                       treatment_var!="salinity"), 
#           aes(x=sensitivity, y=English_Name), 
#           inherit.aes=F, size=0.8, shape=19, fill=1)
ggsave("figures/sensitivity_by_study.pdf", width = 7, height = 6)


# make a category for population or individual-level responses
sensitivity_by_study<-sensitivity_by_study %>%
  mutate(level_of_response=ifelse(response_type=="survival", "pop", "ind"))

#plot population-related sensitivities seperately from rate-related ones
sensitivity_by_study %>%
  filter(treatment_var!="salinity") %>%
  filter(treatment_var!="oxygen") %>%
  ggplot(aes(x=(percentchange), y=English_Name, shape=response_type)) + 
  facet_wrap(level_of_response~treatment_var)  +
  geom_point() + theme_bw() +
  coord_cartesian(xlim=c(-60,60))  +
  labs(y = "Species", x="Percent change in biological rate") +
  geom_errorbarh(aes(xmin=(percentchange-SEpercentchange), 
                     xmax=(percentchange+SEpercentchange)), height=0) +
  geom_vline(xintercept = 0, linetype="dotted") 
#geom_point(data=filter(sensitivity_results, 
#                       treatment_var!="salinity"), 
#           aes(x=sensitivity, y=English_Name), 
#           inherit.aes=F, size=0.8, shape=19, fill=1)
ggsave("figures/each_sensitivity_survival_separated.pdf", width = 7, height = 6)

