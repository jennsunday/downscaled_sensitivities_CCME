#goal: make phase diagram of effects on each species

#librarieslibrary(nlme) 
#detach(package:plyr)  
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(cowplot)



sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_cal.csv")


summarized_sensitivity<-sensitivity_by_study %>%
  group_by(English_Name, treatment_var, danger_of_increase) %>%
  summarize(median_response=median(percentchange))

###for temperature
performance<-summarized_sensitivity %>%
  filter(treatment_var=="temperature")  %>%
  filter(danger_of_increase=="no") %>%
  mutate(performance=median_response)

demand<-summarized_sensitivity %>%
  filter(treatment_var=="temperature")  %>%
  filter(danger_of_increase=="possible") %>%
  mutate(demand=median_response)

phase<-merge(performance, demand, by="English_Name")

###for CO2
performance<-summarized_sensitivity %>%
  filter(treatment_var=="CO2")  %>%
  filter(danger_of_increase=="no") %>%
  mutate(performance=median_response)

demand<-summarized_sensitivity %>%
  filter(treatment_var=="CO2")  %>%
  filter(danger_of_increase=="possible") %>%
  mutate(demand=median_response)

phase_CO2<-merge(performance, demand, by="English_Name")

phase %>%
  ggplot(aes(x=performance, y=demand)) + 
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "orange")  + 
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0 , fill= "turquoise") + 
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "yellow") + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "green") + 
  geom_point() + geom_point(data=phase_CO2, shape=2)
  ggsave("figures/phase_diagram.pdf", width = 5, height = 5)

select(phase, English_Name, demand, performance)
       