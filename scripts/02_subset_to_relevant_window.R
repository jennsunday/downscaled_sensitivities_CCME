# load libraries ----------------------------------------------------------
library(nlme) #for mixed models
library(MuMIn) # to do model averaging
library(mgcv)
library(ggplot2)
library(broom)
library(tidyverse)
library(dplyr)

#read in the procesed data
data<-read.csv("processed_data/Schmidt_biotic_responses_data_180118.csv")
filter(data, treatment_var=="oxygen")
data$treatment_var
#remove very very high CO2 values
data <- data %>%
  filter(treat_value<2000)

#make a column of species names
data$genus_species<-paste(data$Genus, data$Species, sep="_")


#plot all the data - relative response
data %>% 
 ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type, linetype=study_design)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() +
  stat_smooth(data=subset(data, data$study_design=="regression"), span=4) +
  geom_line(data=subset(data, data$study_design %in% c("anova", "multi-level anova"))) +
  xlab("treatment value") + ylab("relative response")
ggsave("figures/data_connected.pdf", width = 8, height = 8)

#set range of relevant values for each stressor
range_temp<-c(7,16)
range_salinity<-c(29,33.5)
range_O2<-c(0.5,6)
range_CO2<-c(350,1100)


#get these into the main data frame
data<-rbind(data %>%
  filter(treatment_var=="CO2") %>%
  mutate(upper_window=range_CO2[2]) %>%
  mutate(lower_window=range_CO2[1]),
            data %>%
  filter(treatment_var=="oxygen") %>%
  mutate(upper_window=range_O2[2]) %>%
  mutate(lower_window=range_O2[1]) ,
            data %>%
  filter(treatment_var=="salinity") %>%
  mutate(upper_window=range_salinity[2]) %>%
  mutate(lower_window=range_salinity[1]) ,
            data %>%
  filter(treatment_var=="temperature") %>%
  mutate(upper_window=range_temp[2]) %>%
  mutate(lower_window=range_temp[1])
            )



#for every study, subset to all values inside window plus frist value outside window
data_in_window<-data %>%
  group_by(study) %>%
  filter(treat_value>=lower_window & treat_value<=upper_window |
           treat_value==max(treat_value[which(treat_value < lower_window)]) | 
           treat_value==min(treat_value[which(treat_value > upper_window)])) %>%
  filter(length(log_rel_response)>1) %>% #Remove studies where NO data were in window
  mutate(weighting=length(which(treat_value>=lower_window & treat_value<=upper_window))) %>% #make weighting value
  ungroup()

#make a column that flags each unique study
data_in_window<-data_in_window %>%
  mutate(unique_study=as.numeric(as.factor(paste(Author, Pub_Year, English_Name, Life_stage, 
                                                 Response_variable, response_type, treatment_var, rate_or_biomass, response_type_2))))

#lots of warnings - perhaps for cases when there is no treat_value outside of window?
write_csv(data_in_window, "processed_data/data_in_window.csv")


#plot all the data - relative response
data_in_window %>% 
  ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() + 
  stat_smooth(data=subset(data_in_window, data$study_design=="regression"), span=4) +
  geom_line(data=subset(data_in_window, data$study_design %in% c("anova", "multi-level anova"))) 

#
#
#
#
#
