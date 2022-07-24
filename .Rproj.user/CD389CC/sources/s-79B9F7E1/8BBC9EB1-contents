# load libraries ----------------------------------------------------------
library(broom)
library(tidyverse)
library(reshape2)

#read in the procesed data
data<-read.csv("processed_data/201230_Schmidt_biotic_responses_data.csv")

#remove very very high CO2 values
data <- data %>%
  filter(treat_value<2000)

#make a column of species names and a unique study identifier
data<-data %>%
  mutate(genus_species=paste(data$Genus, data$Species, sep="_")) %>%
  mutate(unique_study=as.numeric(as.factor(paste(Author, Pub_Year, English_Name, Life_stage, 
                                                 Response_variable, response_type, treatment_var, rate_or_biomass, response_type_2, study_design))))


#plot all the data - relative response
data %>% 
 ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type, linetype=study_design)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() +
  xlab("treatment value") + ylab("relative response")

#set range of relevant values for each stressor
relevant_window_cast<-read_csv("processed_data/relevant_window_cast.csv")


#get these into the main data frame
data2<-left_join(data, relevant_window_cast, by="treatment_var")  %>%
  mutate(upper_window=max, lower_window=min)%>%
  select(-max, -min)


#for every study, subset to all values inside window plus first value outside window
data_in_window<-data2 %>%
  group_by(unique_study) %>%
  filter(treat_value>=lower_window & treat_value<=upper_window |
           treat_value==max(treat_value[which(treat_value < lower_window)]) | 
           treat_value==min(treat_value[which(treat_value > upper_window)])) %>%
  filter(length(rel_response)>1) %>% #Remove studies where insufficient data were in window
  mutate(weighting=length(which(treat_value>=lower_window & treat_value<=upper_window))) %>% #make weighting value based on how many obs within window
  ungroup()
#bunch of warnings if there were no data outside of window


#make a *new* column that flags each unique study
data_in_window<-data_in_window %>%
  mutate(unique_study=as.numeric(as.factor(paste(Author, Pub_Year, English_Name, Life_stage, 
                                                 Response_variable, response_type, treatment_var, rate_or_biomass, response_type_2, study_design))))

write_csv(data_in_window, "processed_data/data_in_window.csv")

#quick plot all the data - relative responses
data_in_window %>% 
  ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() 

#
#
#
#
#
