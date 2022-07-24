# load libraries ----------------------------------------------------------
library(tidyverse)


#### Step 2: read in the data
data<-read_csv(file="raw_data/direct_responses_data.csv")
#name_replace_order<-read_csv("raw_data/name_replace_order.csv")

#make a unique identifier for each author, pub_year, and response_variable combination
data$study<-as.factor(paste(data$Author, data$Pub_Year, data$English_Name, 
                            data$Life_stage, data$Response_variable, data$treatment_var))

# make upper and lower SE, convert 95CI to upper and lower SE for some data
data$lowerSE<-ifelse(is.na(data$LCI_response), 
                     data$response-data$SE_response, 
                     data$response-((data$response-data$LCI_response)/1.96))
data$upperSE<-ifelse(is.na(data$UCI_response), 
                     data$response+data$SE_response, 
                     data$response+((data$UCI_response-data$response)/1.96))


#standardize all response data to a percentage of maximum response
data<-data %>% 
  group_by(study) %>%
  mutate(max_response=max(response),   #add columns of max response and log response relative to max
         rel_response=(response/max_response), #relative response to max
         rel_upperSE=(upperSE/max_response),
         rel_lowerSE=(lowerSE/max_response),
         SE_response_for_resampling=rel_upperSE-rel_response) %>%
  ungroup()
plot(data$SE_response_for_resampling)


#remove rows from anova or multi-level anova studies without SE estimates
#any anova studies with no SE?
data %>%
  filter(study_design %in% c("anova", "multi-level anova") & is.na(rel_upperSE))
# yes, 4; remove these.

data<-data %>%
  filter(study_design=="regression" | !is.na(rel_upperSE))
#4 rows from 2 studies removed
dim(data)

#check which are below 0 and if this is an issue
filter(data, rel_response<0)

#change NAs to 0
#data$SE_response_for_resampling<-ifelse(is.na(
#  data$SE_response_for_resampling), 0, 
#  data$SE_response_for_resampling) 


#change treatment_var from CO2 to pH if only pH data are available
data<-data %>%
  mutate(treatment_var=ifelse(!is.na(data$pH) & is.na(data$CO2) & treatment_var=="CO2", 
                                   "pH", treatment_var))


#add a column with the important treatment value
data<-data %>%
  mutate(treat_value=case_when(data$treatment_var=="CO2"~CO2,
                             data$treatment_var=="oxygen"~oxygen,
                             data$treatment_var=="salinity"~salinity,
                             data$treatment_var=="temperature"~temperature,
                             data$treatment_var=="pH"~pH,
                             TRUE ~ -99))

filter(data,treat_value==(-99))

#get species order and correct common name into the dataframe
#data<-left_join(data, name_replace_order, by = "English_Name")

write.csv(data, "processed_data/201230_Schmidt_biotic_responses_data.csv")

#assess coverage
coverage<-data %>%
  filter(treatment_var!="salinity") %>% #without salinity
  group_by(English_Name, treatment_var, response_type) %>%
  summarize(mean(response)) %>%
  ungroup() 

coverage<-coverage %>% count(English_Name) %>%
  mutate(coverage=n/15) # 5 response types by 3 treatment types
mean(coverage$coverage)

