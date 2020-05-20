# load libraries ----------------------------------------------------------
library(tidyverse)


#### Step 2: read in the data
data<-read_csv(file="raw_data/direct_responses_data_jan2.csv")

#make a unique identifier for each author, pub_year, and response_variable combination
data$study<-as.factor(paste(data$Author, data$Pub_Year, data$English_Name, 
                            data$Life_stage, data$Response_variable, data$treatment_var))
#check units of oxygen entered
View(data %>%
  filter(treatment_var=="oxygen")) 
#checked website, seems fine

# make upper and lower SE, convert 95CI to upper and lower SE for some data
data$lowerSE<-ifelse(is.na(data$LCI_response), 
                     data$response-data$SE_response, 
                     data$response-((data$response-data$LCI_response)/1.96))
data$upperSE<-ifelse(is.na(data$UCI_response), 
                     data$response+data$SE_response, 
                     data$response+((data$UCI_response-data$response)/1.96))


#check first zeros - these are somewhat nonsensical as negatives:
#photosyntehsis, growth rate, activity
filter(data, response<0)

#recenter negative response values to 0, carry upper error distribution
#also limit negative values to 0
#data<-data %>%
#  mutate(rec_lowerSE=ifelse(lowerSE<0.01, 0.01, lowerSE), #limit lower SE to min 0.01
#         rec_upperSE=ifelse(response<0, max(upperSE-response, 0.01), upperSE), # limit upper SE to min 0.01
#         recentered_response=(ifelse(response<0.01, 0.01, response))) # limit response to 0.01
#maybe I only did this because I was going to take logs???
#try not doing it
#calculate variables within each study
data<-data %>% 
  group_by(study) %>%
  mutate(max_response=max(response),   #add columns of max response and log response relative to max
         rel_response=(response/max_response), #log of relative response to max
         rel_upperSE=(upperSE/max_response),
         rel_lowerSE=(lowerSE/max_response),
         SE_response_for_resampling=rel_upperSE-rel_response) %>%
  ungroup()
plot(data$SE_response_for_resampling)


#calculate variables within each study
#potentially delete this:
#data<-data %>% 
#  group_by(study) %>%
#  mutate(max_response=max(recentered_response),   #add columns of max response and log response relative to max
#  rel_response=(recentered_response/max_response), #log of relative response to max
#  rel_upperSE=(rec_upperSE/max_response),
#  rel_lowerSE=(rec_lowerSE/max_response),
#  SE_response_for_resampling=rel_upperSE-rel_response) %>%
#  ungroup()


data$SE_response_for_resampling<-ifelse(is.na(
  data$SE_response_for_resampling), 0, 
  data$SE_response_for_resampling) #change NAs to 0


#change treatment_var from CO2 to pH if only pH data are available
data<-data %>%
  mutate(treatment_var=ifelse(!is.na(data$pH) & is.na(data$CO2), 
                                   "pH", treatment_var))

#add a column with the important treatment value
data<-data %>%
  mutate(treat_value=case_when(data$treatment_var=="CO2"~CO2,
                             data$treatment_var=="oxygen"~oxygen,
                             data$treatment_var=="salinity"~salinity,
                             data$treatment_var=="temperature"~temperature,
                             data$treatment_var=="pH"~pH,
                             TRUE ~ -99))

data$treat_value

write.csv(data, "processed_data/200519_Schmidt_biotic_responses_data.csv")

