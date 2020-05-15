# load libraries ----------------------------------------------------------
library(nlme) #for mixed models
library(MuMIn) # to do model averaging
library(plyr)
library(mgcv)

#### Step 2: read in the data
setwd('/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/')
data<-read.csv(file="raw_data/direct_responses_data_jan2.csv", header=T)

#make a unique identifier for each author, pub_year, and response_variable combination
data$study<-as.factor(paste(data$Author, data$Pub_Year, data$English_Name, data$Life_stage, data$Response_variable, data$treatment_var))

# make upper and lower SE, convert 95CI to upper and lower SE for some data
data$lowerSE<-ifelse(is.na(data$LCI_response), 
                     data$response-data$SE_response, 
                     data$response-((data$response-data$LCI_response)/1.96))
data$upperSE<-ifelse(is.na(data$UCI_response), 
                     data$response+data$SE_response, 
                     data$response+((data$UCI_response-data$response)/1.96))
head(data[,c(26, 45, 46)]) #check some data

#recenter negative response values to 0, carry upper error distribution
#check first - these are somewhat nonsensical as negatives:
#photosyntehsis, growth rate, activity

filter(data, response<0)

#also limit negative values to 0
data<-data %>%
  mutate(rec_lowerSE=ifelse(lowerSE<0.01, 0.01, lowerSE), #limit lower SE to 0.01
         rec_upperSE=ifelse(response<0, max(upperSE-response, 0.01), upperSE), # limit upper SE to 0.01
         recentered_response=(ifelse(response<0.01, 0.01, response))) # limit response to 0.01

#calculate something within each study
data<-ddply(data, "study", mutate, max_response=max(recentered_response),   #add columns of max response and log response relative to max
            log_rel_response=log(recentered_response/max_response), #log of relative response to max
            log_rel_upperSE=log(rec_upperSE/max_response),
            log_rel_lowerSE=log(rec_lowerSE/max_response),
            log_SE_response_for_resampling=log_rel_upperSE-log_rel_response,
            rel_response=(recentered_response/max_response), #log of relative response to max
            rel_upperSE=(rec_upperSE/max_response),
            rel_lowerSE=(rec_lowerSE/max_response),
            SE_response_for_resampling=rel_upperSE-rel_response)


data$log_SE_response_for_resampling<-ifelse(is.na(data$log_SE_response_for_resampling), 0, data$log_SE_response_for_resampling) #change NAs to 0
data$SE_response_for_resampling<-ifelse(is.na(data$SE_response_for_resampling), 0, data$SE_response_for_resampling) #change NAs to 0


#add a column with the important treatment value
for(i in 1:length(data$treatment_var)){
  data$treat_value[i]<-data[i,which(names(data)==(data$treatment_var[i]))]
  }

write.csv(data, "processed_data/Schmidt_biotic_responses_data_180118.csv")

