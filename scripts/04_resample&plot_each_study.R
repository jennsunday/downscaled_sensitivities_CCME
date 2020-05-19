#goal: project uncertainty from anova-style data
#idea: resample from distribution of values with mean and standard error 
#equal to standard deviation of the mean

#librarieslibrary(nlme) 
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)


#read in data subset to relevant range
data_in_window<-read_csv("processed_data/data_in_window.csv")

################################
#simalate data for Anova-style experiments
#with N=sample size
#then fit model, iterate
################################

#subset to just anova-style data
anova_data<-data_in_window %>%
  filter(study_design %in% c("anova", "multi-level anova")) 

unique(data_in_window$unique_study)
unique(regresion_data$unique_study)

#expand anova df by sample size of each row
anova_data_expanded<- anova_data[rep(seq.int(1,nrow(anova_data)), anova_data$sample_size),]

#simulate regression-style data from anova data in 5000 iterations, fit lms through them
lm_anova_resampled<-data.frame()
for(i in 1:100){ #change back to 5000 eventually
  lme_rep<-anova_data_expanded %>%
    mutate(simulted_response=rnorm(rel_response, 
                                   mean=rel_response, 
                                   sd=SE_response_for_resampling)) %>% #add a column with a simulated sample for each row
    group_by(Author, Pub_Year, English_Name, Life_stage, 
             Response_variable, response_type, unique_study, treatment_var, rate_or_biomass, response_type_2) %>% #group data to study level and keep info
    do(tidy(lm(simulted_response~treat_value, data=.))) %>%
    mutate(replicate=i) %>%
    ungroup()
    lm_anova_resampled<-bind_rows(lme_rep, lm_anova_resampled)} 
#naw have slope and intercept of n models fit to anova-style data



#get a summary of simluated lm's from anova data
lm_by_group_summary<-lm_anova_resampled %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, 
           Response_variable, response_type, unique_study, treatment_var, rate_or_biomass, response_type_2, term) %>%
  mutate(mean_estimate= mean(estimate), se_estimate = sd(estimate)) %>% #take means across reps
  filter(replicate=="1") #subset to the first rep

# do regular lm's for regression-style data
regresion_data<-data_in_window %>%
  filter(study_design %in% c("regression")) 

lm_by_group_summary_regress<-regresion_data %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Response_variable, response_type, study, 
           treatment_var, rate_or_biomass, response_type_2) %>%
  do(tidy(lm(rel_response~treat_value, data=.))) %>%
  ungroup() %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Response_variable, response_type, study, 
           treatment_var, rate_or_biomass, response_type_2, term) %>%
  mutate(mean_estimate = estimate, se_estimate = std.error)

#bind anova-style model fit summaries with regression-style:
sensitivity_by_study<-rbind(lm_by_group_summary_regress, lm_by_group_summary)
write_csv(sensitivity_by_study, "processed_data/sensitivity_by_study.csv")




########################################
#convert slope to response using mean deltas from downscaled model
#read in data
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study.csv")
table_delta_masked<-read_csv("processed_data/table_delta_masked.csv")

sensitivity_by_study<-sensitivity_by_study %>%
  filter(term=="treat_value")

deltas_used<-table_delta_masked %>%
  filter(model=="12km", water_range=="200m")

#convert slopes to sensitivity by normalizing variation to the range expected


sensitivity_by_study$change_in_2100<-case_when(sensitivity_by_study$treatment_var=="CO2" ~ filter(deltas_used, variable=="CO2")$mean_delta,
                                               sensitivity_by_study$treatment_var=="temperature" ~ filter(deltas_used, variable=="temp")$mean_delta,
                                               sensitivity_by_study$treatment_var=="oxygen" ~ filter(deltas_used, variable=="oxygen")$mean_delta,
                                               sensitivity_by_study$treatment_var=="salinity" ~ -3,
                                               TRUE ~ -99)

#sensitivity_by_study$change_in_2100<-case_when(sensitivity_by_study$treatment_var=="CO2" ~ 726,
                          #sensitivity_by_study$treatment_var=="temperature" ~ 2.32,
                          #sensitivity_by_study$treatment_var=="oxygen" ~ -0.58,
                          #sensitivity_by_study$treatment_var=="salinity" ~ -3,
                          #TRUE ~ -99)

sensitivity_by_study<-sensitivity_by_study %>%
  mutate(percentchange=mean_estimate*change_in_2100*100) %>% #calculate sensitivity
  mutate(SEpercentchange=se_estimate*change_in_2100*100) #calculate SE of sensitivity

write_csv(sensitivity_by_study, "processed_data/sensitivity_by_study_cal.csv")


##################################

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_cal.csv")



sensitivity_by_study %>%
  ggplot(aes(x=percentchange, y=English_Name, color=response_type)) + 
           facet_wrap(~treatment_var, scales="free_x") + 
           geom_point() + theme_bw() +
           labs(y = "Species") +
           geom_errorbarh(aes(xmin=percentchange-SEpercentchange, xmax=percentchange+SEpercentchange), height=0) +
           geom_vline(xintercept = 0, linetype="dotted")
ggsave("figures/sensitivity_raw.pdf", width = 6, height = 6)
