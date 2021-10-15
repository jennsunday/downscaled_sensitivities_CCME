#goal: project uncertainty from anova-style data
#idea: resample from distribution of values with mean and standard error 
#equal to standard deviation of the mean

library(ggplot2)
library(tidyverse)
library(broom)

#read in data subset to relevant range
data_in_window<-read_csv("processed_data/data_in_window.csv")

################################
#simulate data for Anova-style experiments
#with N=sample size
#then fit model, iterate
################################

#subset to just anova-style data
anova_data<-data_in_window %>%
  filter(study_design %in% c("anova", "multi-level anova")) 

names(data_in_window)

#expand anova df by sample size of each row
anova_data_expanded<- anova_data[rep(seq.int(1,nrow(anova_data)), anova_data$sample_size),]


#simulate regression-style data from anova data in 5000 iterations, fit lms through them
lm_anova_resampled<-data.frame()
for(i in 1:1000){ #change back to 5000 eventually
  lme_rep<-anova_data_expanded %>%
    mutate(simulated_response=rnorm(rel_response, 
                                   mean=rel_response, 
                                   sd=SE_response_for_resampling)) %>% #add a column with a simulated sample for each row
    group_by(Author, Pub_Year, English_Name, Life_stage, Life_stage_category,
             Response_variable, response_type, unique_study, treatment_var, rate_or_biomass, response_type_2) %>% #group data to study level and keep info
    do(tidy(lm(simulated_response~treat_value, data=.))) %>%
    mutate(replicate=i) %>%
    ungroup()
    lm_anova_resampled<-bind_rows(lme_rep, lm_anova_resampled)} 
#now have slope and intercept of n models fit to anova-style data
#bunch of warnings - lm fit to 2 data points - that's ok.


#get a summary of simulated lm's from anova data
lm_by_group_summary<-lm_anova_resampled %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Life_stage_category,
           Response_variable, response_type, unique_study, treatment_var, rate_or_biomass, response_type_2, term) %>%
  mutate(mean_estimate= mean(estimate), se_estimate = sd(estimate)) %>% #take means across reps
  filter(replicate=="1") #subset to the first rep

# do regular lm's for regression-style data
regresion_data<-data_in_window %>%
  filter(study_design %in% c("regression")) 

lm_by_group_summary_regress<-regresion_data %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Life_stage_category, Response_variable, response_type, unique_study, 
           treatment_var, rate_or_biomass, response_type_2) %>%
  do(tidy(lm(rel_response~treat_value, data=.))) %>%
  ungroup() %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Life_stage_category, Response_variable, response_type, unique_study, 
           treatment_var, rate_or_biomass, response_type_2, term) %>%
  mutate(mean_estimate = estimate, se_estimate = std.error)


#bind anova-style model fit summaries with regression-style:
sensitivity_by_study<-rbind(lm_by_group_summary_regress, lm_by_group_summary)
write_csv(sensitivity_by_study, "processed_data/sensitivity_by_study.csv")


###############
#use relavant model zone to standardize deltas expected for each species/life history
###############
#assign model zone to species
#all adults can be considered benthic except for pink salmon = mesopelagic
#propagules are sometimes benthic sometimes not, assign based on specific life stage used

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study.csv") %>%
  filter(term=="treat_value") # just extract slope

unique(sensitivity_by_study$Life_stage) # have a look at life stages

#set zone of life stage of each study
sensitivity_by_study_zoned<-sensitivity_by_study %>%
  mutate(modelzone=case_when(Life_stage %in% c("larvae", "megalopa", "Larvae", "Brachiolaria larva", "embryo") ~ "surface",
                             English_Name =="Pink Salmon" ~ "200m",  #all others are benthic 
                             TRUE ~ "bottom")) %>%
  mutate(adult_zone=case_when(English_Name =="Pink Salmon" ~ "200m",  #all others are benthic 
                              TRUE ~ "bottom"))
write_csv(sensitivity_by_study_zoned, "processed_data/sensitivity_by_study_zoned.csv")


###################################
#plot mean salinity responses before dropping
###################################

#read in mean delta table, and set up for a left_join
environmental_mean_deltas<-read_csv("processed_data/table_delta_masked.csv") %>%
  mutate(modelzone=water_range) %>%
  mutate(variable=ifelse(variable=="temp", "temperature", variable)) 


#left_join so that each model and zone mean delta is joined by a sensitivity on the left
by_zone_response_estimates<-left_join(sensitivity_by_study, environmental_mean_deltas, 
                                         by = c("treatment_var" = "variable", "modelzone" = "modelzone")) %>%
                              filter(model=="2km")

sensitivity_by_study_zoned<-by_zone_response_estimates %>%
  mutate(percentchange=mean_estimate*mean_delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=(mean_estimate-1.96*se_estimate)*mean_delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=(mean_estimate+1.96*se_estimate)*mean_delta*100) %>% #calculate high 95CI
  mutate(percentchangeSE=abs(se_estimate)*mean_delta*100)

sensitivity_by_study %>%
  filter(treatment_var=="salinity") %>%
  group_by(English_Name) %>%
  mutate(mean_response=mean(mean_estimate)) %>%
  ungroup %>%
  arrange(mean_response, mean_estimate) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  ggplot(aes(x=mean_estimate, y=compound_order, shape=response_type, col=English_Name)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin=mean_estimate - 1.96*se_estimate, 
                                    xmax=mean_estimate + 1.96*se_estimate), height=0) +
  theme_classic() +
  geom_vline(xintercept = 0, col="grey") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Slope of response to changes in salinity", 
       col = "species", shape="response type")
ggsave("figures/salinity_responses.png", width=6, height=5)
###################################


