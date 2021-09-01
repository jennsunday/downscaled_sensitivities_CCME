#goal: combine data for overall sensitivity, projecting uncertainty
#idea: take a weighted average of slopes, using metanalalysis approach.
#weight the individual effect sizes by the inverse of the effect size variance to account for
#the precision of each study

#librarieslibrary(nlme) 
library(tidyverse)
library(broom)

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_cal.csv")

sensitivity_by_study$treatment_var
names(sensitivity_by_study)


###############
#assign zone to species
###############
#all adults can be considered benthic except for pink salmon = mesopelagic
#propagules are sometimes benthic sometimes not, assign based on specific life stage used
unique(sensitivity_by_study$Life_stage)
sensitivity_by_study<-sensitivity_by_study%>%
  mutate(zone=case_when(Life_stage %in% c("larvae", "megalopa", "Larvae", "Brachiolaria larva") ~ "pelagic",
                        English_Name =="Pink Salmon" ~ "mesopelagic",
                        TRUE ~ "benthic")) #all others are benthic


###############
#reorder species
###############

#arrange order of species
order_desired<-c("Pink Salmon", "blue & black rockfish", 
                 "copper & quillback rockfish", "sablefish", "Razor Clam", "Red urchin", "Dungeness crab", 
                 "Ochre star", "Canopy-forming Kelp", "seagrass")

sensitivity_by_study<-sensitivity_by_study %>%
  mutate(category =  factor(English_Name, levels = order_desired)) %>%
  arrange(category)  

sensitivity_by_study$English_Name <- factor(sensitivity_by_study$English_Name, 
                                            levels = rev(order_desired))

####################

#calculate the meta-analyzed slope for each treatment type, species, zone, and major response category.
#calculate a slope for each species and response type, 
#weighted by the inverse of the variances, 
#with an overall variance as the inverse of the summed weights (1/sum(1/se2)) (Lipsey and Wilson).
#SD of mean (SE) = square root of variance, sqrt(variance) = SE, variance = SE^2.
#the standard error of the average effect size Xbar becomes SE(Xbar)=sqrt((∑Wi)^(−1)) = sqrt((sum(1/(se_estimate^2)))^(−1))



#calculate weighted mean by species x treatment x response type x zone occupied
sensitivity_by_group<-sensitivity_by_study %>%
  mutate(weight_slope=1/((std.error+0.001)^2)) %>%
  group_by(English_Name, treatment_var, response_type, zone) %>%
  mutate(variance=se_estimate^2, weight=1/(variance+0.00000001)) %>%
  dplyr::summarize(weighted_sensitivity=weighted.mean(mean_estimate, w=weight), 
                   weight_test=sum(weight*mean_estimate)/sum(weight),
                   n=n(), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_sensitivity-1.96*SE_wmean, hi_95=weighted_sensitivity+1.96*SE_wmean)

write_csv(sensitivity_by_group, "processed_data/sensitivity_by_group.csv")

#get mean projected change for each zone and model
environmental_mean_deltas<-read_csv("processed_data/table_delta_masked.csv") %>%
  mutate(modelzone=water_range) %>%
  mutate(variable=ifelse(variable=="temp", "temperature", variable)) 



#right_join so that each model and zone delta is joined by a sensitivity on the left
all_models_response_estimates<-right_join(sensitivity_by_group, environmental_mean_deltas, by = c("treatment_var" = "variable"))


all_models_response_estimates<-all_models_response_estimates %>%
  mutate(percentchange=weighted_sensitivity*mean_delta*100) %>% #calculate meta-analyzed sensitivity
  mutate(percentchange_lo_95=lo_95*mean_delta*100) %>% #calculate low 95CI
  mutate(percentchange_hi_95=hi_95*mean_delta*100)  #calculate high 95CI



# combine oxygen and CO2 responses for plotting standardized responses (slopes)
all_models_response_estimates<-all_models_response_estimates%>%
  mutate(treatment_var2=ifelse(
    treatment_var %in% c("pH", "CO2"), "CO2 and pH", treatment_var))

#plot all responses across different models and water zones
all_models_response_estimates %>%
  mutate(model=ifelse(model=="2km","1.5km", model)) %>%
  mutate(modelzone=ifelse(modelzone=="200m", "upper 200m average", modelzone)) %>%
  ggplot(aes(x=percentchange, y=English_Name, shape=response_type, col=treatment_var2)) +
  #coord_cartesian(xlim=c(-100,150))  +
  facet_grid(model~ modelzone) +
  geom_point() + theme_bw() +
  labs(y = "Species", x="Percent change in biological rate") +
  geom_errorbarh(aes(xmin=(percentchange_lo_95), 
                     xmax=(percentchange_hi_95)), height=0) +
  geom_vline(xintercept = c(-20, 20), linetype="dotted") +
  labs(col = "treatment variable", shape="response type")
ggsave("figures/meta_sensitivities_allmodels.png", width = 10, height = 8)
ggsave("figures/meta_sensitivities_allmodels.pdf", width = 10, height = 8)


# make summary figure that uses the most relavant water zone per response type for 12km CCS
domain_relavant_layer_12km<-all_models_response_estimates %>%
  filter(model=="12km") %>%
  filter(zone=="benthic" & modelzone=="bottom" |
         zone=="pelagic" & modelzone=="surface" |
         zone=="mesopelagic" & modelzone=="200m")

#plot responses for big domain and relevant layer
#plot all responses across different models and water zones
domain_relavant_layer_12km %>%
  ggplot(aes(x=percentchange, y=English_Name, shape=response_type, col=treatment_var2)) +
  coord_cartesian(xlim=c(-95,95)) +
  geom_point() + theme_bw() +
  labs(y = "Species", x="Percent change in biological rate", 
       col = "treatment variable", shape="response type") +
  geom_errorbarh(aes(xmin=(percentchange_lo_95), 
                     xmax=(percentchange_hi_95)), height=0) +
  geom_vline(xintercept = c(-20, 20), linetype="dotted") 
ggsave("figures/meta_sensitivities_relevant_12km.png", width = 7, height = 4)
ggsave("figures/meta_sensitivities_relevant_12km.pdf", width = 7, height = 4)

#Anchovy MI: -19.4%
#Shrimp MI: -29.7%
#make a dataframe to plot Evan's species' responses.
MI_index_response<-data.frame(English_Name=c("anchovy","shrimp"), 
                              percentchange=c(-19.4, -29.7),
                              response_type="growth")
MI_index_response%>%
  ggplot(aes(x=percentchange, y=English_Name, shape=response_type)) +
  coord_cartesian(xlim=c(-95,95)) +
  geom_point() + theme_bw() +
  labs(y = "Species", x="Percent change in biological rate") +
  geom_vline(xintercept = c(-20, 20), linetype="dotted") 
ggsave("figures/MI_sensitivities.png", width = 6, height = 1)
ggsave("figures/MI_sensitivities.pdf", width = 6, height = 1)

write_csv(bigdomain_relavant_layer, "processed_data/bigdomain_relavant_layer")
write_csv(all_models_response_estimates, "processed_data/all_models_response_estimates")

#for supplement, repeat for 1.5km model
# make summary figure that uses the most relavant water zone per response type for 12km CCS
relavant_layer_2km<-all_models_response_estimates %>%
  filter(model=="2km") %>%
  filter(zone=="benthic" & modelzone=="bottom" |
           zone=="pelagic" & modelzone=="surface" |
           zone=="mesopelagic" & modelzone=="200m")

#plot responses for big domain and relevant layer
#plot all responses across different models and water zones
relavant_layer_2km %>%
  ggplot(aes(x=percentchange, y=English_Name, shape=response_type, col=treatment_var2)) +
  coord_cartesian(xlim=c(-95,95)) +
  geom_point() + theme_bw() +
  labs(y = "Species", x="Percent change in biological rate", 
       col = "treatment variable", shape="response type") +
  geom_errorbarh(aes(xmin=(percentchange_lo_95), 
                     xmax=(percentchange_hi_95)), height=0) +
  geom_vline(xintercept = c(-20, 20), linetype="dotted") 
ggsave("figures/meta_sensitivities_relevant_2km.png", width = 7, height = 4)
ggsave("figures/meta_sensitivities_relevant_2km.pdf", width = 7, height = 4)
