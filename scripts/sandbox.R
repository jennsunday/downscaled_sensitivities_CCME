
#plot model fits over input data
#for anova data, draw sample, run and augment model
lm_anova_resampled_pred<-anova_data_expanded %>%
  mutate(simulted_response=rnorm(rel_response, mean=rel_response, sd=SE_response_for_resampling)) %>% #add a column with a simulated sample for each row
  group_by(Author, Pub_Year, English_Name, Life_stage, Response_variable, response_type, study, treatment_var) %>%
  do(augment(lm(simulted_response~treat_value, data=.)))
for(i in 1:10){
  lm_anova_rep_pred<-anova_data_expanded %>%
    mutate(simulted_response=rnorm(rel_response, mean=rel_response, sd=SE_response_for_resampling)) %>% #add a column with a simulated sample for each row
    group_by(Author, Pub_Year, English_Name, Life_stage, Response_variable, response_type, study, treatment_var) %>%
    do(augment(lm(simulted_response~treat_value, data=.))) %>%
    mutate(replicate=i)
  lm_anova_resampled_pred<-rbind(lm_anova_rep_pred, lm_anova_resampled_pred)}

#
#
#
#
#
#
#
#summarize augmented anova data
lm_anova_pred_sum<-lm_anova_resampled_pred %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Response_variable, 
           response_type, study, treatment_var, treat_value) %>%
  summarize(sd_pred = sd(.fitted), mean_pred=mean(.fitted))
#goal: project uncertainty from anova-style data
#idea: resample N=sample_size values from a distribution of with mean and standard error 
#equal to standard deviation of the mean from study

#for regression data, run and augment model
lm_by_group_summary_regress<-regresion_data %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Response_variable, response_type, study, treatment_var) %>%
  do(augment(lm(rel_response~treat_value, data=.)))

View(data)
#plot all the data
data_in_window %>%
  ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() +
  coord_cartesian(ylim = c(-0.1, 1.1)) + labs(x = "Treatment Value", y="Relative response") +
  geom_line(data=lm_anova_pred_sum, aes(x=treat_value, y=mean_pred, color=response_type)) +
  geom_line(data=lm_by_group_summary_regress, aes(x=treat_value, y=.fitted, color=response_type)) +
  geom_errorbar(data=anova_data, aes(ymin=rel_lowerSE, ymax=rel_upperSE), width=0) +
  #geom_line(data=lm_anova_pred_sum, aes(x=treat_value, y=lower_pred)) +
  #geom_line(data=lm_anova_pred_sum, aes(x=treat_value, y=upper_pred)) +
  geom_ribbon(data=lm_by_group_summary_regress, 
              aes(x=treat_value, ymax=.fitted+.se.fit, ymin=.fitted-.se.fit)) +
  geom_ribbon(data=lm_anova_pred_sum, 
              aes(x=treat_value, ymax=mean_pred+sd_pred, ymin=mean_pred-sd_pred))

ggsave("figures/all_fitted_slopes_over_data.pdf", width = 6, height = 7)





