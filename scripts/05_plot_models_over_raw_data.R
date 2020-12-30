#goal: plot model fits over raw data
#this is as an extension of script 04, need to have "lm_anova_resampled" object in environment

#do this by:
#expand an empty df so that for each study there are n=10 rows between minimum and maximum treatment values
#so df has study, metrics of study, and 10 treatment_values between min and max.
#for anova-style data:
#for each row, predict all of the y-values from the model iterations, 
#and summarize the mean, ucl and lcl into new columns
#for regression-style data, use predict with SE


library(nlme) 
library(tidyverse)
library(broom)

########################################

#read in data subset to relevant range
data_in_window<-read_csv("processed_data/data_in_window.csv")


#make a dataframe with metadata per unique study

#first, make new columns for max and min treatment variables
data_in_window_2<-data_in_window %>%
  group_by(unique_study) %>%
  mutate(max_treatment_val=max(treat_value),
         min_treatment_val=min(treat_value))

#now contract so each row is a unique study
one_entry_per_study<-data_in_window_2 %>%
  group_by(unique_study) %>%
  slice(1)

#expand into a sequence of 10 x-values per study
seq_x_per_study<-one_entry_per_study %>%
  group_by(unique_study) %>%
  do(data.frame(seq_x_val=seq(.$min_treatment_val,.$max_treatment_val, length.out=10)))


#join with new_data so that new df has study, metrics of study
seq_x_per_study_2<-left_join(seq_x_per_study, one_entry_per_study, by="unique_study")

#for anova-style data:
#for each row, predict all of the y-values from the model iterations, 
#and summarize the mean, ucl and lcl into new columns
#using iterated models from last script

#initialize columns to store outouts
seq_x_per_study_2$mean_pred<-as.numeric(NA)
seq_x_per_study_2$upp_pred<-as.numeric(NA)
seq_x_per_study_2$low_pred<-as.numeric(NA)
for(i in seq_x_per_study_2$unique_study){ # i is every unique study id repeated for each of 10 input x values
  for(j in 1:10){ #but do this for each input x value
    test<-filter(lm_anova_resampled, unique_study==i, term=="treat_value")$estimate*
      filter(seq_x_per_study_2, unique_study==i)[j,]$seq_x_val+
      filter(lm_anova_resampled, unique_study==i, term=="(Intercept)")$estimate
    seq_x_per_study_2[seq_x_per_study_2$unique_study==i,]$mean_pred[j]<-mean(test)
    seq_x_per_study_2[seq_x_per_study_2$unique_study==i,]$upp_pred[j]<-as.numeric(quantile(test, 0.975))
    seq_x_per_study_2[seq_x_per_study_2$unique_study==i,]$low_pred[j]<-as.numeric(quantile(test, 0.025))
}
}
#ok, predictions made only for anova-style data so far.

#make a vector of regression study i.d.s
reg_studies<-filter(seq_x_per_study_2, study_design == c("regression"))$unique_study

#for regression-style data, use predict with SE
#enter results right into placeholders in "seq_x_per_study_2'
for(i in reg_studies){
  mod<-data_in_window %>%
    filter(study_design %in% c("regression"))  %>%
    filter(unique_study==i) %>%
    lm(rel_response~treat_value, data=.)
  newdata_i<-seq_x_per_study_2 %>%
    filter(unique_study==i) %>%
    mutate(treat_value=seq_x_val)
  mod_pred<-predict.lm(mod, newdata=newdata_i, se.fit=TRUE)
  seq_x_per_study_2[seq_x_per_study_2$unique_study==i,]$mean_pred[1:10]<-mod_pred$fit
  seq_x_per_study_2[seq_x_per_study_2$unique_study==i,]$upp_pred[1:10]<-mod_pred$fit+mod_pred$se.fit
  seq_x_per_study_2[seq_x_per_study_2$unique_study==i,]$low_pred[1:10]<-mod_pred$fit-mod_pred$se.fit
}

#group study design into 2 categories for plotting
seq_x_per_study_2<-seq_x_per_study_2 %>%
mutate(study_design=ifelse(study_design=="regression", "regression", "anova"))
      
write.csv(seq_x_per_study_2, "processed_data/data_to_predict.csv")

#plot
data_to_predict<-read_csv("processed_data/data_to_predict.csv")
data_in_window<-read_csv("processed_data/data_in_window.csv")

data_in_window %>%
  ggplot(aes(x=treat_value, y=rel_response, col=response_type)) + geom_point() +
  facet_grid(English_Name~treatment_var, scales = "free_x") +
  coord_cartesian(ylim=c(-0.1, 1.3)) +
  geom_line(data=data_to_predict,
            aes(x=seq_x_val, y=mean_pred, group=(as.factor(unique_study)), lty=study_design)) +
  geom_errorbar(aes(ymax=rel_upperSE, ymin=rel_lowerSE)) +
  geom_ribbon(data=data_to_predict, 
              aes(x=seq_x_val, ymin=low_pred, ymax=upp_pred, group=as.factor(unique_study), 
                  fill=response_type), linetype=0, alpha = 0.5) +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  labs(x="treatment value", y="relative response",
       lty = "study design", fill="response type", col="response type")
ggsave("figures/model_fits_over_raw_data.pdf", height=8, width=8)
ggsave("figures/model_fits_over_raw_data.png", height=8, width=8)


