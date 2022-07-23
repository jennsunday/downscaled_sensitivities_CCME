#goal: plot model fits over raw data
#this is an extension of script 05, need to have "lm_anova_resampled" object in environment

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

#first, make new columns for max and min treatment variables so predictions don't exceed treatment space
data_in_window_2<-data_in_window %>%
  group_by(unique_study) %>%
  mutate(max_treatment_val=max(treat_value),
         min_treatment_val=min(treat_value))

#now contract so each row is a unique study
one_entry_per_study<-data_in_window_2 %>%
  group_by(unique_study) %>%
  slice(1)

#expand into a sequence of 10 x-values per study, for which predictions will be generated
seq_x_per_study<-one_entry_per_study %>%
  group_by(unique_study) %>%
  do(data.frame(seq_x_val=seq(.$min_treatment_val,.$max_treatment_val, length.out=10)))


#join with new_data so that new df has study, metrics of study
seq_x_per_study_2<-left_join(seq_x_per_study, one_entry_per_study, by="unique_study")

#predict model fits for plotting
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
data_in_window<-read_csv("processed_data/data_in_window.csv") %>%
  mutate(study_design=ifelse(study_design=="regression", "regression", "anova"))


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
  theme(axis.text.x = element_text(angle = -45, hjust=0)) +
  labs(x="treatment value", y="relative response",
       lty = "study design", fill="response type", col="response type")
ggsave("figures/model_fits_over_raw_data.pdf", height=9, width=9)
ggsave("figures/model_fits_over_raw_data.png", height=9, width=9)

##############
#calculate R2
#############
#read in data subset to relevant range
data_in_window<-read_csv("processed_data/data_in_window.csv")


# predict response from LM results for precise treatment values of study
# then calculate R2 by hand (residuals - observed)^2

#get model-predicted response for each treatment value (need output from lm_anova_resampled)
#initialize columns to store outouts

anova_studies<-data_in_window %>%
  filter(study_design != c("regression")) 

anova_studies$predicted_resp<-as.numeric(NA)
for(i in 1:length(anova_studies$treat_value)){
  anova_studies$predicted_resp[i]= mean(filter(lm_anova_resampled, 
                         unique_study==anova_studies$unique_study[i], term=="treat_value")$estimate*
                           anova_studies$treat_value[i] +
                          filter(lm_anova_resampled, 
                                 unique_study==anova_studies$unique_study[i], term=="(Intercept)")$estimate)
}

anova_studies<-anova_studies %>%
  group_by(unique_study) %>%
  mutate(mean_obs_response=mean(rel_response, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(resid=(predicted_resp-rel_response)^2) %>%
  mutate(total_var=(rel_response-mean_obs_response)^2) %>%
  group_by(unique_study) %>%
  mutate(SRR=sum(resid),
         SST=sum(total_var),
         r_square=1-(SRR/SST)) %>%
  mutate(r_square=ifelse(r_square=="-Inf", 1, r_square)) #change -Inf to 1 because there was no variation in observed response
anova_studies$r_square

plot(anova_studies$r_square)
#ok, predictions made only for anova-style data so far.

#for regression-style data, use glance for R2
regres_studies<-filter(data_in_window, study_design == c("regression")) 
regres_studies$r_square<-as.numeric(NA)
for (i in regres_studies$unique_study){
mod_data<-regres_studies %>%
  filter(unique_study==i)
mod<-lm(rel_response~treat_value, data=mod_data)

regres_studies[regres_studies$unique_study==i,]$r_square<-glance(mod)$r.squared 
}
plot(regres_studies$r_square)


#join the regression and anova type dataframes
data_in_window_r<-rbind(select(anova_studies, -c(mean_obs_response, resid, total_var, SRR, SST)), regres_studies) %>%
  mutate(English_Name_short=ifelse(English_Name=="Canopy-forming Kelp", "kelp", English_Name)) %>%
  mutate(English_Name_short=ifelse(English_Name_short=="copper & quillback rockfish", "rockfish cq", English_Name_short)) %>%
  mutate(English_Name_short=ifelse(English_Name_short=="blue & black rockfish", "rockfish bb", English_Name_short)) %>%
  mutate(species_treatment_study=paste(English_Name_short, treatment_var, unique_study, sep=" ")) %>%
  mutate(species_study=paste(English_Name_short, unique_study, sep=" "))
write_csv(data_in_window_r, "processed_data/data_in_window_r.csv")

#left_join to prediction data
data_to_predict_r<-data_in_window_r %>%
  group_by(unique_study) %>%
  summarize(r_square=mean(r_square), n=length(treatment)) %>%
  left_join(data_to_predict, ., by="unique_study") %>%
  mutate(English_Name_short=ifelse(English_Name=="Canopy-forming Kelp", "kelp", English_Name)) %>%
  mutate(English_Name_short=ifelse(English_Name_short=="copper & quillback rockfish", "rockfish cq", English_Name_short)) %>%
  mutate(English_Name_short=ifelse(English_Name_short=="blue & black rockfish", "rockfish bb", English_Name_short)) %>%
  mutate(species_treatment_study=paste(English_Name_short, treatment_var, unique_study, sep=" ")) %>%
  mutate(species_study=paste(English_Name_short, unique_study, sep=" "))


data_in_window_r$species_treatment_study <- factor(data_in_window_r$species_treatment_study)
fct_reorder(data_in_window_r$species_treatment_study, data_in_window_r$r_square, min)


#plot all relationships where R1<0.3 to look for non-linearities
data_in_window_r %>%
  filter(r_square<0.3) %>%
  ggplot(aes(x=treat_value, y=rel_response, col=response_type)) + geom_point() +
  facet_wrap(~fct_reorder(species_treatment_study, r_square), scales = "free_x") +
  coord_cartesian(ylim=c(-0.1, 1.3)) +
  geom_text(x = -Inf, y = 1.2, hjust = 0, aes(label=round(r_square, digits=3))) +
  geom_line(data=filter(data_to_predict_r, r_square<0.3),
            aes(x=seq_x_val, y=mean_pred, group=(as.factor(unique_study)), lty=study_design)) +
  geom_errorbar(aes(ymax=rel_upperSE, ymin=rel_lowerSE)) +
  geom_ribbon(data=filter(data_to_predict_r, r_square<0.3), 
              aes(x=seq_x_val, ymin=low_pred, ymax=upp_pred, group=as.factor(unique_study), 
                  fill=response_type), linetype=0, alpha = 0.5) +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  theme(axis.text.x = element_text(angle = -45, hjust=0)) +
  labs(x="treatment value", y="relative response",
       lty = "study design", fill="response type", col="response type")
ggsave("figures/model_fits_over_raw_data_low_Rsquare.pdf", height=9, width=9)
ggsave("figures/model_fits_over_raw_data_low_Rsquare.png", height=9, width=9)


#plot all relationships with R2 to assess fit, ordered by R2, separately for each treatment variable
data_in_window_r %>%
  filter(treatment_var=="temperature") %>%
  ggplot(aes(x=treat_value, y=rel_response, col=response_type)) + geom_point() +
  facet_wrap(~fct_reorder(species_study, r_square), scales = "free_x", ncol=7) +
  coord_cartesian(ylim=c(-0.1, 1.3)) +
  geom_text(x = -Inf, y = 0, hjust = 0, aes(label=round(r_square, digits=3))) +
  geom_line(data=filter(data_to_predict_r, treatment_var=="temperature"),
            aes(x=seq_x_val, y=mean_pred, group=(as.factor(unique_study)), lty=study_design)) +
  geom_errorbar(aes(ymax=rel_upperSE, ymin=rel_lowerSE)) +
  geom_ribbon(data=filter(data_to_predict_r, treatment_var=="temperature"), 
              aes(x=seq_x_val, ymin=low_pred, ymax=upp_pred, group=as.factor(unique_study), 
                  fill=response_type), linetype=0, alpha = 0.5) +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  theme(axis.text.x = element_text(angle = -45, hjust=0)) +
  labs(x="treatment value", y="relative response",
       lty = "study design", fill="response type", col="response type")
ggsave("figures/temp_linearfits_Rsquare.pdf", height=12, width=9)

data_in_window_r %>%
  filter(treatment_var=="CO2") %>%
  ggplot(aes(x=treat_value, y=rel_response, col=response_type)) + geom_point() +
  facet_wrap(~fct_reorder(species_study, r_square), scales = "free_x", ncol=7) +
  coord_cartesian(ylim=c(-0.1, 1.3)) +
  geom_text(x = -Inf, y = 0, hjust = 0, aes(label=round(r_square, digits=3))) +
  geom_line(data=filter(data_to_predict_r, treatment_var=="CO2"),
            aes(x=seq_x_val, y=mean_pred, group=(as.factor(unique_study)), lty=study_design)) +
  geom_errorbar(aes(ymax=rel_upperSE, ymin=rel_lowerSE)) +
  geom_ribbon(data=filter(data_to_predict_r, treatment_var=="CO2"), 
              aes(x=seq_x_val, ymin=low_pred, ymax=upp_pred, group=as.factor(unique_study), 
                  fill=response_type), linetype=0, alpha = 0.5) +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  theme(axis.text.x = element_text(angle = -45, hjust=0)) +
  labs(x="treatment value", y="relative response",
       lty = "study design", fill="response type", col="response type")
ggsave("figures/CO2_linearfits_Rsquare.pdf", height=8, width=9)

data_in_window_r %>%
  filter(treatment_var=="pH") %>%
  ggplot(aes(x=treat_value, y=rel_response, col=response_type)) + geom_point() +
  facet_wrap(~fct_reorder(species_study, r_square), scales = "free_x", ncol=7) +
  coord_cartesian(ylim=c(-0.1, 1.3)) +
  geom_text(x = -Inf, y = 0, hjust = 0, aes(label=round(r_square, digits=3))) +
  geom_line(data=filter(data_to_predict_r, treatment_var=="pH"),
            aes(x=seq_x_val, y=mean_pred, group=(as.factor(unique_study)), lty=study_design)) +
  geom_errorbar(aes(ymax=rel_upperSE, ymin=rel_lowerSE)) +
  geom_ribbon(data=filter(data_to_predict_r, treatment_var=="pH"), 
              aes(x=seq_x_val, ymin=low_pred, ymax=upp_pred, group=as.factor(unique_study), 
                  fill=response_type), linetype=0, alpha = 0.5) +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  theme(axis.text.x = element_text(angle = -45, hjust=0)) +
  labs(x="treatment value", y="relative response",
       lty = "study design", fill="response type", col="response type")
ggsave("figures/pH_linearfits_Rsquare.pdf", height=2, width=6)

data_in_window_r %>%
  filter(treatment_var=="oxygen") %>%
  ggplot(aes(x=treat_value, y=rel_response, col=response_type)) + geom_point() +
  facet_wrap(~fct_reorder(species_study, r_square), scales = "free_x", ncol=7) +
  coord_cartesian(ylim=c(-0.1, 1.3)) +
  geom_text(x = -Inf, y = 0, hjust = 0, aes(label=round(r_square, digits=3))) +
  geom_line(data=filter(data_to_predict_r, treatment_var=="oxygen"),
            aes(x=seq_x_val, y=mean_pred, group=(as.factor(unique_study)), lty=study_design)) +
  geom_errorbar(aes(ymax=rel_upperSE, ymin=rel_lowerSE)) +
  geom_ribbon(data=filter(data_to_predict_r, treatment_var=="oxygen"), 
              aes(x=seq_x_val, ymin=low_pred, ymax=upp_pred, group=as.factor(unique_study), 
                  fill=response_type), linetype=0, alpha = 0.5) +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  theme(axis.text.x = element_text(angle = -45, hjust=0)) +
  labs(x="treatment value", y="relative rzr5wssesponse",
       lty = "study design", fill="response type", col="response type")
ggsave("figures/oxygen_linearfits_Rsquare.pdf", height=3, width=9)



study_model_fits<-data_in_window_r %>%
  group_by(unique_study) %>%
  summarize(r_squ=mean(r_square), n=length(treatment)) %>%
  filter(n>2) 
hist(study_model_fits$r_squ, breaks=30)

quantile(study_model_fits$r_squ, c(0.1, 0.25, 0.5, 0.75, 0.9))
mean(study_model_fits$r_squ)
min(study_model_fits$r_squ)
max(study_model_fits$r_squ)
