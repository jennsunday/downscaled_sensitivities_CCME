# load libraries ----------------------------------------------------------
detach(package:plyr)  
library(nlme) #for mixed models
library(MuMIn) # to do model averaging
library(mgcv)
library(ggplot2)
library(broom)
library(tidyverse)
library(dplyr)

#read in the procesed data
setwd('/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/')
data<-read.csv("processed_data/Schmidt_biotic_responses_data_180118.csv")


#remove very very high CO2 values
data <- data %>%
  filter(treat_value<2000)

#make a column of species names
data$genus_species<-paste(data$Genus, data$Species, sep="_")


#plot all the data - relative response
data %>% 
 ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type, linetype=study_design)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() +
  stat_smooth(data=subset(data, data$study_design=="regression"), span=4) +
  geom_line(data=subset(data, data$study_design %in% c("anova", "multi-level anova"))) +
  xlab("treatment value") + ylab("relative response")
ggsave("figures/data_connected.pdf", width = 8, height = 8)

#set range of relevant values for each stressor
range_temp<-c(7,16)
range_salinity<-c(29,33.5)
range_O2<-c(0.5,8.5)
range_CO2<-c(350,1100)


#get these into the main data frame
data<-rbind(data %>%
  filter(treatment_var=="CO2") %>%
  mutate(upper_window=range_CO2[2]) %>%
  mutate(lower_window=range_CO2[1]),
            data %>%
  filter(treatment_var=="oxygen") %>%
  mutate(upper_window=range_O2[2]) %>%
  mutate(lower_window=range_O2[1]) ,
            data %>%
  filter(treatment_var=="salinity") %>%
  mutate(upper_window=range_salinity[2]) %>%
  mutate(lower_window=range_salinity[1]) ,
            data %>%
  filter(treatment_var=="temperature") %>%
  mutate(upper_window=range_temp[2]) %>%
  mutate(lower_window=range_temp[1])
            )



#for every study, subset to all values inside window plus frist value outside window
data_in_window<-data %>%
  group_by(study) %>%
  filter(treat_value>=lower_window & treat_value<=upper_window |
           treat_value==max(treat_value[which(treat_value < lower_window)]) | 
           treat_value==min(treat_value[which(treat_value > upper_window)])) %>%
  filter(length(log_rel_response)>1) %>% #Remove studies where NO data were in window
  mutate(weighting=length(which(treat_value>=lower_window & treat_value<=upper_window))) %>% #make weighting value
  ungroup()

#lots of warnings - perhaps for cases when there is no treat_value outside of window?
write_csv(data_in_window, "processed_data/data_in_window.csv")


#plot all the data - relative response
data_in_window %>% 
  ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() + 
  stat_smooth(data=subset(data_in_window, data$study_design=="regression"), span=4) +
  geom_line(data=subset(data_in_window, data$study_design %in% c("anova", "multi-level anova"))) 

#
#
#
#
#
#delete after here

#fit lme to each multi-study category with study as a random effect, weighting as weight (need to add small value so weight != 0)
lmeall<-data_in_window %>%
  group_by(English_Name, treatment_var) %>%
  filter(length(unique(study))>1) %>% #subset to categories with more than one study for lme
  do(tidy(lme(rel_response~treat_value, weights=~weighting+0.001, random=~1|study, data=.), effects="fixed"))

#get predicted values from this model
lmeallpred<-data_in_window %>%
  group_by(English_Name, treatment_var) %>%
  filter(length(unique(study))>1) %>% #subset to categories with more than one study for lme
  do(augment(lme(rel_response~treat_value, weights=~weighting+0.001, random=~1|study, data=.), effects="fixed"))

#fit lm to each single-study category with study as a random effect, weighting as weight (need to add small value so weight != 0)
lmall<-data_in_window %>%
  group_by(English_Name, treatment_var) %>%
  filter(length(unique(study))==1) %>% #subset to categories with only one study for lm
  do(tidy(lm(rel_response~treat_value, data=.)))

#get predicted values from this model
lmallpred<-data_in_window %>%
  group_by(English_Name, treatment_var) %>%
  filter(length(unique(study))==1) %>% #subset to categories with only one study for lm
  do(augment(lm(rel_response~treat_value, data=.)))

mod_results<-rbind(lmeall, lmall)
mod_predictions<-rbind(lmeallpred, lmallpred)


#plot all data with fitted slopes
data_in_window %>%
  ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type)) + geom_point() +
  facet_grid(treatment_var~English_Name, scales = "free_x") + theme_bw() +
  coord_cartesian(ylim = c(-1.1, 1.1)) + labs(x = "Treatment Value", y="Relative response") +
  geom_line(data=mod_predictions, aes(x=treat_value, y=.fitted, colour=factor(response_type)))
ggsave("figures/fitted_linear_slopes.pdf")
#add error bars

#get a vector of range of window for each variable for standardizing slopes
range_of_x<-data_in_window%>%
  group_by(English_Name, treatment_var) %>%
  mutate(range=upper_window-lower_window) %>%
  summarise(range=median(range)) %>%
  rename(English_Name2=English_Name, treatment_var3=treatment_var)

#add range to slopes data 
slopes<-mod_results %>%
  filter(term=="treat_value") %>% #filter out just slope responses
  arrange(English_Name, treatment_var) #order the slopes estimates to match range data

sensitivity_results<-cbind(range_of_x, slopes) %>% #bind range of x values to slopes estimates
  mutate(direction=ifelse(treatment_var3 %in% c("oxygen", "salinity"), -1, 1)) %>%
  mutate(sensitivity=estimate*range*direction) %>% #calculate sensitivity
  mutate(SEsensitivity=std.error*range*direction) #calculate SE of sensitivity

#change slopes into % change given expected total change in each stressor.
sensitivity_results$change_in_2100<-case_when(sensitivity_results$treatment_var=="CO2" ~ 300,
                                              sensitivity_results$treatment_var=="temperature" ~ 3,
                                              sensitivity_results$treatment_var=="oxygen" ~ 0.64,
                                              sensitivity_results$treatment_var=="salinity" ~ 3,
                                               TRUE ~ -99)

sensitivity_results<-sensitivity_results %>%
  mutate(percentchange=estimate*change_in_2100*direction*100) %>%
  mutate(SEpercentchange=std.error*change_in_2100*direction*100)

#arrange order of species
unique(sensitivity_results$English_Name)
order_desired<-c("Canopy-forming Kelp", "seagrass", "Razor Clam", "Dungeness crab", 
                 "Red urchin", "Ochre star", "blue & black rockfish", "copper & quillback rockfish",
                 "sablefish", "Pink Salmon")

sensitivity_results<-sensitivity_results %>%
  mutate(category =  factor(English_Name, levels = order_desired)) %>%
  arrange(category)  

sensitivity_results$English_Name <- factor(sensitivity_results$English_Name, 
                                            levels = rev(order_desired))

#plot sensitivity
sensitivity_results %>%
  ggplot(aes(x=percentchange, y=English_Name)) + facet_wrap(~treatment_var) + geom_point() +
  labs(y = "Species") +
  geom_errorbarh(aes(xmin=percentchange-SEpercentchange, xmax=percentchange+SEpercentchange), height=0) +
  geom_vline(xintercept = 0, linetype="dotted") + theme_bw()
ggsave("figures/sensitivity.pdf", width = 6, height = 5)

#plot sensitivity - ditch salinity
sensitivity_results %>%
  filter(treatment_var!="salinity") %>%
  ggplot(aes(x=percentchange, y=English_Name)) + 
  scale_colour_gradient2(mid="grey", high = "red", low="red") +
  facet_wrap(~treatment_var) + geom_point() +
  labs(y = "Species", x="Percent change in performance") +
  geom_errorbarh(aes(xmin=percentchange-SEpercentchange*2, xmax=percentchange+SEpercentchange*2), height=0) +
  geom_vline(xintercept = 0, linetype="dotted") + theme_bw()
ggsave("figures/sensitivity.pdf", width = 8, height = 3)

#plot slopes
sensitivity_results %>%
  ggplot(aes(x=estimate, y=English_Name)) + facet_wrap(~treatment_var, scale = "free_x") + geom_point() +
  labs(y = "Species") +
  geom_errorbarh(aes(xmin=estimate-std.error, xmax=estimate+std.error), height=0) +
  geom_vline(xintercept = 0, linetype="dotted") + theme_bw()
#ggsave("figures/sensitivity.pdf", width = 6, height = 5)

#label "response to increased CO2" "response to decreased oxygen"...
#add invidivual responses of each category (reduce number of categories)
#reorder in a sensible way - highest to lowest sensitivity? - plants, inverts, fish?

# gets creativ after here
#
#
#
#
#
#

#show responses with every response type in a different plot, colour by treatment, symbol by speciessensitivity_results %>%
#fit lm to every response type
lm_every_study<-data_in_window %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Response_variable, response_type, study, treatment_var) %>%
  do(tidy(lm(rel_response~treat_value, data=.)))

lm_every_study_pred<-data_in_window %>%
  group_by(Author, Pub_Year, English_Name, Life_stage, Response_variable, response_type, study, treatment_var) %>%
  do(augment(lm(rel_response~treat_value, data=.)))

data_in_window %>%
  ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() +
  coord_cartesian(ylim = c(-0.1, 1.1)) + labs(x = "Treatment Value", y="Relative response") +
  geom_line(data=lm_every_study_pred, aes(x=treat_value, y=.fitted, colour=factor(response_type)))

#take each slope, set intercept so they cross at ref temp, weight by sample size (haven't done this yet)
#this is one other way to visualize sensitivity

ref_value<-c(10, 10, 32, 300)
fut_value<-c(2, 16, 26, 1000)

par(mfrow=c(2,2))
par(mar=c(4,4,0.5,0.5), oma=c(0,0,0,0))
for(i in 1:length(unique(lm_every_study$treatment_var))){
    temp_pred<-filter(lm_every_study_pred, treatment_var==unique(lm_every_study_pred$treatment_var)[i])
    temp_slopes<-filter(lm_every_study, treatment_var==unique(lm_every_study$treatment_var)[i])
        with(temp_pred, plot(rel_response~treat_value, type="n", ylim=c(-1.5,1.5), 
                             xlim=c(ref_value[i], fut_value[i])))
        for(j in 1:length(unique(temp_slopes$study))){
        segments(ref_value[i], 0, 
                fut_value[i], filter(temp_slopes, 
                                     term=="treat_value")$estimate[j]*fut_value[i],
                col=filter(temp_slopes, 
                           term=="treat_value")$response_type[j])
        }
}

#try to ggplot it - couldn't figure out how to include ref value
#making a vector of ref values for every lm
ref_for_each_lm<-case_when(lm_every_study$treatment_var== "oxygen" ~ ref_value[1],
          lm_every_study$treatment_var=="temperature" ~ ref_value[2],
          lm_every_study$treatment_var=="salinity"~ ref_value[3],
          lm_every_study$treatment_var=="CO2"~ ref_value[4])

fut_for_each_lm<-case_when(lm_every_study$treatment_var== "oxygen" ~ fut_value[1],
                           lm_every_study$treatment_var=="temperature" ~ fut_value[2],
                           lm_every_study$treatment_var=="salinity"~ fut_value[3],
                           lm_every_study$treatment_var=="CO2"~ fut_value[4])

#add that column to lm_every_study
lm_every_study$ref<-ref_for_each_lm
lm_every_study$fut<-fut_for_each_lm

#plot in ggplot
lm_every_study_pred %>%
ggplot(aes(y=rel_response, x=treat_value)) +
  facet_wrap(~treatment_var, scales="free") + 
  theme_bw() +
  geom_segment(data=filter(lm_every_study, term=="treat_value"), 
               aes(x=ref, y=0, xend=fut, yend=estimate*fut, col=response_type))

lm_every_study_pred %>%
  ggplot(aes(y=rel_response, x=treat_value)) +
  facet_grid(response_type~treatment_var, scales="free") + 
  theme_bw() +
  geom_segment(data=filter(lm_every_study, term=="treat_value"), 
               aes(x=ref, y=0, xend=fut, yend=estimate*fut, col=English_Name)) +
  geom_point(data=filter(lm_every_study, term=="treat_value"), 
               aes(x=fut, y=estimate*fut, col=English_Name, shape=English_Name)) + 
                scale_shape_manual(values=seq(0,10))
ggsave("figures/sensitivy_slopes.pdf", width = 6, height = 6)

lm_every_study_pred %>%
  ggplot(aes(y=rel_response, x=treat_value)) +
  facet_grid(English_Name~treatment_var, scales="free") + 
  theme_bw() +
  geom_segment(data=filter(lm_every_study, term=="treat_value"), 
               aes(x=ref, y=0, xend=fut, yend=estimate*fut, col=response_type))



#plot response to a given exposure where exposure is an input vector 
#(or reference and future values are inputs)
ref_value<-c(10, 10, 32, 300) #order is: oxygen, temp, sal, CO2
fut_value<-c(10, 16, 26, 400) #warmer, fresher, low change in O2 and CO2
fut_value2<-c(2, 12, 30, 800) #lower O2, increased CO2, less change in temp and salinity

exp_value<-fut_value2-ref_value

range_value<-c(range_O2[2]-range_O2[1], range_temp[2]-range_temp[1], 
               range_salinity[2]-range_salinity[1], range_CO2[2]-range_CO2[1])
               
exposure1<-data_frame(rel_exposure=(fut_value-ref_value)/range_value, 
                      abs_exposure=fut_value-ref_value, 
              variable=c("oxygen", "temperature", "salinity", "CO2"), origin=rep(0, 4))
exposure2<-data_frame(rel_exposure=(fut_value2-ref_value)/range_value, 
                      abs_exposure=fut_value2-ref_value, 
                      variable=c("oxygen", "temperature", "salinity", "CO2"), origin=rep(0, 4))

#plot relative exposure - can't plot actual exposure bc units are so different
exposure1 %>%
  ggplot(aes(x=rel_exposure, y=variable)) + geom_point() + 
  labs(y = "", x="relative exposure 1") + theme_bw() +
  geom_segment(aes(x=origin, y=variable, xend=rel_exposure, yend=variable))
ggsave("figures/exposure_eg_1.pdf", width = 3, height = 2)
exposure2 %>%
  ggplot(aes(x=rel_exposure, y=variable)) + geom_point() +
  labs(y = "", x="relative exposure 2") + theme_bw() +
  geom_segment(aes(x=origin, y=variable, xend=rel_exposure, yend=variable))
ggsave("figures/exposure_eg_2.pdf", width = 3, height = 2)


#add appropriate exposure to each sensitivity row in sensitivity table
exposure<-case_when(sensitivity_results$treatment_var== "oxygen" ~ exp_value[1],
                                   sensitivity_results$treatment_var=="temperature" ~ exp_value[2],
                                   sensitivity_results$treatment_var=="salinity"~ exp_value[3],
                                   sensitivity_results$treatment_var=="CO2"~ exp_value[4])

sensitivity_results$exposure<-exposure
sensitivity_results$res_to_exposure<-sensitivity_results$exposure*sensitivity_results$estimate

#sum response across exposure types
total_response<-sensitivity_results %>%
  group_by(English_Name)  %>%
  summarise(tot_response=sum(res_to_exposure)) %>%
  arrange(tot_response) 
#order species levels by total response
total_response$English_Name<-factor(total_response$English_Name, 
                                    levels = total_response$English_Name[order(total_response$tot_response)])

#plot response
total_response %>%
  ggplot(aes(x=tot_response, y=English_Name)) + geom_point() +
  labs(y = "Species") + theme_bw() +
  geom_vline(xintercept = 0, linetype="dotted")
ggsave("figures/response_eg_2.pdf", width = 4, height = 3)
