#goal 1: ask if propagule responses are more extreme than adult responses.
#goal 2: ask if lower-organization responses are more extreme than higher-organization responses



#librarieslibrary(nlme) 
#install.packages("PNWColors")
library(tidyverse)
library(nlme)
library(broom)
library(PNWColors)
library(MuMIn)

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_cal.csv")

sensitivity_by_study %>%
  ggplot(aes(x=Life_stage_category, y=abs(percentchange))) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~common_name)
#nope, propagules do not have more extreme absolute responses

unique(sensitivity_by_study$Life_stage_category)
#order of increasing organization complexity
org_complex<-data.frame(response_type=unique(sensitivity_by_study$response_type),
                        complex=c(1,2,4,5,3))

#get org_complex into the dataframe
sensitivity_by_study<-left_join(sensitivity_by_study, org_complex, by = "response_type")


#fit model
mod<-lme((percentchange)~complex+Life_stage_category, random=~1|common_name, data=sensitivity_by_study)
summary(mod)
newdata=data.frame(expand_grid(complex=seq(1,5, length.out=10), 
                               Life_stage_category=c("adult or juvenile", "propagule")))
pred<-predict(mod, newdata, level=0, se.fit=T)
fitted_pred<-newdata %>%
  mutate(percentchange=pred$fit) %>%
  mutate(percentchange_SE=pred$se.fit)

#make a dataframe for the x-axis label with categories in order of complexity
responses_in_order<-unique(sensitivity_by_study$response_type)[order(unique(sensitivity_by_study$complex))]

#plot
sensitivity_by_study %>%
  mutate(treatment_var2=ifelse(treatment_var %in% c("CO2", "pH"), "CO2 & pH", treatment_var)) %>%
  ggplot(aes(x=factor(complex), y=(percentchange))) +
  geom_violin() +
  geom_jitter(aes(col=Life_stage_category, shape=treatment_var2), height = 0, width = 0.05) +
  #facet_wrap(~common_name) +
  theme_classic() +
  scale_color_manual(values=pnw_palette("Bay",2)) +
  scale_fill_manual(values=pnw_palette("Bay",2)) +
  geom_line(data=fitted_pred, inherit.aes = F, aes(x=(complex), y=(percentchange), col=Life_stage_category))+
  geom_ribbon(data=fitted_pred, aes(x=(complex), ymin=(percentchange-percentchange_SE), 
                                    ymax=percentchange+percentchange_SE, fill=Life_stage_category), alpha=0.1) +
  scale_x_discrete(labels=responses_in_order) +
  xlab("response type in order of increasing complexity") +
  ylab("percent change in response") +
  labs(shape="treatment variable", fill="life stage", col="life stage")
ggsave("figures/ccomplexity_order.png", height=5, width=5)


