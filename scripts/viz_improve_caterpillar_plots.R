#goal: display all data for sensitivity in a way that shows everything

#librarieslibrary(nlme) 
#install.packages("PNWColors")
library(tidyverse)
library(broom)
library(PNWColors)

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_cal.csv")


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
#plot data
###############

label_species<- sensitivity_by_study %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(percentchange)) %>%
  ungroup %>%
  arrange(mean_change, percentchange) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(mean_order=mean(compound_order), 
            mean_response=mean(percentchange), 
            min_response=min(percentchange), 
            max_response=max(percentchange)) %>%
  mutate(x_position=ifelse(mean_response>(3), -40, 55)) %>%
  mutate(x_position=ifelse(common_name %in% c("blue & black rockfish", "copper & quillback rockfish"),
                           70, x_position))


sensitivity_by_study %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(percentchange)) %>%
  ungroup %>%
  arrange(mean_change, percentchange) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  mutate(treatment_var2=ifelse(treatment_var %in% c("CO2", "pH"), "CO2 & pH", treatment_var)) %>%
  ggplot(aes(x=percentchange, y=compound_order, shape=response_type, col=treatment_var2, 
             fill=interaction(treatment_var2, Life_stage_category))) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-75,180)) +
  geom_point(size=2) + theme_classic() +
  scale_shape_manual(values=c(21, 22, 23, 24, 25))+
  geom_path(aes(group=common_name)) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c","#ffffff", "#ffffff", "#ffffff", "#ffffff")) +
  labs(y = "Response", x="Percent change in biological rate", 
       col = "treatment", shape="response type") +
  geom_errorbarh(aes(xmin=(percentchange+1.96*abs(SEpercentchange)), 
                     xmax=(percentchange-1.96*abs(SEpercentchange))), height=0) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_label(data=label_species, inherit.aes = F, aes(x=80, y=mean_order, label=common_name))
ggsave("figures/caterpillar_by_species.png", height=8, width=4)

