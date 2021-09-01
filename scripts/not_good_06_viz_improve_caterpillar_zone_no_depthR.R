#goal: display all data for sensitivity in a way that shows everything
#goal: get weighted mean for each response type per species

#librarieslibrary(nlme) 
#install.packages("ggforce")
library(tidyverse)
library(broom)
library(PNWColors)
library(ggforce)

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")
View(sensitivity_by_study)
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
  scale_color_manual(values = c("#2c7bb6","#fdae61", "#d7191c")) +
  scale_fill_manual(values = c("#2c7bb6", "#fdae61", "#d7191c","#ffffff", "#ffffff", "#ffffff", "#ffffff")) +
  labs(y = "Response", x="Percent change in biological rate", 
       col = "treatment", shape="response type") +
  geom_errorbarh(aes(xmin=percentchange_lo_95, 
                     xmax=percentchange_hi_95), height=0) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_label(data=label_species, inherit.aes = F, aes(x=80, y=mean_order, label=common_name))
ggsave("figures/caterpillar_by_species.png", height=8, width=8)
ggsave("figures/caterpillar_by_species.pdf", height=8, width=8)


#####add species for which sensitivities were estimated from Metabolic Index
#Anchovy MI: -19.4%
#Shrimp MI: -29.7%
#make a dataframe to plot Evan's species' responses.
MI_index_response<-data.frame(English_Name=c("anchovy","shrimp"), 
                              percentchange=c(-19.4, -29.7),
                              response_type="growth")
MI_index_response %>%
  ggplot(aes(x=percentchange, y=English_Name)) +
  theme_bw() +
  labs(y = "Species", x="Percent change in biological rate") +
  coord_cartesian(xlim=c(-75,180)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_point(size=2, shape=0) + theme_classic() +
  geom_label(aes(x=80, y=English_Name, label=English_Name)) +
  geom_vline(xintercept = 0, col="grey")
ggsave("figures/MI_sensitivities.pdf", width = 8, height = 1)
