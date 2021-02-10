#goal: display all data for sensitivity in a way that shows everything
#goal: get weighted mean for each response type per species

#librarieslibrary(nlme) 
#install.packages("ggforce")
library(tidyverse)
library(broom)
library(PNWColors)
library(ggforce)

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv")

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



#plyaing around with ellipses to try to make a "rainforrest plot"
sensitivity_by_study %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(percentchange)) %>%
  ungroup %>%
  arrange(mean_change, percentchange) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  mutate(treatment_var2=ifelse(treatment_var %in% c("CO2", "pH"), "CO2 & pH", treatment_var)) %>%
  ggplot(aes(x=percentchange, y=compound_order, shape=response_type, col=treatment_var2, 
             fill=treatment_var2)) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-75,180)) +
  geom_ellipse(aes(x0=percentchange, y0=compound_order, a=ifelse(percentchangeSE*1.96>1, percentchangeSE*1.96, 1), b=1/sqrt(percentchangeSE), angle=0, alpha=1/sqrt(percentchangeSE))) + theme_classic() +
  #scale_shape_manual(values=c(21, 22, 23, 24, 25))+
  geom_path(aes(group=common_name)) +
  scale_color_manual(values = c("#2c7bb6","#fdae61", "#d7191c")) +
  scale_fill_manual(values = c("#2c7bb6", "#fdae61", "#d7191c")) +
  labs(y = "Response", x="Percent change in biological rate", 
       col = "treatment", shape="response type") +
  geom_errorbarh(aes(xmin=percentchange_lo_95, 
                     xmax=percentchange_hi_95), height=0) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_label(data=label_species, inherit.aes = F, aes(x=80, y=mean_order, label=common_name))

names(sensitivity_by_study)

# Basic usage
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = 0)) +
  coord_fixed()

# Rotation
# Note that it expects radians and rotates the ellipse counter-clockwise
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = pi / 4)) +
  coord_fixed()

# Draw a super ellipse
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 6, b = 3, angle = -pi / 3, m1 = 3)) +
  coord_fixed()

test<-c(0.2,4.3,2,56,4,3,6) 
floor(test)
ceiling(test)
ifelse(test>1, test, 1)
