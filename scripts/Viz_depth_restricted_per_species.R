#goal: get weighted mean of each response type per species for the average environmental change in the system
#goal: get weighted mean positive and negative response per species for the average environmental change in the system

library(tidyverse)
library(broom)
library(PNWColors)

sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_zoned.csv") %>%
  mutate(pos_neg=ifelse(percentchange>0, "pos", "neg"))


#calculate weighted mean of percent change by species x response type
sensitivity_by_response_type<-sensitivity_by_study %>%
  #mutate(weight_slope=1/((percentchangeSE+0.001)^2)) %>% #make a weighting variable for every result
  group_by(common_name, response_type) %>%
  mutate(variance=percentchangeSE^2, weight=1/(variance+0.00000001)) %>%
  dplyr::summarize(weighted_sensitivity=weighted.mean(percentchange, w=weight), 
                   weight_test=sum(weight*percentchange)/sum(weight),
                   n=n(), 
                   weight_sum=sum(weight), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_sensitivity-1.96*SE_wmean, hi_95=weighted_sensitivity+1.96*SE_wmean)

#calculate weighted mean of percent change by species x response sign (+/-)
sensitivity_by_pos_neg<-sensitivity_by_study %>%
#  mutate(weight_slope=1/((std.error+0.001)^2)) %>% #make a weighting variable for every result
  group_by(common_name, pos_neg) %>%
  mutate(variance=percentchangeSE^2, weight=1/(variance+0.00000001)) %>%
  dplyr::summarize(weighted_sensitivity=weighted.mean(percentchange, w=weight), 
                   weight_test=sum(weight*percentchange)/sum(weight),
                   n=n(), 
                   weight_sum=sum(weight), SE_wmean=sqrt(1/(sum(weight)))) %>%
  mutate(lo_95=weighted_sensitivity-1.96*SE_wmean, hi_95=weighted_sensitivity+1.96*SE_wmean)

#by response type
#set up for plotting
label_species<-sensitivity_by_response_type %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(weighted_sensitivity)) %>%
  ungroup %>%
  arrange(mean_change, weighted_sensitivity) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(mean_order=mean(compound_order), 
            mean_response=mean(weighted_sensitivity), 
            min_response=min(weighted_sensitivity), 
            max_response=max(weighted_sensitivity)) %>%
  ungroup() %>%
  mutate(x_position=ifelse(min_response>(-8), min_response-10, max_response+10)) %>%
  mutate(x_position=x_position)

#plot
sensitivity_by_response_type %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(weighted_sensitivity)) %>%
  ungroup %>%
  arrange(mean_change, weighted_sensitivity) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  ggplot(aes(x=weighted_sensitivity, y=compound_order, 
             shape=response_type)) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-40,40)) +
  scale_size_continuous(range = c(2, 4)) +
  geom_path(aes(group=common_name)) +
  geom_point(aes(size=n), fill="black") + theme_classic() +
  labs(y = "Response", x="Percent change in biological rate", 
       shape="response type") +
  geom_errorbarh(aes(xmin=lo_95, 
                     xmax=hi_95), height=0) +
  scale_shape_manual(values=c(21, 22, 23, 24, 25))+
  scale_colour_manual(values=pnw_palette("Bay",5)) +
  scale_fill_manual(values=pnw_palette("Bay",5)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_label(data=label_species, inherit.aes = F, aes(x=x_position, y=mean_order, label=common_name))


#by sign
#set up for plotting
label_species<-sensitivity_by_pos_neg %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(weighted_sensitivity)) %>%
  ungroup %>%
  arrange(mean_change, weighted_sensitivity) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(mean_order=mean(compound_order), 
            mean_response=mean(weighted_sensitivity), 
            min_response=min(weighted_sensitivity), 
            max_response=max(weighted_sensitivity)) %>%
  ungroup() %>%
  mutate(x_position=ifelse(min_response>(-8), min_response-10, max_response+10)) %>%
  mutate(x_position=x_position)

sensitivity_by_pos_neg %>%
  group_by(common_name) %>%
  mutate(mean_change=mean(weighted_sensitivity)) %>%
  ungroup %>%
  arrange(mean_change, weighted_sensitivity) %>%
  mutate(compound_order = c(1:n())) %>%
  ungroup() %>%
  ggplot(aes(x=weighted_sensitivity, y=compound_order)) +
  geom_vline(xintercept = 0, col="grey") +
  coord_cartesian(xlim=c(-40,40)) +
  scale_size_continuous(range = c(0.5, 1.5)) +
  geom_point(aes(size=n)) + theme_classic() +
  labs(y = "Response", x="Percent change in biological rate", 
       col = "treatment", shape="response type") +
  geom_errorbarh(aes(xmin=lo_95, 
                     xmax=hi_95), height=0) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_label(data=label_species, inherit.aes = F, aes(x=x_position, y=mean_order, label=common_name))


