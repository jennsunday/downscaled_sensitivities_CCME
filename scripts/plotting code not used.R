#make a dataframe for polygons to show non-interest area in plots
low_poly<-data.frame(treatment_var=c(rep(levels(data$treatment_var)[1],4),
                                     rep(levels(data$treatment_var)[2],4),
                                     rep(levels(data$treatment_var)[3],4),
                                     rep(levels(data$treatment_var)[4],4)),
                     x=c(0, range_CO2[1], range_CO2[1], 0, 
                         0, range_O2[1], range_O2[1], 0,
                         0, range_salinity[1], range_salinity[1], 0,
                         0, range_temp[1], range_temp[1], 0),
                     y=c(-1, -1, 2, 2, -1, -1, 2, 2, -1, -1, 2, 2, -1, -1, 2, 2), study="yes")

high_poly<-data.frame(treatment_var=c(rep(levels(data$treatment_var)[1],4),
                                      rep(levels(data$treatment_var)[2],4),
                                      rep(levels(data$treatment_var)[3],4),
                                      rep(levels(data$treatment_var)[4],4)),
                      x=c(4500, range_CO2[2], range_CO2[2], 4500, 
                          40, range_O2[2], range_O2[2], 40,
                          70, range_salinity[2], range_salinity[2], 70,
                          50, range_temp[2], range_temp[2], 50),
                      y=c(-1, -1, 2, 2, -1, -1, 2, 2, -1, -1, 2, 2, -1, -1, 2, 2), study="yes")

#plot data with out of range treatments greyed out
data %>% 
  group_by(English_Name, treatment_var) %>% 
  ggplot(aes(y=rel_response, x=treat_value, group=study, color=response_type)) + geom_point() +
  facet_grid(English_Name ~ treatment_var, scales = "free_x") + theme_bw() +
  stat_smooth(data=subset(data, data$study_design=="regression"), span=4) +
  labs(x = "Treatment Value", y="Relative response") +
  geom_line(data=subset(data, data$study_design %in% c("anova", "multi-level anova"))) +
  geom_polygon(data=low_poly, aes(y=y, x=x, color=NA, alpha=0.1), show.legend = FALSE) + 
  geom_polygon(data=high_poly, aes(y=y, x=x, color=NA, alpha=0.1), show.legend = FALSE) +
  coord_cartesian(ylim = c(-0.1, 1.1))
ggsave("figures/responses_over_relavant_windows.pdf", width=9, height=6)
