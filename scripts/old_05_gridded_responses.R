#goal: calculte response by grid, stressor, and response type.

#librarieslibrary(nlme) 

library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(cowplot)
library(maps)
library(reshape2)
library(mapdata)

setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_cal.csv")

#read in one of Sam's maps
littletemp<-read_csv("processed_data/2km_delta_temp_200m.csv", col_names=F)

#reshape it into a long dataframe
littletemp.df<-melt(littletemp) %>%
  mutate(lat=rep(1:dim(littletemp)[1], dim(littletemp)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

head(littletemp.df) # have a look

#plot
littletemp.df %>% 
  ggplot(aes(x = long, y = lat)) + geom_tile(aes(fill = value))

#add sensitivities to calculate responses
sensitivity_by_study<-read_csv("processed_data/sensitivity_by_study_cal.csv")
test<-sensitivity_by_study %>%
  filter(treatment_var=="temperature",
         English_Name=="Pink Salmon")
View(test)
head(sensitivity_by_study)
littletemp.df %>% 
  mutate(temp_sens)