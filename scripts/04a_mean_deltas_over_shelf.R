# Goal: Calculate on-the-shelf mean deltas for each model (2km and 12km),
# and each water range (bottom, 200, surface)
# and each response variable

library(ggplot2)
library(dplyr)
library(tidyverse)
library(maps)
library(reshape2)
library(mapdata)
library(gridExtra)

setwd("/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/")

#read in 500m mask
little_shelf_mask<-read_csv("processed_data/mask_500m_2km.csv", col_names=F)

#read in Sam's delta values 
littleCO2200<-read_csv("processed_data/2km_delta_pCO2_200m.csv", col_names=F)
littleCO2bot<-read_csv("processed_data/2km_delta_pCO2_bot.csv", col_names=F)
littleCO2surf<-read_csv("processed_data/2km_delta_pCO2_surf.csv", col_names=F)

test<-littleCO2200*little_shelf_mask


