# Goal: Calculate on-the-shelf mean deltas for each model (2km and 12km),
# and each water range (bottom, 200, surface)
# and each response variable
# produce a table

library(ggplot2)
library(tidyverse)
library(maps)
library(reshape2)
library(gridExtra)

#read in 500m mask
little_shelf_mask<-read_csv("raw_data/downscaled_climate_data/mask_500m_2km.csv", col_names=F)
big_shelf_mask<-read_csv("raw_data/downscaled_climate_data/mask_500m_12km.csv", col_names=F)

#0 marks ocean points with depth > 500m, 1 marks ocean points with depth <= 500m

#read in Sam's delta values 
CO2_200_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_pCO2_200m.csv", col_names=F)
CO2_bot_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_pCO2_bot.csv", col_names=F)
CO2_surf_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_pCO2_surf.csv", col_names=F)
temp_200_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_temp_200m.csv", col_names=F)
temp_bot_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_temp_bot.csv", col_names=F)
temp_surf_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_temp_surf.csv", col_names=F)
oxy_200_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_oxy_200m.csv", col_names=F)
oxy_bot_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_oxy_bot.csv", col_names=F)
oxy_surf_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_oxy_surf.csv", col_names=F)
pH_200_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_pH_200m.csv", col_names=F)
pH_bot_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_pH_bot.csv", col_names=F)
pH_surf_2km<-read_csv("raw_data/downscaled_climate_data/2km_delta_pH_surf.csv", col_names=F)


#read in Sam's delta values 
CO2_200_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_pCO2_200m.csv", col_names=F)
CO2_bot_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_pCO2_bot.csv", col_names=F)
CO2_surf_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_pCO2_surf.csv", col_names=F)
temp_200_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_temp_200m.csv", col_names=F)
temp_bot_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_temp_bot.csv", col_names=F)
temp_surf_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_temp_surf.csv", col_names=F)
oxy_200_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_oxy_200m.csv", col_names=F)
oxy_bot_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_oxy_bot.csv", col_names=F)
oxy_surf_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_oxy_surf.csv", col_names=F)
pH_200_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_pH_200m.csv", col_names=F)
pH_bot_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_pH_bot.csv", col_names=F)
pH_surf_12km<-read_csv("raw_data/downscaled_climate_data/12km_delta_pH_surf.csv", col_names=F)


#apply the mask to each
CO2_200_2km[!little_shelf_mask==1] <- NA
CO2_bot_2km[!little_shelf_mask==1] <- NA
CO2_surf_2km[!little_shelf_mask==1] <- NA
temp_200_2km[!little_shelf_mask==1] <- NA
temp_bot_2km[!little_shelf_mask==1] <- NA
temp_surf_2km[!little_shelf_mask==1] <- NA
oxy_200_2km[!little_shelf_mask==1] <- NA
oxy_bot_2km[!little_shelf_mask==1] <- NA
oxy_surf_2km[!little_shelf_mask==1] <- NA
pH_200_2km[!little_shelf_mask==1] <- NA
pH_bot_2km[!little_shelf_mask==1] <- NA
pH_surf_2km[!little_shelf_mask==1] <- NA

CO2_200_12km[!big_shelf_mask==1] <- NA
CO2_bot_12km[!big_shelf_mask==1] <- NA
CO2_surf_12km[!big_shelf_mask==1] <- NA
temp_200_12km[!big_shelf_mask==1] <- NA
temp_bot_12km[!big_shelf_mask==1] <- NA
temp_surf_12km[!big_shelf_mask==1] <- NA
oxy_200_12km[!big_shelf_mask==1] <- NA
oxy_bot_12km[!big_shelf_mask==1] <- NA
oxy_surf_12km[!big_shelf_mask==1] <- NA
pH_200_12km[!big_shelf_mask==1] <- NA
pH_bot_12km[!big_shelf_mask==1] <- NA
pH_surf_12km[!big_shelf_mask==1] <- NA


#melt into long data, and calculate mean delta and sd delta, prepare into dataframes
r1<-data.frame(mean_delta=mean(melt(CO2_200_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(CO2_200_2km, var='x_value')$value, na.rm = TRUE),
                        model="2km", water_range="200m", variable="CO2")
r2<-data.frame(mean_delta=mean(melt(CO2_bot_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(CO2_bot_2km, var='x_value')$value, na.rm = TRUE),
                        model="2km", water_range="bottom", variable="CO2")
r3<-data.frame(mean_delta=mean(melt(CO2_surf_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(CO2_surf_2km, var='x_value')$value, na.rm = TRUE),
                         model="2km", water_range="surface", variable="CO2")
r4<-data.frame(mean_delta=mean(melt(temp_200_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(temp_200_2km, var='x_value')$value, na.rm = TRUE),
                         model="2km", water_range="200m", variable="temp")
r5<-data.frame(mean_delta=mean(melt(temp_bot_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(temp_bot_2km, var='x_value')$value, na.rm = TRUE),
                         model="2km", water_range="bottom", variable="temp")
r6<-data.frame(mean_delta=mean(melt(temp_surf_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(temp_surf_2km, var='x_value')$value, na.rm = TRUE),
                          model="2km", water_range="surface", variable="temp")
r7<-data.frame(mean_delta=mean(melt(oxy_200_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(oxy_200_2km, var='x_value')$value, na.rm = TRUE),
                        model="2km", water_range="200m", variable="oxygen")
r8<-data.frame(mean_delta=mean(melt(oxy_bot_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(oxy_bot_2km, var='x_value')$value, na.rm = TRUE),
                        model="2km", water_range="bottom", variable="oxygen")
r9<-data.frame(mean_delta=mean(melt(oxy_surf_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(oxy_surf_2km, var='x_value')$value, na.rm = TRUE),
                         model="2km", water_range="surface", variable="oxygen")
r10<-data.frame(mean_delta=mean(melt(pH_200_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(pH_200_2km, var='x_value')$value, na.rm = TRUE),
               model="2km", water_range="200m", variable="pH")
r11<-data.frame(mean_delta=mean(melt(pH_bot_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(pH_bot_2km, var='x_value')$value, na.rm = TRUE),
               model="2km", water_range="bottom", variable="pH")
r12<-data.frame(mean_delta=mean(melt(pH_surf_2km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(pH_surf_2km, var='x_value')$value, na.rm = TRUE),
               model="2km", water_range="surface", variable="pH")

#combine dataframes
table_results_2km<-rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)


#melt into long data, and calculate mean delta and sd delta, prepare into dataframes
r1<-data.frame(mean_delta=mean(melt(CO2_200_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(CO2_200_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="200m", variable="CO2")
r2<-data.frame(mean_delta=mean(melt(CO2_bot_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(CO2_bot_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="bottom", variable="CO2")
r3<-data.frame(mean_delta=mean(melt(CO2_surf_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(CO2_surf_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="surface", variable="CO2")
r4<-data.frame(mean_delta=mean(melt(temp_200_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(temp_200_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="200m", variable="temp")
r5<-data.frame(mean_delta=mean(melt(temp_bot_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(temp_bot_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="bottom", variable="temp")
r6<-data.frame(mean_delta=mean(melt(temp_surf_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(temp_surf_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="surface", variable="temp")
r7<-data.frame(mean_delta=mean(melt(oxy_200_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(oxy_200_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="200m", variable="oxygen")
r8<-data.frame(mean_delta=mean(melt(oxy_bot_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(oxy_bot_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="bottom", variable="oxygen")
r9<-data.frame(mean_delta=mean(melt(oxy_surf_12km, var='x_value')$value, na.rm = TRUE), 
               sd_delta=sd(melt(oxy_surf_12km, var='x_value')$value, na.rm = TRUE),
               model="12km", water_range="surface", variable="oxygen")
r10<-data.frame(mean_delta=mean(melt(pH_200_12km, var='x_value')$value, na.rm = TRUE), 
                sd_delta=sd(melt(pH_200_12km, var='x_value')$value, na.rm = TRUE),
                model="12km", water_range="200m", variable="pH")
r11<-data.frame(mean_delta=mean(melt(pH_bot_12km, var='x_value')$value, na.rm = TRUE), 
                sd_delta=sd(melt(pH_bot_12km, var='x_value')$value, na.rm = TRUE),
                model="12km", water_range="bottom", variable="pH")
r12<-data.frame(mean_delta=mean(melt(pH_surf_12km, var='x_value')$value, na.rm = TRUE), 
                sd_delta=sd(melt(pH_surf_12km, var='x_value')$value, na.rm = TRUE),
                model="12km", water_range="surface", variable="pH")
          
#combine dataframes
table_results_12km<-rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)

#combine into one table
table_delta_masked<-rbind(table_results_2km, table_results_12km)



#values are still in units of mmol/m^3.  To convert to ml/l, multiply by this:
conv_oxy<-(1000/1026)*(1+26.8/1000)*22.414/1000

table_delta_masked<-table_delta_masked %>%
  mutate(mean_delta=ifelse(variable=="oxygen", mean_delta*conv_oxy, mean_delta))

write_csv(table_delta_masked, "processed_data/table_delta_masked.csv")

