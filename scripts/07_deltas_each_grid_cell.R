# goal: for every grid cell, depth zone, environmental variable, 
# and model domain, get nodel-prejected delta of each response 
# into long format

#librarieslibrary(nlme) 
#install.packages("PNWColors")
library(tidyverse)
library(broom)
library(reshape2)
library(gridExtra)



#read in model-projected delta values from 1.5km model (labelled 2km)
CO2_200_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_pCO2_200m.csv", col_names=F)
CO2_bot_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_pCO2_bot.csv", col_names=F)
CO2_surf_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_pCO2_surf.csv", col_names=F)
temp_200_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_temp_200m.csv", col_names=F)
temp_bot_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_temp_bot.csv", col_names=F)
temp_surf_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_temp_surf.csv", col_names=F)
oxy_200_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_oxy_200m.csv", col_names=F)
oxy_bot_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_oxy_bot.csv", col_names=F)
oxy_surf_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_oxy_surf.csv", col_names=F)
pH_200_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_pH_200m.csv", col_names=F)
pH_bot_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_pH_bot.csv", col_names=F)
pH_surf_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_delta_pH_surf.csv", col_names=F)
depth_bot_2km<-read_csv("raw_data/new_downscaled_climate_data/2km_bot_depth.csv", col_names=F)

#reshape these into long dataframes
#200 layer CO2
df.CO2_200_2km<-melt(CO2_200_2km) %>%
  mutate(lat=rep(1:dim(CO2_200_2km)[1], dim(CO2_200_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="CO2", modelzone="200m")

df.CO2_bot_2km<-melt(CO2_bot_2km) %>%
  mutate(lat=rep(1:dim(CO2_bot_2km)[1], dim(CO2_bot_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))%>%
  rename(delta=value) %>%
  mutate(treatment_var="CO2", modelzone="bot")


df.CO2_surf_2km<-melt(CO2_surf_2km) %>%
  mutate(lat=rep(1:dim(CO2_surf_2km)[1], dim(CO2_surf_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="CO2", modelzone="surf")

df.temp_200_2km<-melt(temp_200_2km) %>%
  mutate(lat=rep(1:dim(temp_200_2km)[1], dim(temp_200_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="temp", modelzone="200m")

df.temp_bot_2km<-melt(temp_bot_2km) %>%
  mutate(lat=rep(1:dim(temp_bot_2km)[1], dim(temp_bot_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="temp", modelzone="bot")

df.temp_surf_2km<-melt(temp_surf_2km) %>%
  mutate(lat=rep(1:dim(temp_surf_2km)[1], dim(temp_surf_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="temp", modelzone="surf")

df.oxy_200_2km<-melt(oxy_200_2km) %>%
  mutate(lat=rep(1:dim(oxy_200_2km)[1], dim(oxy_200_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="oxy", modelzone="200m")

df.oxy_surf_2km<-melt(oxy_surf_2km) %>%
  mutate(lat=rep(1:dim(oxy_surf_2km)[1], dim(oxy_surf_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="oxy", modelzone="surf")

df.oxy_bot_2km<-melt(oxy_bot_2km) %>%
  mutate(lat=rep(1:dim(oxy_bot_2km)[1], dim(oxy_bot_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="oxy", modelzone="bot")

df.pH_200_2km<-melt(pH_200_2km) %>%
  mutate(lat=rep(1:dim(pH_200_2km)[1], dim(pH_200_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="pH", modelzone="200m")

df.pH_bot_2km<-melt(pH_bot_2km) %>%
  mutate(lat=rep(1:dim(pH_bot_2km)[1], dim(pH_bot_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="pH", modelzone="bot")

df.pH_surf_2km<-melt(pH_surf_2km) %>%
  mutate(lat=rep(1:dim(pH_surf_2km)[1], dim(pH_surf_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="pH", modelzone="surf")



df.depth_bot_2km<-melt(depth_bot_2km) %>%
  mutate(lat=rep(1:dim(depth_bot_2km)[1], dim(depth_bot_2km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(depth=value) 

#oxygen conversion from mmol/m3 to ml/l
conv_oxy<-(1000/1026)*(1+26.8/1000)*22.414/1000
df.oxy_200_2km$delta<-df.oxy_200_2km$delta #don't convert this one because it was already in ml/l
df.oxy_bot_2km$delta<-df.oxy_bot_2km$delta*conv_oxy
df.oxy_surf_2km$delta<-df.oxy_surf_2km$delta*conv_oxy

all_deltas_2km<-rbind(
     df.CO2_200_2km, 
     df.CO2_bot_2km, 
     df.CO2_surf_2km, 
     df.temp_200_2km, 
     df.temp_bot_2km, 
     df.temp_surf_2km, 
     df.oxy_200_2km, 
     df.oxy_bot_2km, 
     df.oxy_surf_2km, 
     df.pH_200_2km, 
     df.pH_bot_2km, 
     df.pH_surf_2km)

#get lat longs of land
coastline_mask_2km<-all_deltas_2km %>%
  filter(delta=="NaN",
         treatment_var=="CO2",
         modelzone=="bot") %>%
  select(lat, long)

write_csv(all_deltas_2km, "processed_data/all_deltas_2km.csv")
write_csv(coastline_mask_2km, "processed_data/coastline_mask_2km.csv")
write_csv(df.depth_bot_2km, "processed_data/depth_2km.csv")
#repeat for 12km

#read in model-projected delta values from 1.5km model
CO2_200_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pCO2_200m.csv", col_names=F)
CO2_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pCO2_bot.csv", col_names=F)
CO2_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pCO2_surf.csv", col_names=F)
temp_200_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_temp_200m.csv", col_names=F)
temp_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_temp_bot.csv", col_names=F)
temp_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_temp_surf.csv", col_names=F)
oxy_200_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_oxy_200m.csv", col_names=F)
oxy_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_oxy_bot.csv", col_names=F)
oxy_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_oxy_surf.csv", col_names=F)
pH_200_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pH_200m.csv", col_names=F)
pH_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pH_bot.csv", col_names=F)
pH_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_delta_pH_surf.csv", col_names=F)
depth_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_bot_depth.csv", col_names=F)

#reshape these into long dataframes
df.CO2_200_12km<-melt(CO2_200_12km) %>%
  mutate(lat=rep(1:dim(CO2_200_12km)[1], dim(CO2_200_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="CO2", modelzone="200m")

df.CO2_bot_12km<-melt(CO2_bot_12km) %>%
  mutate(lat=rep(1:dim(CO2_bot_12km)[1], dim(CO2_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))%>%
  rename(delta=value) %>%
  mutate(treatment_var="CO2", modelzone="bot")


df.CO2_surf_12km<-melt(CO2_surf_12km) %>%
  mutate(lat=rep(1:dim(CO2_surf_12km)[1], dim(CO2_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="CO2", modelzone="surf")

df.temp_200_12km<-melt(temp_200_12km) %>%
  mutate(lat=rep(1:dim(temp_200_12km)[1], dim(temp_200_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="temp", modelzone="200m")

df.temp_bot_12km<-melt(temp_bot_12km) %>%
  mutate(lat=rep(1:dim(temp_bot_12km)[1], dim(temp_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="temp", modelzone="bot")

df.temp_surf_12km<-melt(temp_surf_12km) %>%
  mutate(lat=rep(1:dim(temp_surf_12km)[1], dim(temp_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="temp", modelzone="surf")

df.oxy_200_12km<-melt(oxy_200_12km) %>%
  mutate(lat=rep(1:dim(oxy_200_12km)[1], dim(oxy_200_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="oxy", modelzone="200m")

df.oxy_surf_12km<-melt(oxy_surf_12km) %>%
  mutate(lat=rep(1:dim(oxy_surf_12km)[1], dim(oxy_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="oxy", modelzone="surf")

df.oxy_bot_12km<-melt(oxy_bot_12km) %>%
  mutate(lat=rep(1:dim(oxy_bot_12km)[1], dim(oxy_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="oxy", modelzone="bot")

df.pH_200_12km<-melt(pH_200_12km) %>%
  mutate(lat=rep(1:dim(pH_200_12km)[1], dim(pH_200_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="pH", modelzone="200m")

df.pH_bot_12km<-melt(pH_bot_12km) %>%
  mutate(lat=rep(1:dim(pH_bot_12km)[1], dim(pH_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="pH", modelzone="bot")

df.pH_surf_12km<-melt(pH_surf_12km) %>%
  mutate(lat=rep(1:dim(pH_surf_12km)[1], dim(pH_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(delta=value) %>%
  mutate(treatment_var="pH", modelzone="surf")

df.depth_bot_12km<-melt(depth_bot_12km) %>%
  mutate(lat=rep(1:dim(depth_bot_12km)[1], dim(depth_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_")) %>%
  rename(depth=value) 

#oxygen conversion from mmol/m3 to ml/l
conv_oxy<-(1000/1026)*(1+26.8/1000)*22.414/1000
df.oxy_200_12km$delta<-df.oxy_200_12km$delta*conv_oxy
df.oxy_bot_12km$delta<-df.oxy_bot_12km$delta*conv_oxy
df.oxy_surf_12km$delta<-df.oxy_surf_12km$delta*conv_oxy

all_deltas_12km<-rbind(df.CO2_200_12km, df.CO2_bot_12km, df.CO2_surf_12km, 
                      df.temp_200_12km, df.temp_bot_12km, df.temp_surf_12km, 
                      df.oxy_200_12km, df.oxy_bot_12km, df.oxy_surf_12km, 
                      df.pH_200_12km, df.pH_bot_12km, df.pH_surf_12km)


#get lat longs of land
coastline_mask<-all_deltas_12km %>%
  filter(delta=="NaN",
         treatment_var=="CO2",
         modelzone=="bot") %>%
  select(lat, long)


write_csv(all_deltas_12km, "processed_data/all_deltas_12km.csv")
write_csv(coastline_mask, "processed_data/coastline_mask_12km.csv")
write_csv(df.depth_bot_12km, "processed_data/depth_12km.csv")

