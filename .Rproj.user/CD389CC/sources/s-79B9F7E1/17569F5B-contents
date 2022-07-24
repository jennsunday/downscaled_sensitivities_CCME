#goal: get model-driven relevant range for each stressor
#approach: read in full grided data of present and future values of each stressor, all model zones
#subset to just the shelf region
#combine and extract the maximum and minimum values

# load libraries ----------------------------------------------------------
library(tidyverse)
library(reshape2)

#read in 500m mask
little_shelf_mask<-read_csv("raw_data/new_downscaled_climate_data/mask_500m_2km.csv", col_names=F)
big_shelf_mask<-read_csv("raw_data/new_downscaled_climate_data/mask_500m_12km.csv", col_names=F)

#temp - just surface
present_temp_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_present_temp_surf.csv",
                                 col_names=F)
future_temp_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_future_temp_surf.csv",
                                col_names=F)
#CO2 
present_CO2_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_present_pCO2_surf.csv",
                                 col_names=F)
future_CO2_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_future_pCO2_surf.csv",
                                col_names=F)
present_CO2_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_present_pCO2_bot.csv",
                                col_names=F)
future_CO2_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_future_pCO2_bot.csv",
                               col_names=F)

#oxy
present_oxy_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_present_oxy_surf.csv",
                                col_names=F)
future_oxy_surf_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_future_oxy_surf.csv",
                               col_names=F)
present_oxy_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_present_oxy_bot.csv",
                               col_names=F)
future_oxy_bot_12km<-read_csv("raw_data/new_downscaled_climate_data/12km_future_oxy_bot.csv",
                              col_names=F)

#apply the mask to each
present_temp_surf_12km[!big_shelf_mask==1] <- NA
future_temp_surf_12km[!big_shelf_mask==1] <- NA
present_CO2_surf_12km[!big_shelf_mask==1] <- NA
future_CO2_surf_12km[!big_shelf_mask==1] <- NA
present_CO2_bot_12km[!big_shelf_mask==1] <- NA
future_CO2_bot_12km[!big_shelf_mask==1] <- NA
present_oxy_surf_12km[!big_shelf_mask==1] <- NA
future_oxy_surf_12km[!big_shelf_mask==1] <- NA
present_oxy_bot_12km[!big_shelf_mask==1] <- NA
future_oxy_bot_12km[!big_shelf_mask==1] <- NA

#reshape this into long
present_temp_surf_12km.df<-melt(present_temp_surf_12km) %>%
  mutate(lat=rep(1:dim(present_temp_surf_12km)[1], dim(present_temp_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

future_temp_surf_12km.df<-melt(future_temp_surf_12km) %>%
  mutate(lat=rep(1:dim(future_temp_surf_12km)[1], dim(future_temp_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

#CO2
present_CO2_surf_12km.df<-melt(present_CO2_surf_12km) %>%
  mutate(lat=rep(1:dim(present_CO2_surf_12km)[1], dim(present_CO2_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

future_CO2_surf_12km.df<-melt(future_CO2_surf_12km) %>%
  mutate(lat=rep(1:dim(future_CO2_surf_12km)[1], dim(future_CO2_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

present_CO2_bot_12km.df<-melt(present_CO2_bot_12km) %>%
  mutate(lat=rep(1:dim(present_CO2_bot_12km)[1], dim(present_CO2_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

future_CO2_bot_12km.df<-melt(future_CO2_bot_12km) %>%
  mutate(lat=rep(1:dim(future_CO2_bot_12km)[1], dim(future_CO2_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))


#oxy
present_oxy_surf_12km.df<-melt(present_oxy_surf_12km) %>%
  mutate(lat=rep(1:dim(present_oxy_surf_12km)[1], dim(present_oxy_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

future_oxy_surf_12km.df<-melt(future_oxy_surf_12km) %>%
  mutate(lat=rep(1:dim(future_oxy_surf_12km)[1], dim(future_oxy_surf_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

present_oxy_bot_12km.df<-melt(present_oxy_bot_12km) %>%
  mutate(lat=rep(1:dim(present_oxy_bot_12km)[1], dim(present_oxy_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))

future_oxy_bot_12km.df<-melt(future_oxy_bot_12km) %>%
  mutate(lat=rep(1:dim(future_oxy_bot_12km)[1], dim(future_oxy_bot_12km)[2])) %>%
  separate(variable, c(NA, "long"), sep = "X", remove = TRUE) %>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(latlong=paste(long, lat, sep="_"))



combined_temp<-rbind(present_temp_surf_12km.df, future_temp_surf_12km.df)
combined_CO2<-rbind(present_CO2_surf_12km.df, future_CO2_surf_12km.df,
                    present_CO2_bot_12km.df, future_CO2_bot_12km.df)
combined_oxy<-rbind(present_oxy_surf_12km.df, future_oxy_surf_12km.df,
                    present_oxy_bot_12km.df, future_oxy_bot_12km.df)

#oxygen conversion from mmol/m3 to ml/l
conv_oxy<-(1000/1026)*(1+26.8/1000)*22.414/1000

max_temp<-max(combined_temp$value, na.rm=T)
min_temp<-min(combined_temp$value, na.rm=T)
max_CO2<-max(combined_CO2$value, na.rm=T)
min_CO2<-min(combined_CO2$value, na.rm=T)
max_oxy<-max(combined_oxy$value, na.rm=T)*conv_oxy
min_oxy<-min(combined_oxy$value, na.rm=T)*conv_oxy

#set range of relevant values for each stressor
#nb: did not get all present and future values for salinity and pH, so entering a reasonable range manually

relevant_window<-data.frame(temperature=c(min_temp,max_temp), salinity=c(29,33.5),
                            oxygen=c(min_oxy,max_oxy), CO2=c(min_CO2,max_CO2), pH=c(6,10), limit=c("min", "max"))
relevant_window<-melt(relevant_window, id="limit", variable.name = "treatment_var")
relevant_window_cast<-dcast(relevant_window, treatment_var~limit)
write_csv(relevant_window_cast, "processed_data/relevant_window_cast.csv")
