# load libraries ----------------------------------------------------------
library(nlme) #for mixed models
library(MuMIn) # to do model averaging
library(plyr)

#### Step 2: read in the data
setwd('/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/')
data<-read.csv(file="direct_responses_data.csv", header=T)
dim(data)
names(data)
data$Response_variable

#make a unique identifier for each author, pub_year, and response_variable combination
data$study<-paste(data$Author, data$Pub_Year, data$Species, data$Life_stage, data$Response_variable, data$treatment_var)

data<-ddply(data, "study", mutate, max_response=max(response), rel_response=response/max(response))

#subset by species
#dungeness
#temp
par(mfrow=c(2,2))
dungenessdata<-subset(data, data$English_Name=="Crab_ Dungeness")
duntemp<-subset(dungenessdata, 
                dungenessdata$treatment_var %in% 
                  c("temperature"))
with(duntemp, plot(rel_response~temperature, type="n"))
for (i in 1:length(unique(duntemp$study))){
  partdata<-subset(duntemp, duntemp$study==unique(duntemp$study)[i])
  with(partdata, points(rel_response~temperature, col=i))
  with(partdata, points(rel_response~temperature, col=i, type="l"))
  }

#dungeness
#sal
dunsal<-subset(dungenessdata, 
                dungenessdata$treatment_var %in% 
                  c("salinity"))
with(dunsal, plot(rel_response~salinity, type="n"))
for (i in 1:length(unique(dunsal$study))){
  partdata<-subset(dunsal, dunsal$study==unique(dunsal$study)[i])
  with(partdata, points(rel_response~salinity, col=i))
  with(partdata, points(rel_response~salinity, col=i, type="l"))
}

#dungeness
#pH
dunpH<-subset(dungenessdata, 
               dungenessdata$treatment_var %in% 
                 c("pH"))
with(dunpH, plot(rel_response~pH, type="n"))
for (i in 1:length(unique(dunpH$study))){
  partdata<-subset(dunpH, dunpH$study==unique(dunpH$study)[i])
  with(partdata, points(rel_response~pH, col=i))
  with(partdata, points(rel_response~pH, col=i, type="l"))
}

#dungeness
#oxy
dunoxy<-subset(dungenessdata, 
              dungenessdata$treatment_var %in% 
                c("oxygen"))
with(dunoxy, plot(rel_response~oxygen, type="n"))
for (i in 1:length(unique(dunoxy$study))){
  partdata<-subset(dunoxy, dunoxy$study==unique(dunoxy$study)[i])
  with(partdata, points(rel_response~oxygen, col=i))
  with(partdata, points(rel_response~oxygen, col=i, type="l"))
}




with(subset(pHdata, pHdata$study==pHdata$study[1]), plot(response~pH, xlim=c(7, 9), ylim=c(-10, 200)))
abline(lm(response~pH))
for (i in 1:length(unique_pH)){
  with(subset(pHdata, pHdata$study==pHdata$study[i]), points(response~pH))
  with(subset(pHdata, pHdata$study==pHdata$study[i]), abline(lm(response~pH)))
}
