# load libraries ----------------------------------------------------------
library(nlme) #for mixed models
library(MuMIn) # to do model averaging
library(plyr)
library(mgcv)

# load functions
vector.is.empty <- function(x) return(dim(x)[1] ==0 )
vector.is.not.empty <- function(x) return(dim(x)[1] >0 )

#### Step 2: read in the data
setwd('/Users/Jennifer_Sunday/Dropbox/UW_Schmidt/data/')
data<-read.csv(file="direct_responses_data.csv", header=T)
dim(data)


#make a unique identifier for each author, pub_year, and response_variable combination
data$study<-as.factor(paste(data$Author, data$Pub_Year, data$Species, data$Life_stage, data$Response_variable, data$treatment_var))

# make upper and lower SE, convert 95CI to SE for some data
data$lowerSE<-ifelse(is.na(data$LCI_response), 
                          data$response-data$SE_response, 
                     data$response-((data$response-data$LCI_response)/1.96))
data$upperSE<-ifelse(is.na(data$UCI_response), 
                     data$response+data$SE_response, 
                     data$response+((data$UCI_response-data$response)/1.96))
head(data[,c(26, 45, 46)]) #check some data


#calculate something within each study
data<-ddply(data, "study", mutate, max_response=ifelse(is.na(upperSE), max(response), max(upperSE)), 
            rel_response=response/max_response, 
            rel_upperSE=upperSE/max_response, 
            rel_lowerSE=lowerSE/max_response) #add columns of max response and response relative to max

#subset by species
#variable
#study
names(data)
par(mfrow=c(length(unique(data$English_Name)),4))
par(mar=c(2,2,0,0), oma=c(5,0,4,15))
for(j in 1:length(unique(data$English_Name))){
speciesdata<-subset(data, data$English_Name==unique(data$English_Name)[j]) #subset to every species

for (k in 1:length(unique(data$treatment_var))){
speciestreatment<-subset(speciesdata, 
                speciesdata$treatment_var==unique(data$treatment_var)[k]) #subset species to every treatmnet
treat_col<-which(names(data)==unique(data$treatment_var2)[k]) #identify the column for each treatment
if(vector.is.empty(speciestreatment)) #if vector is empty, make a dummy plot
plot(c(0,1),c(0,1), type="n", ylim=c(-0.1, 1.1))

#if(vector.is.empty(speciestreatment)) #if vector is empty, label with "no data"
#text(0.5, 0.5, labels="no data")
if(vector.is.not.empty(speciestreatment)) #if vector is not empty, make box and plot data
plot(speciestreatment$rel_response~speciestreatment[,treat_col], type="n", ylim=c(-0.1, 1.1)) #empty plot for every species/treatment

for (i in 1:length(unique(speciestreatment$study))){ #subset species&treatment by unique study
  partdata<-subset(speciestreatment, speciestreatment$study==unique(speciestreatment$study)[i])
  points(partdata$rel_response~partdata[,treat_col], col=as.numeric(partdata$response_type)) #plot points
  if(partdata$study_design[1] %in% c("anova", "multi-level anova")) #if these kind of data
    points(partdata$rel_response~partdata[,treat_col], col=as.numeric(partdata$response_type), 
         lty=as.numeric(partdata$Life_stage_category), type="l") #add lines
  if(partdata$study_design[1] %in% c("anova", "multi-level anova")) #if these kind of data
    segments(partdata[,treat_col], partdata$rel_upperSE, partdata[,treat_col], partdata$rel_lowerSE, 
           col=as.numeric(partdata$response_type)) #add error bars
  if(partdata$study_design[1] %in% c("regression")) #if these kind of data
    gamobj<-eval(parse(text=paste("gam(rel_response~s(", 
                                  names(data)[treat_col], 
                                  ", k=dim(partdata)[1]-3), data=partdata)")))
  if(partdata$study_design[1] %in% c("regression")) #if these kind of data
    pred <- eval(parse(text=paste("data.frame(", 
                                  names(data)[treat_col],
                                  " = seq(min(partdata[,treat_col]),
                                  max(partdata[,treat_col]),length.out=20))")))
  if(partdata$study_design[1] %in% c("regression")) #if these kind of data
    predictedresponse<-predict(gamobj, newdata = pred, se=TRUE)
  if(partdata$study_design[1] %in% c("regression")) #if these kind of data
  lines(pred[,1], predictedresponse$fit, col=as.numeric(partdata$response_type))
  if(partdata$study_design[1] %in% c("regression")) #if these kind of data
    polygon(c(pred[,1], rev(pred[,1])), 
            c(predictedresponse$fit+predictedresponse$se.fit, 
              rev(predictedresponse$fit-predictedresponse$se.fit)), 
            col=adjustcolor(as.numeric(partdata$response_type), alpha.f = 0.3) , border=NA)
}
}
}


#
#
#
#examples for Terrie meeting
#
#
#

pdf("examplegamm_salmon.pdf", 6, 4)
salmontemp<-subset(data, data$English_Name=="Pink Salmon"
                   &data$treatment_var=="temperature") #subset to salmon and temp

par(mfrow=c(1,1))
par(mar=c(2,2,0,0), oma=c(2,2,0.5,0.5))
gamobj<-gam(rel_response~study+s(temperature)+s(temperature, by=study), data=salmontemp)


plot(salmontemp$rel_response~salmontemp$temperature, col=as.numeric(salmontemp$response_type), ylim=c(-0.1, 1.1))
for (i in 1:length(levels(salmontemp$study))){
  pred <- data.frame(temperature = 1:35, study=levels(salmontemp$study)[i])
predictedresponse<-predict(gamobj, newdata = pred, se=TRUE)
lines(pred$temperature, predictedresponse$fit, 
      col=as.numeric(salmontemp$response_type[salmontemp$study==levels(salmontemp$study)[i]][1]))
polygon(c(pred$temperature, rev(pred$temperature)), 
        c(predictedresponse$fit+predictedresponse$se.fit, 
          rev(predictedresponse$fit-predictedresponse$se.fit)), 
            col=adjustcolor(as.numeric(salmontemp$response_type[salmontemp$study==levels(salmontemp$study)[i]][1]), alpha.f = 0.3), 
              border=NA)
}

gammobj<-gamm(rel_response~s(temperature), data=salmontemp, random=list(study=~1))
predictedresponse<-predict(gammobj, newdata = pred, se=TRUE, levels=1)
lines(pred$temperature, predictedresponse$fit, col=1)
polygon(c(pred$temperature, rev(pred$temperature)), 
        c(predictedresponse$fit+predictedresponse$se.fit, 
          rev(predictedresponse$fit-predictedresponse$se.fit)), col=adjustcolor(1, alpha.f = 0.3) , border=NA)
dev.off()

#
#
pdf("examplegamm_urchinoxy.pdf", 6, 4)
urchinoxy<-subset(data, data$English_Name=="Urchin_ Strongylocentrotus franciscanus"
                   &data$treatment_var=="oxygen") #subset to salmon and temp

par(mfrow=c(1,1))
par(mar=c(2,2,0,0), oma=c(2,2,0.5,0.5))
gamobj<-gam(rel_response~s(oxygen), data=urchinoxy)


plot(urchinoxy$rel_response~urchinoxy$oxygen, col=as.numeric(urchinoxy$response_type), ylim=c(-0.1, 1.1))
for (i in 1:length(levels(urchinoxy$study))){
  pred <- data.frame(oxygen = seq(min(urchinoxy$oxygen), max(urchinoxy$oxygen), length.out=30), study=levels(urchinoxy$study)[i])
  predictedresponse<-predict(gamobj, newdata = pred, se=TRUE)
  lines(pred$oxygen, predictedresponse$fit, 
        col=as.numeric(urchinoxy$response_type[urchinoxy$study==unique(urchinoxy$study)[i]][1]))
  polygon(c(pred$oxygen, rev(pred$oxygen)), 
          c(predictedresponse$fit+predictedresponse$se.fit, 
            rev(predictedresponse$fit-predictedresponse$se.fit)), 
          col=adjustcolor(as.numeric(urchinoxy$response_type[urchinoxy$study==unique(urchinoxy$study)[i]][1]), alpha.f = 0.3), 
          border=NA)
}
dev.off()
#
#
pdf("examplegamm_craboxy.pdf", 6, 4)
craboxy<-subset(data, data$English_Name=="Crab_ Dungeness"
                  &data$treatment_var=="oxygen") #subset to salmon and temp
names(craboxy)
#simulate individual observations
simdata<-data.frame(simoutput = seq(0, 0, length.out=sum(craboxy$sample_size)))
for (i in 1:length(craboxy$treatment_var)){
  simdata[c(i)]<-rnorm(craboxy$sample_size[i], mean=craboxy$rel_response[i], sd=craboxy$rel_upperSE[i]-craboxy$rel_response[i])
  
}

, "treatment_var", rnorm(25, mean=rel_response, sd=0.2))

?rnorm
par(mfrow=c(1,1))
par(mar=c(2,2,0,0), oma=c(2,2,0.5,0.5))
gamobj<-gam(rel_response~s(oxygen), data=urchinoxy)


plot(urchinoxy$rel_response~urchinoxy$oxygen, col=as.numeric(urchinoxy$response_type), ylim=c(-0.1, 1.1))
for (i in 1:length(levels(urchinoxy$study))){
  pred <- data.frame(oxygen = seq(min(urchinoxy$oxygen), max(urchinoxy$oxygen), length.out=30), study=levels(urchinoxy$study)[i])
  predictedresponse<-predict(gamobj, newdata = pred, se=TRUE)
  lines(pred$oxygen, predictedresponse$fit, 
        col=as.numeric(urchinoxy$response_type[urchinoxy$study==unique(urchinoxy$study)[i]][1]))
  polygon(c(pred$oxygen, rev(pred$oxygen)), 
          c(predictedresponse$fit+predictedresponse$se.fit, 
            rev(predictedresponse$fit-predictedresponse$se.fit)), 
          col=adjustcolor(as.numeric(urchinoxy$response_type[urchinoxy$study==unique(urchinoxy$study)[i]][1]), alpha.f = 0.3), 
          border=NA)
}
dev.off()



speciestreatment


#add treatment titles to columns
mtext(paste(unique(data$treatment_var)[1]), 3, 1, outer=T, adj=0.1)
mtext(paste(unique(data$treatment_var)[2]), 3, 1, outer=T, adj=0.4)
mtext(paste(unique(data$treatment_var)[3]), 3, 1, outer=T, adj=0.65)
mtext(paste(unique(data$treatment_var)[4]), 3, 1, outer=T, adj=0.95)


mtext(paste(unique(data$English_Name)[1]), 4, 1, outer=T, padj=-23, las=1)
mtext(paste(unique(data$English_Name)[2]), 4, 1, outer=T, padj=-15, las=1)
mtext(paste(unique(data$English_Name)[3]), 4, 1, outer=T, padj=-5, las=1)
mtext(paste(unique(data$English_Name)[4]), 4, 1, outer=T, padj=4, las=1)
mtext(paste(unique(data$English_Name)[5]), 4, 1, outer=T, padj=13, las=1)
mtext(paste(unique(data$English_Name)[6]), 4, 1, outer=T, padj=22, las=1)

# turn off clipping:
par(xpd=NA)
legend(0, -0.2, col=1:length(levels(partdata$response_type)), levels(partdata$response_type), bty="n", pch=1)
par(xpd=FALSE)


#keep for methods: 
#colour code type of response (e.g. metabolic rate, movement, consumption, growth, survival)
#make dashed lines for propagule/gamete response
#add error bars (convert 95CI to SE, make error relative to max)

#To do:
#clean up figure
#make gams for regression data
#figure out margin labels
#weigh line by sample size (figure out really high samples sizes for proportion data)

#notes from Joey
#fit gams within studies on original axis
#predict mean response at "reference" abiotic value
#standardize data to response at reference value
#fit multi-species GAM
#
#Alistair Hobbs risk assement sensitivity - exposure - adaptatbility
#"risk" is distance from origin
