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
data<-read.csv(file="direct_responses_data_dec.csv", header=T)
dim(data)

data$English_Name
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

pdf("allthedata.pdf", 5, 10)
par(mfrow=c(length(unique(data$English_Name)),4))
#par(mfrow=c(5,4))
par(mar=c(2,2,0,0), oma=c(5,0,4, 6))
for(j in 1:length(unique(data$English_Name))){
#for(j in 1:5){
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
  points(partdata$rel_response~partdata[,treat_col], col=as.numeric(partdata$response_type), 
         lty=as.numeric(partdata$Life_stage_category), type="l") #add lines
  segments(partdata[,treat_col], partdata$rel_upperSE, partdata[,treat_col], partdata$rel_lowerSE, 
           col=as.numeric(partdata$response_type))
}
}
}

#add treatment titles to columns
mtext(paste(unique(data$treatment_var2)[1]), 3, 1, outer=T, adj=0.1)
mtext(paste(unique(data$treatment_var2)[2]), 3, 1, outer=T, adj=0.4)
mtext(paste(unique(data$treatment_var2)[3]), 3, 1, outer=T, adj=0.7)
mtext(paste(unique(data$treatment_var2)[4]), 3, 1, outer=T, adj=1)


mtext(paste(unique(data$English_Name)[1]), 4, 1, outer=T, padj=-9.5, las=1)
mtext(paste(unique(data$English_Name)[2]), 4, 1, outer=T, padj=-6, las=1)
mtext(paste(unique(data$English_Name)[3]), 4, 1, outer=T, padj=-2, las=1)
mtext(paste(unique(data$English_Name)[4]), 4, 1, outer=T, padj=1.2, las=1)
mtext(paste(unique(data$English_Name)[5]), 4, 1, outer=T, padj=4.5, las=1)
mtext(paste(unique(data$English_Name)[6]), 4, 1, outer=T, padj=8, las=1)
mtext(paste(unique(data$English_Name)[7]), 4, 1, outer=T, padj=11, las=1)
mtext(paste(unique(data$English_Name)[8]), 4, 1, outer=T, padj=14, las=1)
mtext(paste(unique(data$English_Name)[9]), 4, 1, outer=T, padj=17, las=1)
mtext(paste(unique(data$English_Name)[10]), 4, 1, outer=T, padj=20, las=1)

# turn off clipping:
par(xpd=NA)
legend(0, -0.2, col=1:length(levels(partdata$response_type)), levels(partdata$response_type), bty="n", pch=1)
par(xpd=FALSE)

dev.off()
#keep for methods: 
#colour code type of response (e.g. metabolic rate, movement, consumption, growth, survival)
#make dashed lines for propagule/gamete response
#add error bars (convert 95CI to SE, make error relative to max)

#To do:
#clean up figure
#make gams for regression data
#figure out margin labels
#weigh line by sample size (figure out really high samples sizes for proportion data)
