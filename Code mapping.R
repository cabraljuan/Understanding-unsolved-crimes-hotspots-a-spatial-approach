
###########################################################################
###########################################################################
###                                                                     ###
###                      UNSOLVED CRIMES HOTSPOTS:                      ###
###                       PREPARING DATA FOR QGIS                       ###
###                                                                     ###
###########################################################################
###########################################################################
# Author: Juan Andrés Cabral
# Contact: juan.drs.c@gmail.com

# This file is intended to prepare the data to be used in QGIS.


# Path
rm(list=ls())
path <- "G:/Facultad/San andres/Econometría espacial/working paper"
setwd(path)

# Load data
data<-read.csv("washington/homicide-data.csv")


# New variables
# ID
data$ID <- seq.int(nrow(data)) # ID variable
# Open or no arrest variable, unsolved cases
data$open_or_no_arrest<-ifelse(data$disposition=="Closed without arrest"|
                                 data$disposition=="Open/No arrest",1,0) 
# Closed without arrest
data$closed_without_arrest<-ifelse(data$disposition=="Closed without arrest",1,0)
# Solved cases
data$arrest<-ifelse(data$disposition=="Closed by arrest",1,0)
data$count<-1

# Save data
write.csv(data,"homicide-data2.csv")


unique(data$state)


#################################################################
##                       STATES datasets                       ##
#################################################################



statesobs<-data.frame()
for (states in unique(data$state)){
  rows<-nrow(data[data$state==states,])
  newdata<-data.frame(obs=rows,state=states)
  statesobs<-rbind(statesobs,newdata)
}

statesobs<-statesobs[with(statesobs,order(obs)),]

# California
dataca<-data[data$state=="CA",]
write.csv(dataca,"homicide-data2CA.csv")

# Texas
datatx<-data[data$state=="TX",]
write.csv(datatx,"homicide-data2TX.csv")


# ILLINOIS
datail<-data[data$state=="IL",]
write.csv(datail,"homicide-data2IL.csv")




#################################################################
##                       CITIES DATASETS                       ##
#################################################################
citiesobs<-data.frame()
for (cities in unique(data$city)){
  rows<-nrow(data[data$city==cities,])
  prctunsolved<-nrow(data[data$open_or_no_arrest==1&
                           data$city==cities,])/nrow(data[data$city==cities,])
  prctblack<-nrow(data[data$victim_race=="Black"&
                            data$city==cities,])/nrow(data[data$city==cities,])
  newdata<-data.frame(city=cities,obs=rows,percent_unsolved=prctunsolved,blackvictim=prctblack)
  citiesobs<-rbind(citiesobs,newdata)
}


citiesobs<-citiesobs[with(citiesobs,order(obs)),]
rownames(citiesobs) <- NULL
citiesobs<-citiesobs[10:50,]
stargazer(citiesobs,summary = FALSE,title="Descriptive statistics for cities",label="descc")


# Chicago
datachicago<-data[data$city=="Chicago",]
write.csv(datachicago,"homicide-data2chicago.csv")


# Chicago unsolved
datachicago<-data[data$city=="Chicago",]
datachicago<-data[data$open_or_no_arrest=="1",]

write.csv(datachicago,"homicide-data2chicagounsolved.csv")


# Philadelphia
dataphil<-data[data$city=="Philadelphia",]
write.csv(dataphil,"homicide-data2philadelphia.csv")
