
############################################################################
############################################################################
###                                                                      ###
###                      UNSOLVED CRIMES HOTSPOTS:                       ###
###                                 MODELS                               ###
###                                                                      ###
############################################################################
############################################################################
# Author: Juan Andrés Cabral
# Contact: juan.drs.c@gmail.com





#################################################################
##                       Chicago dataset                       ##
#################################################################

rm(list=ls())

# Path
path <- "G:/Facultad/San andres/Econometría espacial/working paper"
setwd(path)

# Chicago
datachicago<-read.csv("homicide-data2chicago.csv")

# Drop unkown sex
nrow(datachicago[datachicago$victim_sex=="Unknown",]) # 7
datachicago<-datachicago[!datachicago$victim_sex=="Unknown",]

# Sex dummy
datachicago$male<-ifelse(datachicago$victim_sex=="Male",1,0)

# Drop unknown age
nrow(datachicago[datachicago$victim_age=="Unknown",]) # 5
datachicago<-datachicago[!datachicago$victim_age=="Unknown",]

datachicago$victim_age<-as.numeric(datachicago$victim_age)

# Race
datachicago$victim_race<-as.factor(datachicago$victim_race)

data<-datachicago

# Format
y<-as.data.frame(data$open_or_no_arrest)
x1<-as.data.frame(data$victim_race)
x2<-as.data.frame(data$victim_age)
x3<-as.data.frame(data$victim_sex)

library(spdep)

path <- "G:/Facultad/San andres/Econometría espacial/working paper/qgis/chicago dataset"
setwd(path)

w<-read.gwt2nb("inversedist.GWT")



# Dataset
dataset<-cbind(y,x1,x2)
names(dataset)<-c("y","x1","x2")



#################################################################
##                   De particular a general                   ##
#################################################################


# Regresión no-espacial
reg.ols <- lm(y ~ x1 + x2 , data=dataset)
summary(reg.ols)


# Test I de Moran
lm.morantest(reg.ols,w, alternative = "two.sided")
# Rechazo h0 -> hay dependencia espacial

# Tests LM's
lms <- lm.LMtests(reg.ols, w, test = "all")
tests <- t(sapply(lms, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tests) <- c("Test","df","p-valor")
printCoefmat(tests)
# stargazer(tests)
# Paso a SLX

##############
##  1) SLX  ##
##############
# M6
w_x1 <- lag.listw(w, dataset$x1)
w_x2 <- lag.listw(w, dataset$x2)
reg.slx <- lm(y ~ x1 + x2 + w_x1 + w_x2 , data=dataset)
summary(reg.slx)



###########################################################################
###########################################################################
###                                                                     ###
###                         CHICAGO AGGREGGATED                         ###
###                                                                     ###
###########################################################################
###########################################################################

rm(list=ls())

# Path
path <- "G:/Facultad/San andres/Econometría espacial/working paper/NEW"
setwd(path)

# load data
data<-read.csv("dataforR.csv")
names(data)


# Library
library(spdep)
library(stargazer)

# weight matrix
w<-read.gal("G:/Facultad/San andres/Econometría espacial/working paper/NEW/chicago_commpop/queen.gal")
w <- nb2listw(w)

#################################################################
##                   De particular a general                   ##
#################################################################


# OLS
reg.ols <- lm(crimepc ~  PER.C_COME+ PERCE_old+PERCE_unem+ Prc_pvrty+PERCE_crwd+
                prcwodplm, data=data)
summary(reg.ols)
stargazer(reg.ols,label="ols",title="OLS regression",covariate.labels =c("Per capita income","Percent aged under 18 or over 64","Percent aged 16+ unemployed","Percent households below poverty","Percent of housing crowded", "Percent aged 25+ without high school diploma") )


# Moran's I test
lm.morantest(reg.ols,w, alternative = "two.sided")
# Spatial dependence

# Tests LM's
lms <- lm.LMtests(reg.ols, w, test = "all")
tests <- t(sapply(lms, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tests) <- c("Test","df","p-value")
printCoefmat(tests)
# stargazer(tests)

##############
##  1) SLX  ##
##############
# M6
w_x1 <- lag.listw(w, data$PER.C_COME)
w_x2 <- lag.listw(w, data$PERCE_old)
w_x3 <- lag.listw(w, data$PERCE_unem)
w_x4 <- lag.listw(w, data$Prc_pvrty)
w_x5 <- lag.listw(w, data$PERCE_crwd)
w_x6 <- lag.listw(w, data$prcwodplm)
reg.slx <- lm(crimepc ~  PER.C_COME+ PERCE_old+PERCE_unem+ Prc_pvrty+PERCE_crwd+
                prcwodplm + w_x1 + w_x2 + w_x3 + w_x4+w_x5+w_x6 , data=data)
summary(reg.slx)


# Moran's I test
lm.morantest(reg.slx,w, alternative = "two.sided")
# Reject H0

# LM's test
lms <- lm.LMtests(reg.slx, w, test = "all")
tests <- t(sapply(lms, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tests) <- c("Test","df","p-value")
printCoefmat(tests)
#RLMlag  7.9292  1.0000  0.0049 we need to add \rho






##############
##  2) SEM  ##
##############
# M7 
reg.sem <- errorsarlm(crimepc ~  PER.C_COME+ PERCE_old+PERCE_unem+ Prc_pvrty+PERCE_crwd+
                        prcwodplm , data=data, w)
summary(reg.sem)
(LR_lambda=2*(reg.sem$LL - logLik(reg.ols)))
#'log Lik.' 12.26411 (df=8)






##############
##  3) SLM  ##
##############

reg.slm <- lagsarlm(crimepc ~  PER.C_COME+ PERCE_old+PERCE_unem+ Prc_pvrty+PERCE_crwd+
                      prcwodplm, data=data, w)
summary(reg.slm)
(LR_rho=2*(reg.slm$LL - logLik(reg.ols)))
#'log Lik.' 14.61659 (df=8)
stargazer(reg.slm,reg.ols,label = "slmols",title = "OLS compared to SLM")







##################################################################
##                From general to the particular                ##
##################################################################



##############
##  1) SDM  ##
##############
# M3
reg.sdm <- lagsarlm(crimepc ~  PER.C_COME+ PERCE_old+PERCE_unem+ Prc_pvrty+PERCE_crwd+
                      prcwodplm, Durbin=~PER.C_COME+ PERCE_old+PERCE_unem+ Prc_pvrty+PERCE_crwd+
                      prcwodplm,data=data, w, type = "mixed")
summary(reg.sdm)
(LR_rho1=2*(reg.sdm$LL - logLik(reg.slx)))
# 'log Lik.' 9.282095 (df=14)

LR.sarlm(reg.sdm,reg.sem)  # p-value = 0.0455
LR.sarlm(reg.sdm,reg.slm)  # p-value = 0.1052


##############
##  2) SLX  ##
##############

# M6
w_x1 <- lag.listw(w, data$PER.C_COME)
w_x2 <- lag.listw(w, data$PERCE_old)
w_x3 <- lag.listw(w, data$PERCE_unem)
w_x4 <- lag.listw(w, data$Prc_pvrty)
w_x5 <- lag.listw(w, data$PERCE_crwd)
w_x6 <- lag.listw(w, data$prcwodplm)
reg.slx <- lm(crimepc ~  PER.C_COME+ PERCE_old+PERCE_unem+ Prc_pvrty+PERCE_crwd+
                prcwodplm + w_x1 + w_x2 + w_x3 + w_x4+w_x5+w_x6 , data=data)
summary(reg.slx)



# Moran's I test
lm.morantest(reg.slx,w, alternative = "two.sided")
# Reject H0: p-value 0.001004

# LM's test
lms <- lm.LMtests(reg.slx, w, test = "all")
tests <- t(sapply(lms, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tests) <- c("Test","df","p-value")
printCoefmat(tests)
#RLMlag  7.9292  1.0000  0.0049 we need to add \rho
# 
# Test      df p-value
# LMerr   4.9441  1.0000  0.0262
# LMlag   8.3120  1.0000  0.0039
# RLMerr  4.5612  1.0000  0.0327
# RLMlag  7.9292  1.0000  0.0049
# SARMA  12.8732  2.0000  0.0016



# Comparing SLX and OLS
stargazer(reg.slx,label = "regslxols",title = "SLX",covariate.labels=c("Per capita income","Percent aged under 18 or over 64","Percent aged 16+ unemployed","Percent households below poverty","Percent of housing crowded", "Percent aged 25+ without high school diploma","W: Per capita income","W: Percent aged under 18 or over 64","W: Percent aged 16+ unemployed","W: Percent households below poverty","W: Percent of housing crowded", "W: Percent aged 25+ without high school diploma") )
          
