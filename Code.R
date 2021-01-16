############################################################################
############################################################################
###                                                                      ###
###                       UNSOLVED CRIMES HOTSPOTS                       ###
###                                                                      ###
############################################################################
############################################################################


# Author: Juan Andrés Cabral
# Contact: juan.drs.c@gmail.com


# Libraries
library(ggplot2) # Graphs
library(dplyr) # Pipes and more
library(stargazer) # Latex tables

# Path
rm(list=ls())
drive <- "G:/Facultad/San andres/Econometría espacial/working paper"
setwd(drive)

# Load data
data<-read.csv("washington/homicide-data.csv")



# New variables
data$open_or_no_arrest<-ifelse(data$disposition=="Closed without arrest"|
                                 data$disposition=="Open/No arrest",1,0)
data$closed_without_arrest<-ifelse(data$disposition=="Closed without arrest",1,0)
data$arrest<-ifelse(data$disposition=="Closed by arrest",1,0)
data$count<-1



# Proportion of closed and not solved
sum(data$closed_without_arrest)/sum(data$count)
# 0.055

# Proportion of not solved cases
sum(data$open_or_no_arrest)/sum(data$count)
# 0.507963

# Proportion of arrested
sum(data$arrest)/sum(data$count)
# 0.492037

unique(data$victim_race)
unique(data$victim_sex)

data$hispanic<-ifelse(data$victim_race=="Hispanic",1,0)
data$black<-ifelse(data$victim_race=="Black",1,0)
data$white<-ifelse(data$victim_race=="White",1,0)
data$white<-ifelse(data$victim_race=="White",1,0)
data$male<-ifelse(data$victim_sex=="Male",1,0)

# Comparisons
sum(data[data$black==1,]$open_or_no_arrest)/sum(data[data$black==1,]$count)
sum(data[data$white==1,]$open_or_no_arrest)/sum(data[data$white==1,]$count)

sum(data[data$male==1,]$open_or_no_arrest)/sum(data[data$male==1,]$count)
sum(data[data$male==0,]$open_or_no_arrest)/sum(data[data$male==0,]$count)


data$age<-ifelse(data$victim_age!="Unknown",data$victim_age,NA)


# Date format
data$date<-paste0(substr(data$reported_date,1,4),"-",
                  substr(data$reported_date,5,6),"-",substr(data$reported_date,7,8))


stargazer(data,keep = c("age","black","white","male","arrest","open_or_no_arrest"),
          omit.summary.stat=c("p25","p75","sd"),title="Descriptive statistics", label="desc")

############################################################################
############################################################################
###                                                                      ###
###                                GRAPHS                                ###
###                                                                      ###
############################################################################
############################################################################



#################################################################
##                     Age and case status                     ##
#################################################################

cor(data$age,data$open_or_no_arrest,use="complete.obs")
#-0.03672649

p <- ggplot(data, aes(x = age)) + 
  # geom_line(data=mardel,aes(y = mediana, colour = "Mar del Plata"))+
  #geom_line(data=pinamar,aes(y = mediana, colour = "Pinamar"))+
  geom_point(data=data,aes(y = open_or_no_arrest))+
  geom_smooth(aes(y=open_or_no_arrest),method=lm,se = FALSE)+

#scale_y_continuous(sec.axis = sec_axis(~., name = "Mediana real Pinamar")) +
labs(y = "Open case or closed without arrest",
     x = "Age")  +
  # scale_colour_manual(values = c("blue", "red","green","orange","cyan","yellow","black")) +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())
p



#################################################################
##                        Time analysis                        ##
#################################################################


# Format dates
data$date
data$date <- as.Date(data$date, "%Y-%m-%d")
data$year<-paste0(substr(data$reported_date,1,4),"-","01","-","01")
data$year <- as.Date(data$year, "%Y-%m-%d")
data$year <- as.Date(data$year, "%Y")

dataunsolved<-data[data$open_or_no_arrest==1,]
datasolved<-data[data$open_or_no_arrest==0,]

# Aggregate
data2<- data %>% group_by(date)%>% mutate(crimes=sum(count))
dataunsolved<- dataunsolved %>% group_by(year)%>% mutate(crimes=sum(count))
datasolved<- datasolved %>% group_by(year)%>% mutate(crimes=sum(count))

data2<-as.data.frame(data2)

# Graphs
graph <- ggplot(datasolved, aes(x = year)) + 

labs(y = "Homicides",
     x = "Date")  +
  #geom_point(data=data2,aes(y = crimes))+
  geom_line(data=datasolved,aes(y = crimes,colour="solved"),size=1)+
  geom_line(data=dataunsolved,aes(y = crimes,colour="unsolved"),size=1)+
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())+
  scale_x_date(date_labels = "%Y") 


graph

ggsave("G:/Facultad/San andres/Econometría espacial/working paper/washington/graphs/timeline.png",
       dpi=320,units="in",width=10,height=5)












