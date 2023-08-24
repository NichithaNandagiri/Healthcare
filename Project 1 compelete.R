getwd()
setwd("C:\\Users\\nichi\\OneDrive\\Desktop\\Web app")
getwd()

data <- read.csv("Healthcare.csv",stringsAsFactors = T)
View(data)

## patient & appointment ID are auto increment
# so it normally not included
#it doesn't make any sense 

#####----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringi)
library(lubridate)
library(psych)
library(mltools)

head(data)
tail(data)

summary(data)
glimpse(data)
dim(data)
class(data)
describe(data)

#######--- check any NA values------------------------------------
sum(is.na(data))

####---change col names----------------------------------------------
colnames(data)

data <- data %>% 
  rename(Patient_ID = PatientId,     
         Appointment_ID = AppointmentID,
         Scheduled_Day = ScheduledDay,
         Appointment_Day = AppointmentDay,
         Hypertension = Hipertension,
         Handicap = Handcap,
         No_show = No.show)
view(data)

###--convert it into date-----------------------------------------
data$Scheduled_Day = as.Date(data$Scheduled_Day)
class(data$Scheduled_Day)

data$Appointment_Day = as.Date(data$Appointment_Day)
class(data$Appointment_Day)

#########--day of the week-----------------------------------------------------------
# 1-sun,2-mon,3-tue.........                            
wday(data$Scheduled_Day)                                 
                                                            
## week starts with Monday----                          
wday(data$Scheduled_Day,week_start = 1)                  

## char day------
wday(data$Scheduled_Day,label = T)

wday(data$Scheduled_Day,label = T,abbr = F)


#############################################################

data$Sch_weekday <- wday(data$Scheduled_Day,week_start = 1)

data$App_weekday <- wday(data$Appointment_Day,week_start = 1)

view(data)

#### count of the weekday-----------=-------------------------------
## schedule 
data %>% 
  group_by(Sch_weekday) %>% 
  summarise(count=length(Sch_weekday))

## appointment 
data %>% 
  group_by(App_weekday) %>% 
  summarise(count= length(App_weekday))

## or
table(data$Alcoholism)
table(data$No_show)
table(data$Handicap)
table(data$Diabetes)
table(data$Scholarship)

####-------------------------------------------------------------
## bar plot
ggplot(data,aes(No_show))+  
  geom_bar(aes(fill=No_show))+
  labs(title = "Count of TARGET Variables per category")+
       xlab("Target Variables")+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))

##------
ggplot(data,aes(No_show))+   ### without percent
  geom_bar(aes(fill=No_show))+
  labs(title = "Count of TARGET Variables per category")+
  xlab("Target Variables")+
  scale_y_continuous(labels= scales::percent)


### percent of the values in variables###################################################################
## percent of the yes , no
round(prop.table(table(data$No_show))*100,2)

prop.table(xtabs(~Gender+No_show,data=data))*100
## Inference is highly imbalance 

lapply(data, function(x) round(prop.table(table(x))*100,2))

###### plot age group ############################################################################

summary(data$Age)

data <- data %>% 
  mutate("Age_group"= case_when(Age<13 ~ "Age.00-12",
                                Age>=13 & Age<18 ~ "Age.13-17",
                                Age>=18 & Age<60 ~ "Age.18-60",
                                Age>=60 ~ "Age.60-ov"))
view(data)

ggplot(data, aes(Age_group , fill=Age_group))+geom_bar()+
  scale_fill_brewer(palette = "Set1") + 
  scale_y_continuous(labels= function(x) paste0(x/1000, "%")) + 
  ylab("Percentage %") + 
  ggtitle("Age Groups") 

##### count of variables  ###############################################_____________________
data %>% 
  group_by(Gender) %>% 
  summarise(count=length(Gender))

data %>% 
  group_by(No_show) %>% 
  summarise(count=length(No_show))

## or

fd<- data %>% 
  select(everything(data)) %>% 
  group_by(Gender) %>% 
  summarise(n=n()) %>% 
  mutate(freq=(n/sum(n)*100))

fd 

####-----------------------------------------------

data<- data %>% 
  mutate(No_show=case_when(No_show=="No" ~0,
         No_show=="Yes" ~1))

ggplot(data, aes(Hypertension,fill=Gender))+geom_bar()+
  facet_wrap(~Gender)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))

ggplot(data, aes(Age_group,fill=Gender))+geom_bar()+
  facet_wrap(~Hypertension)+ 
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))

ggplot(data,aes(Age_group, fill=Gender))+geom_bar(position = "Fill")+
  scale_y_continuous(labels= function(x) paste0(x*100, "%"))

ggplot(data,aes(Gender,fill=Neighbourhood))+geom_bar()+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  coord_polar(theta = "x")


data$Gender=as.factor(data$Gender)
data1<- one_hot(as.data.table(data))

#####################
for (i in 1:ncol(data)) {
  print(mean(data[,i]))
}
#or
apply(data,2,mean)
##########################

data <- data %>% 
  mutate("Gender_M"= case_when(Gender=="M" ~1,
                               Gender=="F" ~0),
         "Gender_F"=case_when(Gender=="M" ~0,
                              Gender=="F" ~1))
   
data <- data %>% 
  mutate("Age.00-12"= case_when(Age_group=="Age.00-12"~1,
                             Age_group!="Age.00-12"~0),
         "Age.13-17"=case_when(Age_group=="Age.13-17"~1,
                               Age_group!="Age.13-17"~0),
         "Age.18-60"=case_when(Age_group=="Age.18-60"~1,
                               Age_group!="Age.18-60"~0),
         "Age.60-ov"=case_when(Age_group=="Age.60-ov"~1,
                               Age_group!="Age.60-ov"~0))

colnames(data)

####----------------------------------------------------------

## hypertension 
ggplot(data, aes(Hypertension,fill=Gender))+geom_bar()+
  facet_wrap(~Gender)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Hypertension per Gender (%)")

## diabetes
ggplot(data, aes(Diabetes,fill=Gender))+geom_bar()+
  facet_wrap(~Gender)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Diabetes per Gender (%)")

## alcoholism
ggplot(data, aes(Alcoholism,fill=Gender))+geom_bar()+
  facet_wrap(~Gender)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Alcoholism per Gender (%)")

## Handicap
ggplot(data, aes(Handicap,fill=Gender))+geom_bar()+
  facet_wrap(~Gender)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Handicap per Gender (%)")

#######---------------------------------------------------------------
######---------------------------------------------------------------------

## Handicap
ggplot(data, aes(Handicap,fill=Gender))+geom_bar(aes(fill=Gender))+
  facet_wrap(~Age_group)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Handicap per Age group (%)")

## Alcoholism
ggplot(data, aes(Alcoholism,fill=Gender))+geom_bar(aes(fill=Gender))+
  facet_wrap(~Age_group)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Alcoholism per Age group (%)")

## Hypertension
ggplot(data, aes(Hypertension,fill=Gender))+geom_bar(aes(fill=Gender))+
  facet_wrap(~Age_group)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Hypertension per Age group (%)")

## diabetes 
ggplot(data, aes(Diabetes,fill=Gender))+geom_bar(aes(fill=Gender))+
  facet_wrap(~Age_group)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Diabetes per Age group (%)")


#######   No show & show  ############################################################################3
ggplot(data,aes(No_show,fill=Gender))+geom_bar()+
  facet_wrap(~Gender)+ 
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Number of People showed per Gender")


ggplot(data,aes(No_show,fill=Gender))+geom_bar(aes(fill=Gender))+
  facet_wrap(~Age_group)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "Number of People showed per Age group and Gender")

############################################################################
###############################################################################################

##  findings

1 #Female patients have taken more appointments then male patients
ggplot(data,aes(Gender,fill=Appointment_ID))+geom_bar(aes(fill=Gender))+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))+
  labs(title = "No of Appointments taken per Gender")


2 ### Scholarship
table(data$Scholarship)


ggplot(data,aes(No_show,fill=Gender))+geom_bar()+
  facet_wrap(~Scholarship)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))

prop.table(xtabs(~Scholarship+No_show,data=data))*100

3### Hypertension

table(data$Hypertension)
round(prop.table(xtabs(~Hypertension+No_show,data))*100,2)

ggplot(data,aes(No_show,fill=Gender))+geom_bar()+
  facet_wrap(~Hypertension)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))

4### Diabetes

table(data$Diabetes)

round(prop.table(xtabs(~Diabetes+No_show,data))*100,2)

ggplot(data,aes(No_show,fill=Gender))+geom_bar()+
  facet_wrap(~Diabetes)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))

5##  Handicap

table(data$Handicap)
ggplot(data,aes(Handicap,fill=Gender))+geom_bar()+
  facet_wrap(~No_show)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))

6###  SMS
table(data$SMS_received)

round(prop.table(xtabs(~SMS_received+No_show,data))*100,2)

ggplot(data,aes(SMS_received,fill=Gender))+geom_bar()+
  facet_wrap(~No_show)+
  scale_y_continuous(labels= function(x) paste0(x/1000, "%"))

view(data)
