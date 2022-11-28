library(readr)
library(readxl)
library(tidyverse)
library(spdep)
library(spatialreg)
library(plm)
library(splm)
library(fixest)
library(feather)
setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/data/processed")
source("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/source/functions/dist_approx.R")
############################ Load Data #########################################
data=read_feather("V2SLX_data_gravitation.feather")
#INZIDENZ VARIABLEN wurden mit 1 addiert, log(1)=0 

#transforming distance
data$distance=ifelse(data$distance!=0,data$distance,approx_intra_distance(data))

########## Estimation via pseudo poisson maximum likelihood with fixed effects and heterosk.SE ################

data2=data %>% pdata.frame(index=c("obs","date"))
memory.limit(size=60000)

# create AFD dummy -> überdurchschnittlicher Anteil AFD wähler
data2$Afd_dummy_start=ifelse(data2$AFD_start>mean(data2$AFD_start),1,0)
data2$Linke_dummy_start=ifelse(data2$Linke_start>mean(data2$Linke_start),1,0)
data2$Afd_dummy_end=ifelse(data2$AFD_end>mean(data2$AFD_end),1,0)
data2$Linke_dummy_end=ifelse(data2$Linke_end>mean(data2$Linke_end),1,0)
data2$government_dummy_start=ifelse(data2$CDU_CSU_start+data2$SPD_start>mean(data2$CDU_CSU_start+data2$SPD_start),1,0)
data2$government_dummy_end=ifelse(data2$CDU_CSU_end+data2$SPD_end>mean(data2$CDU_CSU_end+data2$SPD_end),1,0)
data2$Gruene_dummy_start=ifelse(data2$Gruene_start>mean(data2$Gruene_start),1,0)
data2$Gruene_dummy_end=ifelse(data2$Gruene_end>mean(data2$Gruene_end),1,0)

###Test for parallel trends ##############
dates=unique(data2$date)
#The reason behind this test is to look whether coefficients for treatment variable interacted with time differs significantly from zero pre treatment
#pre treatment is defined as the time before any of the counties were treated

#choose datess before and after treatment
#we use counties which were treated after 2020-03-22 as control group
data3=data2[as.character(data2$date) %in% dates[4:16],]
data3$treat_M08_start=rep(0,dim(data3)[1])
data3$treat_M08_end=rep(0,dim(data3)[1])

groups=data.frame("Krs"=unique(data3$start_krs),"Treatment"=rep(0,400))
for(krs in unique(data3$start_krs)){
  if(sum(data3$M08_start[data3$start_krs==krs])>0){
    groups[groups$Krs==krs,"Treatment"]=1
  }
}
colnames(groups)=c("start_krs","treatment_M08_start")
data3=left_join(data3,groups,by="start_krs")
colnames(groups)=c("end_krs","treatment_M08_end")
data3=left_join(data3,groups,by="end_krs")

#test for m08
form=moves~ log_inzidenz_start+log_inzidenz_end+i(date,treatment_M08_start,"2020-03-12")+i(date,treatment_M08_end,"2020-03-12")+
  Afd_dummy_start:M08_start+Afd_dummy_end:M08_end+
  government_dummy_start:M08_start+government_dummy_end:M08_end+
  M08_start_W+M08_end_W+log_inzidenz_start_W+log_inzidenz_end_W|start_krs+end_krs+obs+date
fit1=feglm(form, data =data3,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit6)
names=names(coef(fit1))[3:26]
p1=coefplot(fit1,keep=c(names[1:4]),pt.join = TRUE)
p2=coefplot(fit1,keep=c(names[13:16]),pt.join=TRUE)
#######################################################################################################
#test for m10 
#we use counties which were treated after 2020-03-22 as control group
#choose date 
data3=data2[as.character(data2$date) %in% dates[4:16],]
data3$treat_M10_start=rep(0,dim(data3)[1])
data3$treat_M10_end=rep(0,dim(data3)[1])

groups=data.frame("Krs"=unique(data3$start_krs),"Treatment"=rep(0,400))
for(krs in unique(data3$start_krs)){
  if(sum(data3$M10_start[data3$start_krs==krs])>0){
    groups[groups$Krs==krs,"Treatment"]=1
  }
}
colnames(groups)=c("start_krs","treatment_M10_start")
data3=left_join(data3,groups,by="start_krs")
colnames(groups)=c("end_krs","treatment_M10_end")
data3=left_join(data3,groups,by="end_krs")

form=moves~ log_inzidenz_start+log_inzidenz_end+i(date,treatment_M10_start,"2020-03-12")+i(date,treatment_M10_end,"2020-03-12")+
  Afd_dummy_start:M10_start+Afd_dummy_end:M10_end+
  government_dummy_start:M10_start+government_dummy_end:M10_end+
  M10_start_W+M10_end_W+log_inzidenz_start_W+log_inzidenz_end_W|start_krs+end_krs+obs+date
fit2=feglm(form, data =data3,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit6)
names=names(coef(fit2))[3:26]
p3=coefplot(fit2,keep=c(names[1:4]),pt.join = TRUE)
p4=coefplot(fit2,keep=c(names[13:16]),pt.join=TRUE)
#######################################################################################################
#test for m14
#choose date 
# for m14 the day of first implementation 2020-03-20 was used as day of implementation
#only consider data up to march 31st
data3=data2[as.character(data2$date) %in% dates[12:25],]
data3$treat_M14_start=rep(0,dim(data3)[1])
data3$treat_M14_end=rep(0,dim(data3)[1])

groups=data.frame("Krs"=unique(data3$start_krs),"Treatment"=rep(0,400))
for(krs in unique(data3$start_krs)){
  if(sum(data3$M14_start[data3$start_krs==krs])>0){
    groups[groups$Krs==krs,"Treatment"]=1
  }
}
colnames(groups)=c("start_krs","treatment_M14_start")
data3=left_join(data3,groups,by="start_krs")
colnames(groups)=c("end_krs","treatment_M14_end")
data3=left_join(data3,groups,by="end_krs")


form=moves~ log_inzidenz_start+log_inzidenz_end+i(date,treatment_M14_start,"2020-03-20")+i(date,treatment_M14_end,"2020-03-20")+
  Afd_dummy_start:M14_start+Afd_dummy_end:M14_end+
  government_dummy_start:M14_start+government_dummy_end:M14_end+
  M14_start_W+M14_end_W+log_inzidenz_start_W+log_inzidenz_end_W|start_krs+end_krs+obs+date
fit3=feglm(form, data =data3,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit6)
names=names(coef(fit3))[3:28]
p5=coefplot(fit3,keep=c(names[1:4]),pt.join = TRUE)
p6=coefplot(fit3,keep=c(names[14:17]),pt.join=TRUE)
names#######################################################################################################
#test for m17 
#choose date 
#For M17 the date 2020-03-18was choosen as day of treatment
#only consider data up to march 31st
data3=data2[as.character(data2$date) %in% dates[10:25],]

data3$treat_M17_start=rep(0,dim(data3)[1])
data3$treat_M17_end=rep(0,dim(data3)[1])

groups=data.frame("Krs"=unique(data3$start_krs),"Treatment"=rep(0,400))
for(krs in unique(data3$start_krs)){
  if(sum(data3$M17_start[data3$start_krs==krs])>0){
    groups[groups$Krs==krs,"Treatment"]=1
  }
}
colnames(groups)=c("start_krs","treatment_M17_start")
data3=left_join(data3,groups,by="start_krs")
colnames(groups)=c("end_krs","treatment_M17_end")
data3=left_join(data3,groups,by="end_krs")


form=moves~ log_inzidenz_start+log_inzidenz_end+M17_start+M17_end+i(date,treatment_M17_start,"2020-03-18")+i(date,treatment_M17_end,"2020-03-18")+
  Afd_dummy_start:M17_start+Afd_dummy_end:M17_end+
  government_dummy_start:M17_start+government_dummy_end:M17_end+
  M17_start_W+M17_end_W+log_inzidenz_start_W+log_inzidenz_end_W|start_krs+end_krs+obs+date
fit4=feglm(form, data =data3,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit6)

names=names(coef(fit4))[5:34]
p7=coefplot(fit4,keep=c(names[1:4]),pt.join = TRUE)
p8=coefplot(fit4,keep=c(names[16:19]),pt.join=TRUE)

library(gridExtra)
