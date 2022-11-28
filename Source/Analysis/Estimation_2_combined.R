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
  #Most simple model
form=moves~log(distance)+log(bevoelkerung_start)+log(bevoelkerung_end)

fit1=feglm(form, data =data2,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit1) 
coefplot(fit1)
#include time effects 
form=moves~log(distance)+log(bevoelkerung_start)+log(bevoelkerung_end)|date

fit2=feglm(form, data =data2,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit2) 
coefplot(fit2)

# create AFD dummy -> überdurchschnittlicher Anteil AFD wähler
data2$Afd_dummy_start=ifelse(data2$AFD_start>mean(data2$AFD_start),1,0)
data2$Linke_dummy_start=ifelse(data2$Linke_start>mean(data2$Linke_start),1,0)
data2$Afd_dummy_end=ifelse(data2$AFD_end>mean(data2$AFD_end),1,0)
data2$Linke_dummy_end=ifelse(data2$Linke_end>mean(data2$Linke_end),1,0)
data2$government_dummy_start=ifelse(data2$CDU_CSU_start+data2$SPD_start>mean(data2$CDU_CSU_start+data2$SPD_start),1,0)
data2$government_dummy_end=ifelse(data2$CDU_CSU_end+data2$SPD_end>mean(data2$CDU_CSU_end+data2$SPD_end),1,0)
data2$Gruene_dummy_start=ifelse(data2$Gruene_start>mean(data2$Gruene_start),1,0)
data2$Gruene_dummy_end=ifelse(data2$Gruene_end>mean(data2$Gruene_end),1,0)

#Afd repräsentiert nicht einverständniss mit corona politik, governemnt repräsentiert unterstützung
form=moves~log(distance)+log(bevoelkerung_start)+log(bevoelkerung_end)+
  log_inzidenz_start+log_inzidenz_end+
  log(startanteil_homeoffice_WZ)+log(endanteil_homeoffice_WZ)+
  Afd_dummy_start*M08_start+Afd_dummy_end*M08_end+
  government_dummy_start*M08_start+government_dummy_end*M08_end+
  Afd_dummy_start*M10_start+Afd_dummy_end*M10_end+
  government_dummy_start*M10_start+government_dummy_end*M10_end+
  Afd_dummy_start*M14_start+Afd_dummy_end*M14_end+
  government_dummy_start*M14_start+government_dummy_end*M14_end+
  Afd_dummy_start*M17_start+Afd_dummy_end*M17_end+
  government_dummy_start*M17_start+government_dummy_end*M17_end|date

fit3=feglm(form, data =data2,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit3) 
coefplot(fit3)

#add individual fixed effects 
form=moves~log(distance)+log_inzidenz_start+log_inzidenz_end+
  M08_start+M08_end+
  Afd_dummy_start:M08_start+Afd_dummy_end:M08_end+
  government_dummy_start:M08_start+government_dummy_end:M08_end+
  M10_start+M10_end+
  Afd_dummy_start:M10_start+Afd_dummy_end:M10_end+
  government_dummy_start:M10_start+government_dummy_end:M10_end+
  M14_start+M14_end+
  Afd_dummy_start:M14_start+Afd_dummy_end:M14_end+
  government_dummy_start:M14_start+government_dummy_end:M14_end+
M17_start+M17_end+
  Afd_dummy_start:M17_start+Afd_dummy_end:M17_end+
  government_dummy_start:M17_start+government_dummy_end:M17_end| start_krs+end_krs+date

fit4=feglm(form, data =data2,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit4) 
coefplot(fit4)
#add pairwise fixed effects and time effects

form=moves~ log(distance)+log_inzidenz_start+log_inzidenz_end+
  M08_start+M08_end+
  Afd_dummy_start:M08_start+Afd_dummy_end:M08_end+
  government_dummy_start:M08_start+government_dummy_end:M08_end+
  M10_start+M10_end+
  Afd_dummy_start:M10_start+Afd_dummy_end:M10_end+
  government_dummy_start:M10_start+government_dummy_end:M10_end+
  M14_start+M14_end+
  Afd_dummy_start:M14_start+Afd_dummy_end:M14_end+
  government_dummy_start:M14_start+government_dummy_end:M14_end+
  M17_start+M17_end+
  Afd_dummy_start:M17_start+Afd_dummy_end:M17_end+
  government_dummy_start:M17_start| start_krs+end_krs+obs+date
fit5=feglm(form, data =data2,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

#add spatial lags
form=moves~ log_inzidenz_start+log_inzidenz_end+M08_start+M08_end+
    Afd_dummy_start:M08_start+Afd_dummy_end:M08_end+
    government_dummy_start:M08_start+government_dummy_end:M08_end+
    M08_start_W+M08_end_W+log_inzidenz_start_W+log_inzidenz_end_W+
  M10_start+M10_end+
  Afd_dummy_start:M10_start+Afd_dummy_end:M10_end+
  government_dummy_start:M10_start+government_dummy_end:M10_end+
  M10_start_W+M10_end_W+
  M14_start+M14_end+
  Afd_dummy_start:M14_start+Afd_dummy_end:M14_end+
  government_dummy_start:M14_start+government_dummy_end:M14_end+
  M14_start_W+M14_end_W+
  M17_start+M17_end+
  Afd_dummy_start:M17_start+Afd_dummy_end:M17_end+
  M17_start_W+M17_end_W|start_krs+end_krs+obs+date
fit6=feglm(form, data =data2,panel.id=c("obs","date"),vcov="hetero",family = "quasipoisson")

summary(fit6) 
coefplot(fit6)

etable(fit5,fit6, style.tex = style.tex("base"), tex = TRUE)
