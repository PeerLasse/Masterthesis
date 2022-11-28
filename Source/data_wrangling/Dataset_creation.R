#load packages -- install if not already installed
library(tidyverse)
library(readr)
library(readxl)
library(feather)
library(collapse)
library(reshape2)
library(plm)
library(Matrix)
library(spdep)

##########################set working directory and load data###############################################
setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/Source/data_wrangling/Clean/Daten")
mobility <- read_csv("mobility_teralytics_kreis_nol.csv")
mobility=read_csv("temp1.csv")[,-1]

########################## Change dataformat to something we can work with more easily.###########################
months=c("jan","feb","mar","apr","may","jun")
month_count=c("01","02","03","04","05","06")
dates_mob=mobility$date
for (m in months){
#
   for( i in 1:31){
     if(i<10){
       dates_mob=gsub(paste("0",as.character(i),m,sep=""),paste("0",as.character(i),"_",month_count[which(months==m)],"_",sep=""),dates_mob,perl=TRUE)
     }
     else{
      dates_mob=gsub(paste(as.character(i),m,sep=""),paste(as.character(i),"_",month_count[which(months==m)],"_",sep=""),dates_mob,perl=TRUE)
     }

   }
   print(m)
 }
mobility$date=dates_mob

mobility$date=as.Date(mobility$date,format="%d_%m_%Y",origin="1970-01-01")


##########################Select Timeframe in Question################################################
mobility=as.data.frame(mobility)
mobility=mobility[mobility[,1] >= as.Date("2020-03-07") & mobility[,1] <= as.Date("2020-04-30"),]



########################## Aggregate modes of transport to reduce computational load##########################
mobility<-aggregate(mobility[,-c(1,2,3,4,6)],by=list("date"=mobility$date,"start_krs"=mobility$start_krs,"end_krs"=mobility$end_krs),FUN=sum)
colnames(mobility)[4]="moves"

########################## Balance the panel #####################################################
# Note: this is a rather primitive approach, however all automated options for balancing the panel did not 
# work as intended and/or exceeded the computational resources at hand
#This approach works fine.

dist_mat_401 <- read_csv("dist_mat_401_durations.csv")
distances=melt(dist_mat_401,id.vars = "Kennziffer") %>% type.convert()
L=data.frame("start_krs"=distances[,1],"end_krs"=distances[,2])
L=L[L$start_krs!=16056 &L$end_krs !=16056,]

m=data.frame("start_krs"=rep(NA,160000*55),"end_krs"=rep(NA,160000*55),"date"=rep(NA,160000*55))

L2=rbind(L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L)

m$start_krs=L2$start_krs
m$end_krs=L2$end_krs
dates=unique(mobility$date)

m=arrange(m,start_krs,end_krs)
m$date=rep(dates,160000)

new_data=full_join(mobility,m,by=c("start_krs","end_krs","date"))
new_data$moves=replace_na(new_data$moves,0)
mobility=new_data

##########################Start bringing in other variables#################################################
#Exclude obsolete counties
dist_mat_401=dist_mat_401[dist_mat_401$Kennziffer!=16056,-385]

#reshape distance matrix into paired list
distances=melt(dist_mat_401,id.vars = "Kennziffer") %>% type.convert()
colnames(distances)=c("start_krs","end_krs","distance")

# join mobility data and infections data
infections=read.csv("7 Tage Inzidenz Kreise.csv")
infections$date=as.Date(infections$date,format="%Y_%m_%d")
colnames(infections)[5]="start_krs"

data=left_join(mobility,distances,by=c("start_krs","end_krs")) %>% left_join(infections[,c(5,8,9)],by=c("date","start_krs")) 

colnames(infections)[5]="end_krs"

data=data %>% left_join(infections[,c(5,8,9)],by=c("date","end_krs")) 
colnames(data)[6:7]=c("Inzidenz_start","Inzidenz_end")

#join data on county population and area
bev=read_excel("401 Kreise.xlsx") %>% type.convert()
colnames(bev)[1]="start_krs"

# Eingliederung von Eisenach zu Wartburgkreis
bev[bev$start_krs==16063,5:6]=bev[bev$start_krs==16063,5:6]+bev[bev$start_krs==16056,5:6]
bev=bev[bev$start_krs!=16056,]

data=left_join(data,bev[,c(1,5,6)],by="start_krs")
colnames(bev)[1]="end_krs"
colnames(data)[8:9]=c("Flaeche_km2_start","bevoelkerung_start")
data=left_join(data,bev[,c(1,5,6)],by="end_krs")
colnames(data)[10:11]=c("Flaeche_km2_end","bevoelkerung_end")



#read policy Data
M08=read_csv("M08.csv")
M10=read_csv("M10.csv")
M14=read_csv("M14.csv")
M17=read_csv("M17.csv")
# Eingliederung von Eisenach zu Wartburgkreis
Eingliedern=function(M){
  M[M$ags5==16063,6]=M[M$ags5==16063,6]+M[M$ags5==16056,6]
  M=M[M$ags5!=16056,]
  M$value=ifelse(M$value>0,1,0)
  colnames(M)[c(3,6)]=c("start_krs",M$m_code[1])
  M=mutate(M,"end_krs"=M$start_krs,.after=start_krs)
  M$date=as.Date(M$date,format="%Y_%m_%d")
  return(M)
}
res=lapply(list(M08,M10,M14,M17),FUN=Eingliedern)
M08=res[[1]][,-c(1,5,6)]
M10=res[[2]][,-c(1,5,6)]
M14=res[[3]][,-c(1,5,6)]
M17=res[[4]][,-c(1,5,6)]

#Join policy data
data=left_join(data,M08[,-3],by=c("date","start_krs")) %>% left_join(M10[,-3],by=c("date","start_krs")) %>% left_join(M14[,-3],by=c("date","start_krs")) %>% 
  left_join(M17[,-3],by=c("date","start_krs")) %>% left_join(M08[,-2],by=c("date","end_krs"))%>% left_join(M10[,-2],by=c("date","end_krs")) %>% left_join(M14[,-2],by=c("date","end_krs")) %>% 
  left_join(M17[,-2],by=c("date","end_krs"))

#read and join data on homeoffice potential
Reg_home=read.csv("Homeoffice_potential_400Kreise.csv")
colnames(Reg_home)[1]="start_krs"
colnames(Reg_home)[-1]=gsub("krs","start",colnames(Reg_home[-1]))
data2=left_join(data,Reg_home,by="start_krs")

colnames(Reg_home)[1]="end_krs"
colnames(Reg_home)[-1]=gsub("start","end",colnames(Reg_home[-1]))
data2=left_join(data2,Reg_home,by="end_krs")

#read and join data on election results btw. 2017
wahlen=read.csv("Wahlergebnisse_Bundestagswahl_2017_länder.csv",sep=";")
wahlen$NR=c(8,9,11,12,4,2,6,13,3,5,7,10,14,15,1,16)
data2=mutate(data2,"NR"=floor(data2$start_krs/1000))
colnames(wahlen)[c(2,3,4,6)]=c("CDU_start","SPD_start","AFD_start","Linke_start")
data2=left_join(data2,wahlen[,c(2,3,4,6,8)],by="NR")

data2=mutate(data2,"NR"=floor(data2$end_krs/1000))
colnames(wahlen)[c(2,3,4,6)]=c("CDU_end","SPD_end","AFD_end","Linke_end")
data2=left_join(data2,wahlen[,c(2,3,4,6,8)],by="NR")

#get rid of helper for matching the states
data2=select(data2,-32)

colnames(data2)=gsub(".x","_start",colnames(data2))
colnames(data2)=gsub(".y","_end",colnames(data2))

#add 1 to Inzidenz since we need the log of it 
data2$Inzidenz_start=data2$Inzidenz_start+1
data2$Inzidenz_end=data2$Inzidenz_end+1

#log incidence 
data2=mutate(data2,"log_inzidenz_start"=log(Inzidenz_start),"log_inzidenz_end"=log(Inzidenz_end))

#create observation identifier
data2=mutate(data2,"obs"=paste(data2$start_krs,data2$end_krs,sep="_"),.before=1)

#delete useless objects in environment
rm(list=setdiff(ls(),"data2"))

#arrange data for matching 
data2=arrange(data2,date,start_krs,end_krs)

#Source Weight_functions
setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/Source/data_wrangling/functions")
source("create.weights.R")
source("apply.weights.R")

memory.limit(size=60000)

#Compute and join spatial lags for Incicdence and policy variables
varlist=c("log_inzidenz_start","M08_start","M10_start","M14_start","M17_start","log_inzidenz_end","M08_end","M10_end","M14_end","M17_end")
i=1
for(var in varlist){
  if(i <=5){
    data2[,paste(var,"_W",sep="")]=apply_weights(data2,weights=Weight_o,var =var)
  }
  else{
    data2[,paste(var,"_W",sep="")]=apply_weights(data2,weights=Weight_d,var =var)
  }
  i=i+1
  print(i)
}


data2=arrange(data2,start_krs,end_krs,date)
#set observation and date identifiers for panel econometrics
data2=pdata.frame(data2,index = c("obs","date"))

#save file
write_feather(data2,"SLX_data_gravitation.feather")
write.csv(data1,"SLX_data_gravitation.csv",row.names=FALSE,fileEncoding="UTF-8")
