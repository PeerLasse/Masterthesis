library(readr)
library(readxl)
library(tidyverse)
library(spdep)
library(pml)
library(spml)
library(fixest)
library(collapse)
library(feather)
setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/data/processed")
source("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/source/functions/dist_approx.R")
############################ Load Data #########################################
data=read_feather("SLX_data_gravitation.feather")
policy_dates<- read_csv("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/data/Info/Heterogenität maßnahmen/Maßnahmeneinführung.csv")

data2=mutate(data,"bundesland"=floor(data$start_krs/1000))
names=c("SH","HH","NI","HB","NW","HE","RP","BW","BY","SL","BE","BB","MV","SN","ST","TH")
data2=mutate(data2,"bundesland"=names[bundesland])

#complete data 
data_bl=collap(data2,moves~bundesland+date,FUN=sum)

ggplot(data=data_bl,aes(x=as.Date(date),y=moves,group=bundesland,color=as.factor(bundesland)))+
  geom_vline(xintercept=as.Date("2020-03-12"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-15"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-18"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-20"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_text(aes(x=as.Date("2020-03-10"), label="M10", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-14"), label="M08", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-17"), label="M14", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-21"), label="M17", y=0), colour="black", text=element_text(size=8)) +
  labs(title="Aggregate Mobility 2020")+
  xlab("Date")+
  ylab("Total number of moves")+
  guides(color=guide_legend(title="State"))+
  scale_color_manual(values=c("#641E16", "#E74C3C", "#884EA0","#D2B4DE","#154360","#5DADE2","#48C9B0","#0B5345","#F1C40F","#7D6608","#F39C12","#1B2631","#99A3A4","#FF33FB","#F5FF33","#33FFFD"))+
  scale_y_continuous(limits = c(0, 43000000))+
  theme_bw()+
  geom_line(size=1)
# only internal movement
data_bl_internal=collap(data2[data2$start_krs==data2$end_krs,],moves~bundesland+date,FUN=sum)

ggplot(data=data_bl_internal,aes(x=as.Date(date),y=moves,group=bundesland,color=as.factor(bundesland)))+
  geom_vline(xintercept=as.Date("2020-03-12"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-15"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-18"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-20"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_text(aes(x=as.Date("2020-03-10"), label="M10", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-14"), label="M08", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-17"), label="M14", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-21"), label="M17", y=0), colour="black", text=element_text(size=8)) +
  labs(title="Aggregate within mobility 2020")+
  xlab("Date")+
  ylab("Total number of moves")+
  guides(color=guide_legend(title="State"))+
  scale_color_manual(values=c("#641E16", "#E74C3C", "#884EA0","#D2B4DE","#154360","#5DADE2","#48C9B0","#0B5345","#F1C40F","#7D6608","#F39C12","#1B2631","#99A3A4","#FF33FB","#F5FF33","#33FFFD"))+
  scale_y_continuous(limits = c(0, 43000000))+
  theme_bw()+
  geom_line(size=1)

# only externalmovement
data_bl_external=collap(data2[data2$start_krs!=data2$end_krs,],moves~bundesland+date,FUN=sum)

ggplot(data=data_bl_external,aes(x=as.Date(date),y=moves,group=bundesland,color=as.factor(bundesland)))+
  geom_vline(xintercept=as.Date("2020-03-12"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-15"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-18"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-20"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_text(aes(x=as.Date("2020-03-10"), label="M10", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-14"), label="M08", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-17"), label="M14", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-21"), label="M17", y=0), colour="black", text=element_text(size=8)) +
  labs(title="Aggregate between mobility 2020")+
  xlab("Date")+
  ylab("Total number of moves")+
  guides(color=guide_legend(title="State"))+
  scale_color_manual(values=c("#641E16", "#E74C3C", "#884EA0","#D2B4DE","#154360","#5DADE2","#48C9B0","#0B5345","#F1C40F","#7D6608","#F39C12","#1B2631","#99A3A4","#FF33FB","#F5FF33","#33FFFD"))+
  scale_y_continuous(limits = c(0,10000000))+
  theme_bw()+
  geom_line(size=1)

###### Using MEAN instead of SUM #####################
#complete data 
data_bl=collap(data2,moves~bundesland+date,FUN=mean)

ggplot(data=data_bl,aes(x=as.Date(date),y=moves,group=bundesland,color=as.factor(bundesland)))+
  geom_vline(xintercept=as.Date("2020-03-12"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-15"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-18"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-20"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_text(aes(x=as.Date("2020-03-10"), label="M10", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-14"), label="M08", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-17"), label="M14", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-21"), label="M17", y=0), colour="black", text=element_text(size=8)) +
  labs(title="Mean Mobility 2020")+
  xlab("Date")+
  ylab("Average number of moves")+
  guides(color=guide_legend(title="State"))+
  scale_color_manual(values=c("#641E16", "#E74C3C", "#884EA0","#D2B4DE","#154360","#5DADE2","#48C9B0","#0B5345","#F1C40F","#7D6608","#F39C12","#1B2631","#99A3A4","#FF33FB","#F5FF33","#33FFFD"))+
  scale_y_continuous(limits = c(0, 2100))+
  theme_bw()+
  geom_line(size=1)

# only internal movement
data_bl_internal=collap(data2[data2$start_krs==data2$end_krs,],moves~bundesland+date,FUN=mean)

ggplot(data=data_bl_internal,aes(x=as.Date(date),y=moves,group=bundesland,color=as.factor(bundesland)))+
  geom_vline(xintercept=as.Date("2020-03-12"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-15"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-18"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-20"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_text(aes(x=as.Date("2020-03-10"), label="M10", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-14"), label="M08", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-17"), label="M14", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-21"), label="M17", y=0), colour="black", text=element_text(size=8)) +
  labs(title="Mean within mobility 2020")+
  xlab("Date")+
  ylab("average number of moves")+
  guides(color=guide_legend(title="State"))+
  scale_color_manual(values=c("#641E16", "#E74C3C", "#884EA0","#D2B4DE","#154360","#5DADE2","#48C9B0","#0B5345","#F1C40F","#7D6608","#F39C12","#1B2631","#99A3A4","#FF33FB","#F5FF33","#33FFFD"))+
  scale_y_continuous(limits = c(0, 650000))+
  theme_bw()+
  geom_line(size=1)

# only externalmovement
data_bl_external=collap(data2[data2$start_krs!=data2$end_krs,],moves~bundesland+date,FUN=mean)

ggplot(data=data_bl_external,aes(x=as.Date(date),y=moves,group=bundesland,color=as.factor(bundesland)))+
  geom_vline(xintercept=as.Date("2020-03-12"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-15"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-18"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-20"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_text(aes(x=as.Date("2020-03-10"), label="M10", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-14"), label="M08", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-17"), label="M14", y=0), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-21"), label="M17", y=0), colour="black", text=element_text(size=8)) +
  labs(title="Mean between mobility 2020")+
  xlab("Date")+
  ylab("average number of moves")+
  guides(color=guide_legend(title="State"))+
  scale_color_manual(values=c("#641E16", "#E74C3C", "#884EA0","#D2B4DE","#154360","#5DADE2","#48C9B0","#0B5345","#F1C40F","#7D6608","#F39C12","#1B2631","#99A3A4","#FF33FB","#F5FF33","#33FFFD"))+
  scale_y_continuous(limits = c(0,2100))+
  theme_bw()+
  geom_line(size=1)



data_agg=collap(data,moves~date,FUN=sum)

#12. märz wurde M10 zum allerersten mal eingeführt(SH) 
#Als letztes hat tühringen M10 eingeführt am 27.märz

#15. märz wurde M08 in HH und SH gleichzeitig eingeführt 
#Als letztes wieder Tühringen am 27.März

#20.03.2020 wurde M14 das erstemal eingeführt, von Rheinlandpfalz 
#Als letztes führte bayern diese Maßnahme am 13.01.2021 ein 
#In unserem beobachtungszeitraum haben nur 3 Länder diese maßnahme eingeführt (RP,BW,Sachsen anhalt)

#18.03.2020 M17 wurde das erstemal einfeführt, von Sachsen
#Als letztes wurde diese maßnhame von Sachasen Anhalt eingeführt am 14.09.2021 
# In unserem Beobachtungszeitraum nur 3 länder (Sachsen, Thüringen, NRW)

ggplot(data=data_agg,aes(x=as.Date(date),y=moves))+
  geom_vline(xintercept=as.Date("2020-03-12"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-15"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-18"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_vline(xintercept=as.Date("2020-03-20"),linetype="dashed", 
             color = "red", size=1,alpha=0.5)+
  geom_text(aes(x=as.Date("2020-03-10"), label="M10", y=50000000), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-14"), label="M08", y=50000000), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-17"), label="M14", y=50000000), colour="black", text=element_text(size=8)) +
  geom_text(aes(x=as.Date("2020-03-21"), label="M17", y=50000000), colour="black", text=element_text(size=8)) +
  labs(title="Aggregate Mobility Germany 2020")+
  xlab("Date")+
  ylab("Total number of moves")+
  scale_y_continuous(limits = c(50000000, 200000000))+
  theme_bw()+
  geom_line(color="steelblue",size=1.2)


ggplot(data=data[as.character(data$date)%in% c("2020-03-11","2020-03-12","2020-03-13","2020-03-14","2020-03-15","2020-03-16","2020-03-17","2020-03-18","2020-03-19"),],aes(as.Date(date),fill=factor(M08_start)))+geom_bar(position="fill")

dates=as.character(unique(data$date))

par(mfrow = c(2, 2))
p1=ggplot(data =data[as.character(data$date)%in% dates[7:21],],aes(x = date, fill =factor(M08_start)))+
  geom_bar(position="fill")+
  xlab("Date")+
  ylab("Fraction of observations")+
  labs(title="Implementation of M08")+
  scale_fill_manual(labels = c("inactive", "active"),values=c("#F8766D","#00BFC4")) +
  guides(fill=guide_legend(title="Restriction on gastronomy"))+
  theme(axis.text.x = element_text(angle = 90))



p2=ggplot(data =data[as.character(data$date)%in% dates[9:30],],aes(x = date, fill =factor(M10_start)))+
  geom_bar(position="fill")+
  xlab("Date")+
  ylab("Fraction of observations")+
  labs(title="Implementation of M10")+
  scale_fill_manual(labels = c("inactive", "active"),values=c("#F8766D","#00BFC4")) +
  guides(fill=guide_legend(title="Restriction on nightlife"))+
  theme(axis.text.x = element_text(angle = 90))

p3=ggplot(data =data[as.character(data$date)%in% dates[13:20],],aes(x = date, fill =factor(M14_start)))+
  geom_bar(position="fill")+
  xlab("Date")+
  ylab("Fraction of observations")+
  labs(title="Implementation of M14")+
  scale_fill_manual(labels = c("inactive", "active"),values=c("#F8766D","#00BFC4")) +
  guides(fill=guide_legend(title="Restriction on internal movement"))+
  theme(axis.text.x = element_text(angle = 90))

p4=ggplot(data =data[as.character(data$date)%in% dates[13:20],],aes(x = date, fill =factor(M17_start)))+
  geom_bar(position="fill")+
  xlab("Date")+
  ylab("Fraction of observations")+
  labs(title="Implementation of M17")+
  scale_fill_manual(labels = c("inactive", "active"),values=c("#F8766D","#00BFC4")) +
  guides(fill=guide_legend(title="Restriction on workplaces"))+
  theme(axis.text.x = element_text(angle = 90))


library(gridExtra)
grid.arrange(p1,p2,p3,p4,ncol=2)


#experiment with shiny interface
library(ExPanDaR)
dates=unique(data$date)
ExPanD(data[,1:12], cs_id = "obs", ts_id = "date")
