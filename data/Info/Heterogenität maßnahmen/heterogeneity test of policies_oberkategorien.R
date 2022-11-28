setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/data/raw")
library(tidyverse)
library(readr)
#m?rz 2020 bis november 2021
covid_policy=read_csv("kr_massnahmen_oberkategorien03_20_15_11_21.csv") %>% na.omit() 
#M_14-----------------------------------------------------------------------------------------
M_14=covid_policy[covid_policy$m_code=="M14",-c(1:3,5:6)]
sum(M_14[,-1]!=0 && M_14[,-1]!=1)
sum(M_14[,-c(1,2)]==2)
sum(M_14[,-c(1,2)]==3)
sum(M_14[,-c(1,2)]==4)
sum(M_14[,-c(1,2)]==5)
#betrachte nur ma?nahme M14 (reisebeschr?nkung Inland)
active_days_M14=data.frame(rowSums(M_14[,-1]))
plot(1:401,jitter(active_days_M14[,1],amount=20),type="p")
#anzahl der Regionen mit mindestens 50 tagen an denen M14 aktiv war
threshold=50
sum(active_days_M14>threshold) #58 Regionen mit 50 tagen an denen M18 aktiv war


active_regions_M14=data.frame(colSums(M_14[,-1]))
plot(1:625,jitter(active_regions_M14[,1],amount=20),type="p")
sum(active_regions_M14> 50) # 117 tage mit mehr als 50 regionen mit M14 

#M_15-----------------------------------------------------------------------------------------                
M_15=covid_policy[covid_policy$m_code=="M15",-c(1:3,5:6)]
sum(M_15[,-1]!=0 && M_15[,-1]!=1)
sum(M_15[,-c(1,2)]==2)
sum(M_15[,-c(1,2)]==3)
sum(M_15[,-c(1,2)]==4)
sum(M_15[,-c(1,2)]==5)
#betrachte nur ma?nahme M15 (reisebeschr?nkung Ausland)
active_days_M15=data.frame(rowSums(M_15[,-1]))
plot(1:401,jitter(active_days_M15[,1],amount=20),type="p")
#anzahl der Regionen mit mindestens 30 tagen an denen M15 aktiv war
threshold=30
sum(active_days_M15>threshold) #Regionen 36 Regionen mit 30 tagen an denen M18 aktiv war


active_regions_M15=data.frame(colSums(M_15[,-1]))
plot(1:625,jitter(active_regions_M15[,1],amount=20),type="p")
sum(active_regions_M15> 30) # 31 tage mit mehr als 30 regionen mit M15 

#M_17-----------------------------------------------------------------------------------------                
M_17=covid_policy[covid_policy$m_code=="M17",-c(1:3,5:6)]
sum(M_17[,-1]!=0 && M_17[,-1]!=1)
sum(M_17[,-c(1,2)]==2)
sum(M_17[,-c(1,2)]==3)
sum(M_17[,-c(1,2)]==4)
sum(M_17[,-c(1,2)]==5)
#betrachte nur ma?nahme M18 (Ausgangsbeschr?nkung)
active_days_M17=data.frame(rowSums(M_17[,-1]))
plot(1:401,jitter(active_days_M17[,1],amount=20),type="p")
#anzahl der Regionen mit mindestens 50 tagen an denen M17 aktiv war
threshold=60
sum(active_days_M17>threshold) # 117 Regionen mit 60 tagen an denen M17 aktiv war


active_regions_M17=data.frame(colSums(M_17[,-1]))
plot(1:625,jitter(active_regions_M17[,1],amount=20),type="p")
sum(active_regions_M17> 60) # 265 tage mit mehr als 60 regionen mit M17

#M_8------------------------------------------------------------
M_8=covid_policy[covid_policy$m_code=="M08",-c(1:3,5:6)]
sum(M_8[,-1]!=0 && M_8[,-1]!=1)
sum(M_8[,-c(1,2)]==2)
sum(M_8[,-c(1,2)]==3)
sum(M_8[,-c(1,2)]==4)
sum(M_8[,-c(1,2)]==5)

active_days_M8=data.frame(rowSums(M_8[,-1]))
plot(1:401,jitter(active_days_M8[,1],amount=2),type="p")

threshold=60
sum(active_days_M8>threshold) 


active_regions_M8=data.frame(colSums(M_8[,-1]))
plot(1:625,jitter(active_regions_M8[,1],amount=2),type="p",xlim =c(0,100) )
sum(active_regions_M8> 60)
# not enough variation between regions (only variation between march and april) Zeitraum von knapp einer woche wo alle regionen m8 implementieren

#M_10------------------------------------------------------------
M_10=covid_policy[covid_policy$m_code=="M10",-c(1:3,5:6)]
sum(M_10[,-1]!=0 && M_10[,-1]!=1)
sum(M_10[,-c(1,2)]==2)
sum(M_10[,-c(1,2)]==3)
sum(M_10[,-c(1,2)]==4)
sum(M_10[,-c(1,2)]==5)

active_days_M10=data.frame(rowSums(M_10[,-1]))
plot(1:401,jitter(active_days_M10[,1],amount=2),type="p")

threshold=60
sum(active_days_M10>threshold) 


active_regions_M10=data.frame(colSums(M_10[,-1]))
plot(1:625,jitter(active_regions_M10[,1],amount=2),type="p",xlim=c(0,100))
sum(active_regions_M10> 60)
#Variation auch nur f?r m?rz april


###################comparing in plot ###############################
M_14=covid_policy[covid_policy$m_code=="M14",-c(1,5:6)]
M_14=M_14%>% pivot_longer(4:628,names_to="date",values_to="M14")
M_14=M_14%>% mutate("day"=as.Date(M_14$date,format="d%Y%m%d"))

M_17=covid_policy[covid_policy$m_code=="M17",-c(1,5:6)]
M_17=M_17%>%pivot_longer(4:628,names_to="date",values_to="M17")
M_17=M_17%>%mutate("day"=as.Date(M_17$date,format="d%Y%m%d"))

M_8=covid_policy[covid_policy$m_code=="M08",-c(1,5:6)] 
M_8=M_8%>%pivot_longer(4:628,names_to="date",values_to="M08")
M_8=M_8%>%mutate("day"=as.Date(M_8$date,format="d%Y%m%d"))

M_10=covid_policy[covid_policy$m_code=="M10",-c(1,5:6)]
M_10=M_10%>%pivot_longer(4:628,names_to="date",values_to="M10") 
M_10=M_10%>%mutate("day"=as.Date(M_10$date,format="d%Y%m%d"))

data=left_join(M_8,M_10,by=c("ags2","bundesland","ags5","date","day")) %>% left_join(M_14,by=c("ags2","bundesland","ags5","date","day"))%>% left_join(M_17,by=c("ags2","bundesland","ags5","date","day"))
data=relocate(data,day,.before = M08)

res=matrix(data=NA,nrow=16,ncol = 4)
j=1
for(m in colnames(data)[6:9]){
  i=1
  for(b in unique(data$bundesland)){
    res[i,j]=ifelse(!is.null(head(data$day[which(data[data$bundesland==b,m]==1)],1)),head(data$day[which(data[data$bundesland==b,m]==1)],1),0)
    i=i+1
    
  }
  j=j+1
}
res1=data.frame("bundesland"=rep(NA,16),"M08"=rep(NA,16),"M10"=rep(NA,16),"M14"=rep(NA,16),"M17"=rep(NA,16))
res1$bundesland=unique(data$bundesland)
res1$M08=as.Date(res[,1],origin="1970-01-01")
res1$M10=as.Date(res[,2],origin="1970-01-01")
res1$M14=as.Date(res[,3],origin="1970-01-01")
res1$M17=as.Date(res[,4],origin="1970-01-01")

abrv=data.frame("bundesland"=unique(data$bundesland),"Abrv"=c("SH","HH","NI","NW","HE","RP","BW","BY","SL","BE","BB","MV","SN","ST","TH","HB"))

dodge=position_dodge2(width = 16.7) 
ggplot(data=res1,mapping = aes(x = abrv[,2],y=M08))+geom_point(aes(color="Gastronomy (M08)"),size=3)+
  geom_point(aes(y=M10,color="Nightlife activities (M10)"),position=dodge,size=3)+
  labs(x = "State", y = "",
       title ="Day of first activation",
       subtitle = "M08 and M10",
       caption = "own calculations, data from https://www.corona-datenplattform.de",
       colour = "Restrictions on")+
 theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=18),
        legend.text=element_text(size=13))  

ggplot(data=res1,mapping = aes(x = abrv[,2],y=M14))+geom_point(aes(color="Internal mobility (M14)"),size=3)+  
  geom_point(aes(y=M17,color="Workingplace (M17)"),size=3,position=dodge)+
  labs(x = "Federal State", y = "Date of activation",
       title ="Day of first Activation",
       subtitle = "M14 and M18",
       caption = "own calculations, data from https://www.corona-datenplattform.de",
       colour = "Restrictions on")+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=18),
        legend.text=element_text(size=13))  

#install.packages("kableExtra")
library(kableExtra)
res1 %>%
  kbl(caption="First Implementation of policies",
      format= "latex",
      col.names = colnames(res1),
      align="r") %>%
  kable_minimal(full_width = F, html_font = "helvetica")
