library(feather)
library(haven)
library(tidyverse)

setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Test")

mobdata=read_feather("Test_data.feather") %>% select(c(2:5))
coordinates=read_dta("coordinates_401.dta")

colnames(coordinates)[c(1,4,5)]=c("start_krs","lat_start","lon_start")
data=left_join(mobdata,coordinates[,c(1,4,5)],by="start_krs")

colnames(coordinates)[c(1,4,5)]=c("end_krs","lat_end","lon_end")
data=left_join(data,coordinates[,c(1,4,5)],by="end_krs")

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

dates=unique(data$date)

data2=data[data$date==dates[5],]


p1=ggplot(data2[which(data2$moves>5),], aes(lat_start, lon_start))+
  borders(reg="GER", color="black", fill="black", alpha=1)+
  
  geom_segment(aes(x=lon_start, y=lat_start,xend=lon_end, yend=lat_end, alpha=moves), col="white")+
  
  scale_alpha_continuous(range = c(0.01, 0.2))+
  
  quiet+coord_fixed()+
  
  theme(legend.position="none")+ggtitle("Mobility Flows Pre-Treatment (2020-03-11)")

  
data2=data[data$date==dates[19],]
p2=ggplot(data2[which(data2$moves>5),], aes(lat_start, lon_start))+
  borders(reg="GER", color="black", fill="black", alpha=1)+
  
  geom_segment(aes(x=lon_start, y=lat_start,xend=lon_end, yend=lat_end, alpha=moves), col="white")+
  
  scale_alpha_continuous(range = c(0.01, 0.2))+
  
  quiet+coord_fixed()+
  
  theme(legend.position="none")+ggtitle("Mobility Flows Post-Treatment (2020-03-25)")

p3=ggplot(data2[which(data2$moves>5),], aes(lat_start, lon_start))+
  borders(reg="GER", color="black", fill="black", alpha=1)+
  
  geom_segment(aes(x=lon_start, y=lat_start,xend=lon_end, yend=lat_end, alpha=moves), col="white")+
  
  scale_alpha_continuous(range = c(0.01, 0.1))+
  
  quiet+coord_fixed()+
  
  theme(legend.position="none")+ggtitle("Mobility Flows rebound (2020-04-29)")

library(gridExtra)
grid.arrange(p1,p2,nrow=2)
