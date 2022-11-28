library(rvest)
library(tidyverse)
library(readr)
setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Daten/Basisdaten")
OSM_IDs <- read_csv("OSM IDs.csv")

urls=data.frame("URLS"=rep(NA,513))
for(i in 1:513){
  urls[i,1]=paste("https://www.openstreetmap.org/relation/",OSM_IDs[i,1],sep="")
}

data=data.frame(rep(NA,513),"gemeindeschlüssel"=rep(NA,513))
for(j in 1:513){
  tab=read_html(urls[j,1]) %>% html_node("table") %>% html_table()
  #data[j,1]=read_html(urls[j,1]) %>% html_node("h2") %>% html_text()
  data[j,1]=OSM_IDs[j,1]
  data[j,2]=tab[tab$X1=="name",2]
  data[j,3]=tab[tab$X1=="de:regionalschluessel",2]
  
}

colnames(data)=c("rel_id","kreis","ags5")
raw=data

data=raw
data=type.convert(data)
for(i in 1:513){
  if(data[i,3]>1000000000)
    data[i,3]=data[i,3]/10000000
}
raw_id=read_csv("raw_OSMID_teralytics.csv")

result=data.frame("raw_id"=raw_id[,2],data)
for(i in 1:513){
  result[i,1]=paste("00",result[i,1],sep = "")
}

colnames(result)[1]="raw_ID"
setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Daten/Preprocessing")
write.csv(result,"OSMID_to_AGS5.csv",row.names = FALSE,fileEncoding = "UTF-8")
