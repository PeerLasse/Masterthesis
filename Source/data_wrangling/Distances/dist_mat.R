setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Code")
library(osrm)
library(tidyverse)
library(haven)
#import data
data= read_dta("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Code/coordinates_401.dta") #as.data.frame(read_csv("Kreissitz_Kreisname_koordinaten.csv")) %>% distinct()
#point towards local server
options(osrm.server = "http://127.0.0.1:5000/")
#which data to use
table_data=data %>% select(krs,longitude,latitude)
#create table 
table=osrmTable(src=table_data,dst=table_data,osrm.profile = "car")
distmat=table$durations
distmat=cbind("Kennziffer"=colnames(distmat),distmat)

table_km=osrmTable(src=table_data,dst=table_data,osrm.profile = "car",measure="distance")
distmat_km=table_km$distances/1000
distmat_km=cbind("Kennziffer"=colnames(distmat_km),distmat_km)

setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Daten/Basisdaten")
write_csv(as.data.frame(distmat),"dist_mat_401_durations.csv")
write_csv(as.data.frame(distmat_km),"dist_mat_401_distance.csv")
