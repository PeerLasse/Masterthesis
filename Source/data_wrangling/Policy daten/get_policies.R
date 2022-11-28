library(tidyverse)
Maﬂnahmen <-read_csv("kr_massnahmen_oberkategorien03_20_15_11_21.csv") %>% na.omit() %>% type.convert()

#---------------------------------------------------------------------------------------------------------------------
#M14 zuschneiden
 M14<-Maﬂnahmen[Maﬂnahmen$m_code=="M14",-c(1:3)]
 M17<-Maﬂnahmen[Maﬂnahmen$m_code=="M17",-c(1:3)]
 M08<-Maﬂnahmen[Maﬂnahmen$m_code=="M08",-c(1:3)]
 M10<-Maﬂnahmen[Maﬂnahmen$m_code=="M10",-c(1:3)]
 #date format anpassen
 dates=colnames(M14)[4:length(M14)]
 dates=gsub("d","",dates)
 dates=gsub("2020","2020_",dates)
 dates=gsub("2021","2021_",dates)
 dates=gsub("2022","2022_",dates)
 for(y in c("2020_","2021_","2022_")){
   for( i in 1:12){
     if(i<10){
     dates=gsub(paste(y,"0",as.character(i),sep=""),paste(y,"0",as.character(i),"_",sep=""),dates)
     }
     else{
       dates=gsub(paste(y,as.character(i),sep=""),paste(y,as.character(i),"_",sep=""),dates)
     }
   }
 }
 colnames(M14)[4:length(M14)]=dates
 colnames(M17)[4:length(M14)]=dates
 colnames(M10)[4:length(M14)]=dates
 colnames(M08)[4:length(M14)]=dates
 
 
 #transform to longformat
 M14_long=M14 %>% pivot_longer(cols=`2020_03_01`:`2021_11_15`,names_to="date") %>% relocate(date,before=ags5)
 M17_long=M17 %>% pivot_longer(cols=`2020_03_01`:`2021_11_15`,names_to="date") %>% relocate(date,before=ags5)
 M10_long=M10 %>% pivot_longer(cols=`2020_03_01`:`2021_11_15`,names_to="date") %>% relocate(date,before=ags5)
 M08_long=M08 %>% pivot_longer(cols=`2020_03_01`:`2021_11_15`,names_to="date") %>% relocate(date,before=ags5)
# setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Daten/Preprocessing")
 write.csv(M14_long,"M14.csv")
 write.csv(M17_long,"M17.csv")
 write.csv(M10_long,"M10.csv")
 write.csv(M08_long,"M08.csv")
