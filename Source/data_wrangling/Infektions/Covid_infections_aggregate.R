setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Daten/Basisdaten")
#infektionsdaten
infections=read_csv("infektionen_stand13.03.22.csv")
infections=infections[infections$variable=="kr_inz_rate",] %>% pivot_longer(7:length(infections),names_to="date")
#WARUM NUR 400 ???

dates=infections$date
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
infections$date=dates

infections[infections$value<0,]=NA
infections=na.omit(infections)
colnames(infections)[8]="7_Tage_inzidenz_100k "

setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Daten/Preprocessing")
write.csv(infections,"7 Tage Inzidenz Kreise.csv",)

#DATENSTAND 13.03.2022###############################################

##Gegeben falls noch andere indikatoren für das infektionsgeschehen berücksichtigen