setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Daten/basisdaten")
library(tidyverse)
library(readr)
#m‰rz 2020 bis februar 2022
covid_policy=read_csv("kr_massnahmen_unterkategorien.csv") %>% na.omit() 
#M_14-----------------------------------------------------------------------------------------
M14_codes=c("M14_010","M14_010_1","M14_010_2","M14_010_3","M14_010_4","M14_010_5","M14_020","M14_020_1","M14_020_2","M14_020_3", "M14_020_4", "M14_020_5" ,"M14_030" ,"M14_030_1", "M14_030_2", "M14_030_3", "M14_030_4", "M14_030_5")
M_14=covid_policy[covid_policy$code %in% M14_codes ,-c(1:3,5)]

hetfunc_days=function(data,codename){
 result=data.frame(rowSums(data[data$code==codename,-c(1,2)]))
 return(result)
}
hetfunc_regions=function(data,codename){
  result=data.frame(colSums(data[data$code==codename,-c(1,2)]))
  return(result)
}

# wir omitten alle variablen, in denen die Eintragung - 99 vorkommt (Maﬂnahmen, die nichtmehr verwendet werden)
#das sind hier immer die Maﬂnahmen, die erst ab 50 bzw 100 neuinfektionen pro 100k einwohner greifen

M_14=replace(M_14,M_14==-99,NA) %>% na.omit()
#vorl‰ufig setze ich alle werte, die 2 oder 3 sind auf 1, da hier die maﬂnahme auch aktiv ist, aber gg.f aus der Kreisverordnung stammt oder imputiert von den Bundesland ist
M_14=replace(M_14,M_14==2,1) 
M_14=replace(M_14,M_14==3,1)

#anpassen der M14_codes
M14_codes=c("M14_010","M14_010_1","M14_010_2","M14_020","M14_020_1","M14_020_2" ,"M14_030" ,"M14_030_1", "M14_030_2")

par(mfrow=c(1,3))
for( code in M14_codes){
  assign(paste("active_days_",code,sep=""),hetfunc_days(M_14,code))
  plot(1:401,hetfunc_days(M_14,code)[,1],type="p",ylab=paste("number active days ",code))
}
#----------------------------------------------------------------------------------------------------------
for( code in M14_codes){
  assign(paste("active_regions_",code,sep=""),hetfunc_regions(M_14,code))
  plot(1:717,hetfunc_regions(M_14,code)[,1],type="p",ylab=paste("number regions with",code,"active"))
}
par(mfrow=c(1,1))

#Beobachtung 

#Das Maﬂnahmen geschehen scheint durch M14_020 dominiert zu werden, die Maﬂnahmeb, die abh‰ngig von den neuinfektionen sind ( _1,_2) sind nicht genutzt worden bzw. werden nicht mehr benutzt.