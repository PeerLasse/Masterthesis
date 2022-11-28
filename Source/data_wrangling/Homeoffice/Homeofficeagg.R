library(readr)
library(readxl)
library(tidyverse)
library(spdep)
library(spatialreg)
library(plm)
library(splm)
library(fixest)
library(feather)
Reg_home <- read_excel("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/data/raw/Regionale Homeoffice Anteile.xls")
Reg_home <- mutate(Reg_home,"AGS5"=as.numeric(gsub("([0-9]+).*$", "\\1", Reg_home$krs)),.before = krs)
Reg_home <- select(Reg_home,-2)

# To adjust to 400 counties, we 
# Sum up the number for
# -	krs_WZ_gesamt: regionale Gesamtbeschäftigung basierend auf Beschäftigung nach Wirtschaftszweigen (WZ)
# -	krs_homeoffice_WZ: Arbeitsplätze, die in der Region potentiell homeofficefähig sind, basierend auf Analyse nach WZ
# and tage mean for 
# -	krsanteil_homeoffice_WZ: Anteil der homofficefähigen Arbeitsplätze in der Region basierend auf WZ-Analyse des Homeofficepotentials
# for Eisenach and Wartburgkreis
Reg_home[Reg_home$AGS5==16063,c(2,3,5,6)]=Reg_home[Reg_home$AGS5==16063,c(2,3,5,6)]+Reg_home[Reg_home$AGS5==16056,c(2,3,5,6)]
Reg_home$krsanteil_homeoffice_WZ=Reg_home$krs_homeoffice_WZ/Reg_home$krs_WZ_gesamt
Reg_home$krsanteil_homeoffice_KldB=Reg_home$krs_homeoffice_KldB/Reg_home$krs_KldB_gesamt
Reg_home=Reg_home[Reg_home$AGS5!=16056,]

setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/data/raw")
write.csv(Reg_home,"Homeoffice_potential_400Kreise.csv",fileEncoding = "UTF8",row.names = FALSE)    
