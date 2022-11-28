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
#transforming distance
data$distance=ifelse(data$distance!=0,data$distance,approx_intra_distance(data))

p1=ggplot(data,aes(x=moves))+geom_histogram(bins = 60)+
  scale_x_log10()+
  labs(title="Distribution of moves")+
  xlab("moves on log scale")
sum1=summary(data$moves)

p2=ggplot(data[data$start_krs!=data$end_krs,],aes(x=moves))+geom_histogram(bins = 60)+
  scale_x_log10()+
  labs(title="Distribution of inter county moves")+
  xlab("moves on log scale")
sum2=summary(data$moves[data$start_krs!=data$end_krs])

p3=ggplot(data[data$start_krs==data$end_krs,],aes(x=moves))+geom_histogram(bins = 60)+
  scale_x_log10()+
  labs(title="Distribution of intra county moves")+
  xlab("moves on log scale")
sum3=summary(data$moves[data$start_krs==data$end_krs])

################################not logged axis##############################
p4=ggplot(data,aes(x=moves))+geom_histogram(bins = 60)+
  labs(title="Distribution of moves")+
  xlab("moves on count scale")


p5=ggplot(data[data$start_krs!=data$end_krs,],aes(x=moves))+geom_histogram(bins = 60)+
  labs(title="Distribution of inter county moves")+
  xlab("moves on countscale")

p6=ggplot(data[data$start_krs==data$end_krs,],aes(x=moves))+geom_histogram(bins = 60)+
  labs(title="Distribution of intra county moves")+
  xlab("moves on count scale")
library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

sums=data.frame("All observations"=as.vector(sum1),"Inter county mobility"=as.vector(sum2),"Intra county mobility"=as.vector(sum3),row.names = names(sum1) )

#finding counties with minimum and maximum number of moves
intra=data[data$start_krs==data$end_krs,]
unique(intra$start_krs[intra$moves==min(intra$moves)])
unique(intra$start_krs[intra$moves==max(intra$moves)])

dates=unique(data$date)
ggplot(data[data$date %in% dates[1],],aes(y=moves,x=distance))+
  geom_point()+
  scale_y_log10()+
  geom_smooth(method=lm)+
  labs(title="Number of trips and distance",subtitle = "07.03.2020")+
  ylab("moves (log scale)")+
  xlab("time needed to travel in minutes")+theme_bw()

#correlation plot
library(reshape2)
corr_data=data %>% select(c(moves,distance,Inzidenz_start,Inzidenz_end,AFD_start,AFD_end,Linke_start,Linke_end,bevoelkerung_start,bevoelkerung_end,startanteil_homeoffice_WZ,endanteil_homeoffice_WZ))
cormat=round(cor(corr_data),2)
cor_melted=melt(cormat)
corr_plot=ggplot(data = cor_melted, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
corr_plot

