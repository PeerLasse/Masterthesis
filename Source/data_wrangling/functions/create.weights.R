library(readr)
library(tidyverse)
library(spdep)
library(plm)
library(splm)
library(Matrix)
setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/data/raw")
dist=read_csv("dist_mat_401_durations.csv")
W_matrix=function(dist,thresh){
  weights=dist
  for (i in 1:dim(dist)[1]){
    for (j in 1:dim(dist)[2]){
      if (i==j){
        #not neighbour of oneself
        weights[i,j]=0
      }
      else if(dist[i,j]>thresh){
        weights[i,j]=0
      }
      else{
        weights[i,j]=1
      }
    }
  }
  rowtotals=rowSums(weights)
  weights=weights/rowtotals 
  return(as.matrix(weights))
}
#exclude outdated county
distances=dist[dist$Kennziffer!=16056,-c(1,385)]

#get weights for threshold of 45 min
Weights_nb=W_matrix(dist=distances,thresh=90)
W2=Matrix(Weights_nb,sparse=T)
I=Matrix(diag(400),sparse=T)
#origin based dependence
Weight_o= W2 %x% I
#destination based dependence
Weight_d= I %x% W2
rm(dist,W_matrix,distances,Weights_nb,W2,I)
