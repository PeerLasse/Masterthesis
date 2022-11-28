library(readr)
library(readxl)
library(tidyverse)
library(spdep)
library(spatialreg)
library(plm)
library(splm)
library(fixest)
library(feather)
setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/data/processed")
source("C:/Users/peerl/OneDrive/Desktop/Studium/Masterarbeit/Masterthesis/source/functions/dist_approx.R")
############################ Load Data #########################################
data=read_feather("V2SLX_data_gravitation.feather")

data_reduced=data %>% select(c(1:6,9:12,17,19:23,31,37,47:49))

library(stargazer)
stargazer(as.data.frame(data_reduced[,-c(1:4)]),summary = TRUE,summary.stat = c("n","mean","sd","median","p25","p75","min","max"))

