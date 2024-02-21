library(dplyr)
library(readr)
library(ggplot2)
library(mgcv)
library(deSolve)
library(tidyverse)
library(stats)
library(itsadug)
library(data.table)
library(car)
library(broom)

setwd("~/Desktop/Rscripts/Data")

RainChad <- read_csv("Monthly_Rain_Chad.csv",locale=locale(encoding="latin1"))

maxrain = max(RainChad$Precipitation)

#water level breaks 20,40,60,80,100% of the maxrain

RainChad$Precipitation


monthly_seq = c(0,0,0,0,0,1,3,4,3,0,0,0)

#water level vector 
rep(monthly_seq, c(31,28,31,30,31,30,31,31,30,31,30,31)) 


#data source 
https://climateknowledgeportal.worldbank.org/country/chad/climate-data-historical 