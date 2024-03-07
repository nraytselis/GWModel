library(deSolve) 
library(ggplot2) 
library(tidyr)
library(ggsci)
library(wesanderson)
library(scales)
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

dogdiet <- read_csv("plot-data.csv",locale=locale(encoding="latin1"))

dogdiet = as.data.frame((dogdiet))

ggplot(data = dogdiet, aes(x = dogdiet$`Food Type`, y = dogdiet$`Mean % Diet`, fill= dogdiet$`Food Type`)) + geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Food Type", y = "Mean % of Diet",title = "Chadian Dog Food Types as Mean Percent of Diet") +  # Label axes
  theme_minimal() + scale_fill_manual(values = wes_palette("Darjeeling1", 10, type = "continuous"))+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  ) + theme(plot.title=element_text(size=40))
  # Customize plot theme


fishcopepods <- read_csv("FishCopepods.csv",locale=locale(encoding="latin1"))

fishcopepods = as.data.frame((fishcopepods))

ggplot(data = fishcopepods, aes(x = fishcopepods$FishType, y = fishcopepods$CopepodsIngested, fill= fishcopepods$FishType)) + geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Fish Species", y = "Copepods Ingested",title = "Chadian Fish Species Consume Copepods") +  # Label axes
  theme_minimal() + scale_fill_manual(values = wes_palette("Darjeeling1", n = 5)) + theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  ) + theme(plot.title=element_text(size=40)) +  scale_x_discrete(label = function(x) stringr::str_trunc(x, 12))# Customize plot theme


Infections <- read_csv("FishInfectionData.csv",locale=locale(encoding="latin1"))

Infections$RatioInfected = Infections$`Infection status (No. positive)`/Infections$`No. hosts`

Infections = Infections %>% slice(1:4)

ggplot(data = Infections, aes(x =  reorder(Infections$`Host species`, -RatioInfected), y = Infections$RatioInfected, fill= Infections$`Host species`)) + geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Host Species", y = "Percent Infected",title = "Susceptibility of Host Species \nto Dracunculiasis Mendinensis") +  # Label axes
  theme_minimal() + scale_fill_manual(values = wes_palette("Darjeeling1", n = 4)) + theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  ) + theme(plot.title=element_text(size=40)) # Customize plot theme

