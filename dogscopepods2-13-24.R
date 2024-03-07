library(ggplot2)
library(cowplot)
library(stringr)
library(NatParksPalettes)
library(tidyverse)

setwd("~/Desktop/Rscripts/Data")
Dogs <- read_csv("PercentCopepodsConsumedDogs.csv",locale=locale(encoding="latin1"))

ggplot(data=Dogs, aes(x=Copepods, y=Percent*100)) + geom_point(size=8) + ylab("Average Percent Copepods Consumed (%)") + xlab("Initial Copepod Density (L)") +  theme_minimal() + 
  scale_y_continuous(limits = c(0, 100)) + labs(title = "Average Percent Copepods Consumed \nby Dogs") + theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  ) + theme(plot.title=element_text(size=40))
