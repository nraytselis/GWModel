setwd("~/Desktop/Rscripts/Data")
zoops = read.csv("Abate_time_series.csv")

head(zoops)


library(ggplot2)
library(cowplot)
library(stringr)
library(NatParksPalettes)
library(tidyverse)


##### 
# Abate time series #
ggplot(data=zoops, aes(x=Date, y=Copepod.density+1, group=Site, color=Site)) + geom_point() + geom_line() + 
  scale_y_log10() +
  geom_vline(aes(xintercept=0), linetype="dashed") + theme_cowplot() +
  ylab("Copepod density, #/L")

#####

#####
# Banda wells
Banda = read.csv("Banda_wells.csv")
Banda = subset(Banda, Taxon == "copepod")

Banda_Densities = aggregate(Size ~ Well, FUN=length, data=Banda)


#####
Sarh = read.csv("Sarh_field_survey.csv")
#Sarhdensity <- Sarh %>% select(2,4)
#pal <- park_palette("Olympic")
ggplot(Sarh, aes(x=Habitat.type, y=Copepod.density, color = Habitat.type)) + geom_boxplot() + theme_cowplot() + geom_point(aes(size = 0.5, shape=Habitat.type)) +
  xlab("Habitat type") + ylab("Clarity") + scale_color_brewer(palette="Dark2") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

Sarhclarity <- Sarh %>% select(2,8)
ggplot(Sarh, aes(x=Habitat.type, y=Clarity, color = Habitat.type)) + geom_boxplot() + theme_cowplot() + geom_point(aes(size = 0.5, shape=Habitat.type)) +
  xlab("Habitat type") + ylab("Clarity") + scale_color_brewer(palette="Dark2") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))




### ridgelines ###
Sarh = read.csv("Sarh_field_survey.csv")
ggplot(Sarh, aes(x=log(Clarity*100), y=Copepod.density+0.1, color=Habitat.type)) + theme_cowplot() + geom_point(aes(size = 3,shape=Habitat.type)) +
  xlab("Clarity") + ylab("Copepod density, #/L") + scale_y_log10() + scale_color_brewer(palette="Dark2")  

Sarh2 = Sarh %>% filter(Habitat.type %in% c('River','Pond')) 
ggplot(Sarh2, aes(x=log(Clarity*100), y=Copepod.density+0.1, color = Habitat.type)) + theme_cowplot() + geom_point(aes(size = 3,shape=Habitat.type)) + geom_smooth(method='lm', aes(group=1)) +
  xlab("Clarity, log(Secchi depth, cm") + ylab("Copepod density, #/L (log10)") + scale_y_log10() + scale_color_brewer(palette="Dark2")

