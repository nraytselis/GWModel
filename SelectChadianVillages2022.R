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

Ponds2022 <- read_csv("chad_abate_2022_10-27-23.csv",locale=locale(encoding="latin1"))

Ponds2022 <- Ponds2022 %>% mutate(Date = as.Date(treatmentdate, format = "%m/%d/%y")) 

Ponds2022$Month = months(as.Date(Ponds2022$treatmentdate))

Ponds2022numeric <- Ponds2022 %>%
  mutate(Month = as.numeric(factor(Month, levels = month.name)))


#Toukra Mousgoum
TM2022 = filter(Ponds2022numeric,village == "toukra mousgoum") 

TM2022_summary <- TM2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(TM2022_summary) = c("Month", "Village", "Count")

TM2022_summary[TM2022_summary=="toukra mousgoum"] <- "Toukra Mousgoum"

TM2022_summary$Village = as.factor(TM2022_summary$Village) 

TM2022Year = data.frame(c(rep("2022", times = 10)))

ToukraMousgoum = cbind(TM2022_summary,TM2022Year)

colnames(ToukraMousgoum) = c("Month","Village", "Count", "Year")

#KobÈ
kob2022 = filter(Ponds2022numeric,village == "kob") 

kob2022_summary <- kob2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(kob2022_summary) = c("Month", "Village", "Count")

kob2022_summary[kob2022_summary=="kob"] <- "Kob"

kob2022_summary$Village = as.factor(kob2022_summary$Village) 

KobYear2022 = data.frame(c(rep("2022", times = 12)))

Kob = cbind(kob2022_summary,KobYear2022)

colnames(Kob) = c("Month","Village", "Count", "Year")

#Madjira 1
Madjira12022 = filter(Ponds2022numeric,village == "madjira 1") 

Madjira12022_summary <- Madjira12022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(Madjira12022_summary) = c("Month", "Village", "Count")

Madjira12022_summary[Madjira12022_summary=="madjira 1"] <- "Madjira 1"

Madjira12022_summary$Village = as.factor(Madjira12022_summary$Village) 

Madjira2022Year = data.frame(c(rep("2022", times = 12)))

Madjira1 = cbind(Madjira12022_summary,Madjira2022Year)

colnames(Madjira1) = c("Month","Village", "Count", "Year")

#Diganali 
Diganali2022 = filter(Ponds2022numeric,village == "diganali") 

Diganali2022_summary <- Diganali2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(Diganali2022_summary) = c("Month", "Village", "Count")

Diganali2022_summary[Diganali2022_summary=="diganali"] <- "Diganali"

Diganali2022_summary$Village = as.factor(Diganali2022_summary$Village) 

DiganaliYear2022 = data.frame(c(rep("2022", times = 11)))

Diganali = cbind(Diganali2022_summary,DiganaliYear2022)

colnames(Diganali) = c("Month","Village", "Count", "Year")

#Asso2
Asso22022 = filter(Ponds2022numeric,village == "asso 2") 

Asso22022_summary <- Asso22022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(Asso22022_summary) = c("Month", "Village", "Count")

Asso22022_summary[Asso22022_summary=="asso 2"] <- "Asso 2"

Asso22022_summary$Village = as.factor(Asso22022_summary$Village) 

Asso2Year2022 = data.frame(c(rep("2022", times = 12)))

Asso2 = cbind(Asso22022_summary,Asso2Year2022)

colnames(Asso2) = c("Month","Village", "Count", "Year")

#LoumiaCenter
LC2022 = filter(Ponds2022numeric,village == "loumia centre") 

LC2022_summary <- LC2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(LC2022_summary) = c("Month", "Village", "Count")

LC2022_summary[LC2022_summary=="loumia centre"] <- "Loumia Centre"

LC2022_summary$Village = as.factor(LC2022_summary$Village) 

LCYear2022 = data.frame(c(rep("2022", times = 12)))

LoumiaCentre = cbind(LC2022_summary,LCYear2022)

colnames(LoumiaCentre) = c("Month","Village", "Count", "Year")

#Mailao

Mailao2022 = filter(Ponds2022numeric,village == "mailao") 

Mailao2022_summary <- Mailao2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(Mailao2022_summary) = c("Month", "Village", "Count")

Mailao2022_summary[Mailao2022_summary=="mailao"] <- "Mailao"

Mailao2022_summary$Village = as.factor(Mailao2022_summary$Village) 

MailaoYear2022 = data.frame(c(rep("2022", times = 12)))

Mailao = cbind(Mailao2022_summary,MailaoYear2022)

colnames(Mailao) = c("Month","Village", "Count", "Year")

#Kolemara Sara
KS2022 = filter(Ponds2022numeric,village == "kolemara sara") 

KS2022_summary <- KS2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(KS2022_summary) = c("Month", "Village", "Count")

KS2022_summary[KS2022_summary=="kolemara sara"] <- "Kolemara Sara"

KS2022_summary$Village = as.factor(KS2022_summary$Village) 

KS2022 = data.frame(c(rep("2022", times = 12)))

KolemaraSara = cbind(KS2022_summary,KS2022)

colnames(KolemaraSara) = c("Month","Village", "Count", "Year")

#Guizeme 

Guimeze2022 = filter(Ponds2022numeric,village == "guimeze") 

Guimeze2022_summary <- Guimeze2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(Guimeze2022_summary) = c("Month", "Village", "Count")

Guimeze2022_summary[Guimeze2022_summary=="guimeze"] <- "Guimeze"

Guimeze2022_summary$Village = as.factor(Guimeze2022_summary$Village) 

Guimeze2022year = data.frame(c(rep("2022", times = 12)))

Guimeze = cbind(Guimeze2022_summary,Guimeze2022year)

colnames(Guimeze) = c("Month","Village", "Count", "Year")

#Bem Bem-2

bembem2022 = filter(Ponds2022numeric,village == "bem-bem 2") 

bembem2022_summary <- bembem2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(bembem2022_summary) = c("Month", "Village", "Count")

bembem2022_summary[bembem2022_summary=="bem-bem 2"] <- "Bem-Bem 2"

bembem2022_summary$Village = as.factor(bembem2022_summary$Village) 

BemBem2022Year = data.frame(c(rep("2022", times = 12)))

BemBem2 = cbind(bembem2022_summary,BemBem2022Year)

colnames(BemBem2) = c("Month","Village", "Count", "Year")


#Combine Data for All Villages and Analyze
SelectVillages = bind_rows(Diganali,Asso2, Mailao,LoumiaCentre, KolemaraSara,Guimeze,BemBem2,ToukraMousgoum,Kob,Madjira1) 


SelectVillages2022 = SelectVillages %>% filter(Year == 2022)

SelectVillages2022$Year = as.numeric(unlist(SelectVillages2022$Year))

SelectVillages2022$Village = as.factor(SelectVillages2022$Village)

SelectResults2022 = gamm(round(Count) ~ s(Month) + s(Village, bs = "re"),
                         family = quasipoisson(), correlation = corCAR1(form = ~Month | Village),
                         data = SelectVillages2022) 


summary(SelectResults2022$gam) #if edf is close to k, k is too low and model is over-fitted (need more data)

SelectResults2022plot=plot_smooth(SelectResults2022$gam, view="Month",lwd=2,
                                  transform=exp,se=1, shade=T, #ylim = c(0,50),
                                  ylab="Water Body Count",rug=F,
                                  hide.label=T, n.grid = 100)$fv

ggplot() + geom_line(data = SelectResults2022plot,aes(x=Month, y=fit)) +  #fitted line
  geom_ribbon(data = SelectResults2022plot,aes(x=Month, y=fit, ymin = ll,ymax=ul),alpha=0.2) + #recreates plot smooth
  geom_point(data=SelectVillages2022,aes(x=Month,y=Count)) #raw data


