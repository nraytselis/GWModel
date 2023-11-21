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

setwd("~/Desktop/CivitelloRScipts")

Ponds2019 <- read_csv("AbateLineList_2019_8-17-23-NR.csv",locale=locale(encoding="latin1"))
Ponds2020 <- read_csv("Abate2020.csv",locale=locale(encoding="latin1"))
Ponds2021 <- read_csv("Abate2021data.csv",locale=locale(encoding="latin1"))
Ponds2022 <- read_csv("chad_abate_2022_10-27-23.csv",locale=locale(encoding="latin1"))

#list of villages from Stephanie's dataset present as far back as 2019:
#Bem-Bem 2
#Guimeze
#Digangali
#Asso 2
#Mailao
#Loumia Centre
#Kolemara Sara


Ponds2019Stephanie = filter(Ponds2019,Village =="Bem-Bem 2"| Village =="Guiemeze"| Village =="Digangali"| Village =="Asso 2"| Village =="Mailao"| Village =="Loumia Centre"| Village =="Kolemara Sara")
Ponds2020Stephanie = filter(Ponds2020,Village =="Bem-Bem 2"| Village =="Guiemeze"| Village =="Digangali"| Village =="Asso 2"| Village =="Mailao"| Village =="Loumia Centre"| Village =="Kolemara Sara")
Ponds2022Stephanie = filter(Ponds2022,village =="bem-bem 2"| village =="guiemeze"| village =="digangali"| village =="asso 2"| village =="mailao"| village =="loumia centre"| village =="kolemara sara")

#fix alignment issues with 2021 data
#fix alignment issue 1
month_names = c("Janvier", "Fevrier", "Mars", "Avril", "Mai","Juin","Juillet", "Aout","Septembre","Octobre", "Novembre", "Decembre")

Ponds2021$Waterbody = ifelse(Ponds2021$`Nom de la source d'eau` %in% month_names, Ponds2021$`Niveau de surveillance`, Ponds2021$`Nom de la source d'eau`)

#fix alignment issue 2 (import list of villages from 2021 from other tab in Donnees Abate_Decembre_2021 excel file and compare with dataframe)

Villages2021 <- read_csv("Villages_2022_Donnees Abate_Decembre_2021_Village_eligible_2021_update_10-31-23.csv",locale=locale(encoding="latin1"))

Ponds2021$CorrectedVillages = ifelse(Ponds2021$`Zone` %in% Villages2021, Ponds2021$`Village`, Ponds2021$`Zone`)

#then filter for only Stephanie's villages:
Ponds2021Stephanie = filter(Ponds2021,Village =="Bem-Bem 2"| Village =="Guiemeze"| Village =="Digangali"| Village =="Asso 2"| Village =="Mailao"| Village =="Loumia Centre"| Village =="Kolemara Sara")

#make sure all data frames have the same water body column
Ponds2019Stephanie = Ponds2019Stephanie %>% rename(WaterBody = "Water Body") 
Ponds2020Stephanie = Ponds2020Stephanie %>% rename(WaterBody = "Nom de la source d'eau") 
Ponds2021Stephanie = Ponds2021Stephanie %>% rename(WaterBody = "Waterbody") 
Ponds2022Stephanie = Ponds2022Stephanie %>% rename(WaterBody = "watername") 

#compare villages year-to-year 

#just look at Bem-Bem 2
Ponds2019BemBem2 = filter(Ponds2019Stephanie,Village =="Bem-Bem 2") 
Ponds2020BemBem2 = filter(Ponds2020Stephanie,Village =="Bem-Bem 2") 
Ponds2021BemBem2 = filter(Ponds2021Stephanie,Village =="Bem-Bem 2") 
Ponds2022BemBem2 = filter(Ponds2022Stephanie,village =="bem-bem 2") 

#select columns for Village and WaterBody 
Ponds2019BemBem2 = Ponds2019BemBem2 %>% select(1:2) 
Ponds2020BemBem2 = Ponds2020BemBem2 %>% select(10,15) 
Ponds2021BemBem2 = Ponds2021BemBem2 %>% select(7,32) 
Ponds2022BemBem2 = Ponds2022BemBem2 %>% select(4,7) 

#uppercase all for consistency 
Ponds2019BemBem2$WaterBody = toupper(Ponds2019BemBem2$WaterBody)
Ponds2020BemBem2$WaterBody = toupper(Ponds2020BemBem2$WaterBody)
Ponds2021BemBem2$WaterBody = toupper(Ponds2021BemBem2$WaterBody)
Ponds2022BemBem2$WaterBody = toupper(Ponds2022BemBem2$WaterBody)

#rename so easier to run
A <- Ponds2019BemBem2$WaterBody 
B <- Ponds2020BemBem2$WaterBody 
C <- Ponds2021BemBem2$WaterBody 
D <- Ponds2022BemBem2$WaterBody 


#how similar are WB names across years in same village?
library(stringdist)
distance1=stringsimmatrix(A,B, method="jw")
valid_distances <- distance1[which(!is.na(distance1))]
top_n_indices <- order(valid_distances)[1:5]
#top_n_matches <- A[top_n_indices]

#all in one data frame
BemBem2 <- read_csv("bem-bem data.csv",locale=locale(encoding="latin1"))

#find common ponds between years
intersect(BemBem2$"2019",BemBem2$"2020")
intersect(BemBem2$"2019",BemBem2$"2021")
intersect(BemBem2$"2019",BemBem2$"2022")



library(stringdist)
distances=stringsimmatrix(BemBem2$"2019",BemBem2$"2020", method="jw")
distancesnoNAs<- distances[1:59,]
distancesnoNAs[is.na(distancesnoNAs)] = 0
results <- pheatmap(distancesnoNAs)
maxlist <- apply(distancesnoNAs, 2, max)

distancesnoNAs.clust <- cbind(distancesnoNAs, 
                      cluster = cutree(results$tree_row, 
                                       k = 10))
head(distancesnoNAs.clust)




#####11-13-23
Ponds2019BemBem2 = filter(Ponds2019,Village =="Bem-Bem 2")
Ponds2019BemBem2$`Water Body` = toupper(Ponds2019BemBem2$`Water Body`)
unique(Ponds2019BemBem2$`Water Body`)
names(which.max(table(Ponds2019BemBem2$`Water Body`))) #Bouta Ramadan is most common in 2019, but only present in 5 months 
Ponds2019BemBem2_BoutaCarriere = filter(Ponds2019BemBem2,`Water Body` == "BOUTA CARRIERE") #only present in 2 months in 2019

Ponds2020BemBem2 = filter(Ponds2020Stephanie,Village =="Bem-Bem 2") 
Ponds2020BemBem2$`WaterBody` = toupper(Ponds2020BemBem2$`WaterBody`)
unique(Ponds2020BemBem2$`WaterBody`)
Ponds2020BemBem2_BoutaCarriere = filter(Ponds2020BemBem2,`WaterBody` == "BOUTA CARRIERE" |`WaterBody` == "BOUTA CARRIERE1")

Ponds2022BemBem2 = filter(Ponds2022Stephanie,Village =="bem-bem 2") 
Ponds2022BemBem2$`WaterBody` = toupper(Ponds2022BemBem2$`WaterBody`)
unique(Ponds2022BemBem2$`WaterBody`)
names(which.max(table(Ponds2022BemBem2$`WaterBody`)))
Ponds2022BemBem2_BoutaCarrier = filter(Ponds2022BemBem2,`WaterBody` == "BOUTA CARRI\u008fRE" |`WaterBody` == "BOUTA CARRI\u008fRE 1")


###Diganali 

#2019

Ponds2019 <- Ponds2019 %>% mutate(Date = as.Date(Date, format = "%m/%d/%y")) 

Ponds2019$Month = months(as.Date(Ponds2019$Date))

Ponds2019numeric = Ponds2019 %>%
  mutate(Month = recode(Month,
                        January = 1,
                        February = 2,
                        March = 3,
                        April = 4,
                        May = 5,
                        June = 6,
                        July = 7,
                        August = 8,
                        September = 9,
                        October = 10,
                        November = 11,
                        December = 12))

Diganali2019 = filter(Ponds2019numeric,Village == "Diganali") 
Diganali2019$`Water Body` = toupper(Diganali2019$`Water Body`)
unique(Diganali2019$`Water Body`)
names(which.max(table(Diganali2019$`Water Body`)))

Diganali2019_summary <- Diganali2019 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

Diganali2019_summary$Village = as.factor(Diganali2019_summary$Village) 

Diganali2019Results=gamm(round(Count)~ s(Month,k=6), #can remove k=6 when more than 6 months, want to keep k ~10
              family = quasipoisson(), correlation=corCAR1(form=~Month|Village),
              data=Diganali2019_summary)

summary(Diganali2019Results$gam) #if edf is close to k, k is too low and model is over-fitted (need more data)

plot_smooth(x=Diganali2019Results$gam, view="Month", )

Diganali2019Resultsplot=plot_smooth(Diganali2019Results$gam, view="Month",lwd=2,
                             transform=exp,se=1, shade=T, #ylim = c(0,50),
                             ylab="Water Body Count",rug=F,
                             hide.label=T, n.grid = 100)$fv

ggplot() + geom_line(data = Diganali2019Resultsplot,aes(x=Month, y=fit)) +  #fitted line
  geom_ribbon(data = Diganali2019Resultsplot,aes(x=Month, y=fit, ymin = ll,ymax=ul),alpha=0.2) + #recreates plot smooth
  geom_point(data=Diganali2019_summary,aes(x=Month,y=Count)) #raw data

#2020
Ponds2020 <- Ponds2020 %>% mutate(Date = as.Date(Date, format = "%m/%d/%y")) 

Ponds2020$Month = months(as.Date(Ponds2020$Date))

Ponds2020numeric = Ponds2020 %>%
  mutate(Month = recode(Month,
                        January = 1,
                        February = 2,
                        March = 3,
                        April = 4,
                        May = 5,
                        June = 6,
                        July = 7,
                        August = 8,
                        September = 9,
                        October = 10,
                        November = 11,
                        December = 12))

Diganali2020 = filter(Ponds2020numeric,Village == "Diganali") 
Diganali2020$`Water Body` = toupper(Diganali2020$"Nom de la source d'eau")
unique(Diganali2020$`Water Body`)

Diganali2020_summary <- Diganali2020 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

Diganali2020_summary$Village = as.factor(Diganali2020_summary$Village) 

DiganaliWaterBodies = bind_rows(Diganali2019_summary,Diganali2020_summary)

Months2020 = data.frame(c(14,15,16,17,18,19,20,21,23,24)) #replace with months since january 2019
colnames(Months2020) = "CorrectedMonths"
DiganaliWaterBodies[9:18,1] <- Months2020

DiganaliWaterBodiesResults=gamm(round(Count)~ s(Month,k=10), #can remove k=6 when more than 6 months, want to keep k ~10
                         family = quasipoisson(), correlation=corCAR1(form=~Month|Village),
                         data=DiganaliWaterBodies)

summary(DiganaliWaterBodiesResults$gam) #if edf is close to k, k is too low and model is over-fitted (need more data)

DiganaliWaterBodiesResultsplot=plot_smooth(DiganaliWaterBodiesResults$gam, view="Month",lwd=2,
                                    transform=exp,se=1, shade=T, #ylim = c(0,50),
                                    ylab="Water Body Count",rug=F,
                                    hide.label=T, n.grid = 100)$fv

ggplot() + geom_line(data = DiganaliWaterBodiesResultsplot,aes(x=Month, y=fit)) +  #fitted line
  geom_ribbon(data = DiganaliWaterBodiesResultsplot,aes(x=Month, y=fit, ymin = ll,ymax=ul),alpha=0.2) + #recreates plot smooth
  geom_point(data=DiganaliWaterBodies,aes(x=Month,y=Count)) #raw data

#2021

#more data cleaning, some are in village column and others in zone 
DIGANALI2021Zone = Ponds2021 %>% filter(Ponds2021$Zone == "Diganali")
DIGANALI2021Village = Ponds2021 %>% filter(Ponds2021$Village == "Diganali") 
Diganali2021 = bind_rows(DIGANALI2021Zone,DIGANALI2021Village)

Diganali2021 <- Diganali2021 %>% mutate(Date = as.Date(Date, format = "%m/%d/%y")) 

Diganali2021$Month = months(as.Date(Diganali2021$Date))

Diganali2021numeric = Diganali2021 %>%
  mutate(Month = recode(Month,
                        January = 1,
                        February = 2,
                        March = 3,
                        April = 4,
                        May = 5,
                        June = 6,
                        July = 7,
                        August = 8,
                        September = 9,
                        October = 10,
                        November = 11,
                        December = 12))

month_names = c("Janvier", "Fevrier", "Mars", "Avril", "Mai","Juin","Juillet", "Aout","Septembre","Octobre", "Novembre", "Decembre")
Diganali2021numeric$Waterbody = ifelse(Diganali2021numeric$`Nom de la source d'eau` %in% month_names, Diganali2021numeric$`Niveau de surveillance`, Diganali2021numeric$`Nom de la source d'eau`)
Diganali2021numeric$`Water Body` = toupper(Diganali2021numeric$"Waterbody")
unique(Diganali2021numeric$`Water Body`)

Diganali2021_summary <- Diganali2021numeric %>%
  group_by(Month) %>%  #ignore column for village because names of villages in both zone and village column, but all Diganali
  summarise(Count = n()) 

Diganali = rep("Diganali", times = 11)
as.data.frame(Diganali)

Diganali2021_summary = cbind(Diganali,Diganali2021_summary) #add a column for Diganali before combining with data from 2019 and 2020
colnames(Diganali2021_summary) = c("Village","Month", "Count" )
Diganali2021_summary$Village = as.factor(Diganali2021_summary$Village) 
DiganaliWaterBodies = bind_rows(Diganali2019_summary,Diganali2020_summary,Diganali2021_summary)
Months2020 = data.frame(c(14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,36)) #replace with months since january 2019
colnames(Months2020) = "CorrectedMonths"
DiganaliWaterBodies[9:29,1] <- Months2020

#run gam for Diganali 2019-2021
DiganaliWaterBodiesResults=gamm(round(Count)~ s(Month,k=10), #can remove k=6 when more than 6 months, want to keep k ~10
                                family = quasipoisson(), correlation=corCAR1(form=~Month|Village),
                                data=DiganaliWaterBodies)

summary(DiganaliWaterBodiesResults$gam) #if edf is close to k, k is too low and model is over-fitted (need more data)

DiganaliWaterBodiesResultsplot=plot_smooth(DiganaliWaterBodiesResults$gam, view="Month",lwd=2,
                                           transform=exp,se=1, shade=T, #ylim = c(0,50),
                                           ylab="Water Body Count",rug=F,
                                           hide.label=T, n.grid = 100)$fv

ggplot() + geom_line(data = DiganaliWaterBodiesResultsplot,aes(x=Month, y=fit)) +  #fitted line
  geom_ribbon(data = DiganaliWaterBodiesResultsplot,aes(x=Month, y=fit, ymin = ll,ymax=ul),alpha=0.2) + #recreates plot smooth
  geom_point(data=DiganaliWaterBodies,aes(x=Month,y=Count)) #raw data

#2022
Ponds2022 <- Ponds2022 %>% mutate(Date = as.Date(treatmentdate, format = "%m/%d/%y")) 

Ponds2022$Month = months(as.Date(Ponds2022$treatmentdate))

Ponds2022numeric = Ponds2022 %>%
  mutate(Month = recode(Month,
                        January = 1,
                        February = 2,
                        March = 3,
                        April = 4,
                        May = 5,
                        June = 6,
                        July = 7,
                        August = 8,
                        September = 9,
                        October = 10,
                        November = 11,
                        December = 12))

Diganali2022 = filter(Ponds2022numeric,village == "diganali") 

Diganali2022_summary <- Diganali2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(Diganali2022_summary) = c("Month", "Village", "Count")

Diganali2022_summary[Diganali2022_summary=="diganali"] <- "Diganali"

Diganali2022_summary$Village = as.factor(Diganali2022_summary$Village) 


DiganaliWaterBodies = bind_rows(Diganali2019_summary,Diganali2020_summary,Diganali2021_summary,Diganali2022_summary)
#Months = data.frame(c(14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,36,37,39,40,41,42,43,44,45,46,47,48)) #replace with months since january 2019
#colnames(Months) = "CorrectedMonths"
#DiganaliWaterBodies[9:40,1] <- Months

#run gam for Diganali 2019-2022
DiganaliWaterBodiesResults=gamm(round(Count)~ s(Month,k=10), #can remove k=6 when more than 6 months, want to keep k ~10
                                family = quasipoisson(), correlation=corCAR1(form=~Month|Village),
                                data=DiganaliWaterBodies)

summary(DiganaliWaterBodiesResults$gam) #if edf is close to k, k is too low and model is over-fitted (need more data)

DiganaliWaterBodiesResultsplot=plot_smooth(DiganaliWaterBodiesResults$gam, view="Month",lwd=2,
                                           transform=exp,se=1, shade=T, #ylim = c(0,50),
                                           ylab="Water Body Count",rug=F,
                                           hide.label=T, n.grid = 100)$fv

ggplot() + geom_line(data = DiganaliWaterBodiesResultsplot,aes(x=Month, y=fit)) +  #fitted line
  geom_ribbon(data = DiganaliWaterBodiesResultsplot,aes(x=Month, y=fit, ymin = ll,ymax=ul),alpha=0.2) + #recreates plot smooth
  geom_point(data=DiganaliWaterBodies,aes(x=Month,y=Count)) #raw data

#write.csv(DiganaliWaterBodies,"~/Desktop/CivitelloRScipts/DiganaliWaterBodies_11-16-23.csv")

DiganaliWaterBodies

#create Year column 
Years = data.frame(c(rep("2019", times = 8), rep("2020", times = 10), rep("2021", times = 11), rep("2022", times = 11))) 

#add to DigangaliWaterBodies 
DiganaliWaterBodiesYears = cbind(DiganaliWaterBodies,Years)
colnames(DiganaliWaterBodiesYears) = c("Month","Village", "Count", "Year")

#run gams with different splines
#https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/


DiganaliWaterBodiesYears$Year = as.numeric(unlist(DiganaliWaterBodiesYears$Year))
#DiganaliWaterBodiesYears["Year"] = as.numeric(DiganaliWaterBodiesYears["Year"]) #make year continuous


#te(x, z) includes both the smooth main effects of x and z, plus their smooth interaction. ti() is just the pure smooth interaction of x and z.
#https://stats.stackexchange.com/questions/519433/gam-and-multiple-continuous-continuous-interactions-tensor-smooths
DiganaliWaterBodiesResults <- gamm(round(Count) ~ te(Month, Year, k = c(6, 4), bs = c("ps", "ps")), 
                                   family = quasipoisson(), correlation = corARMA(form = ~ 1|Year, p = 1),
                                   data = DiganaliWaterBodiesYears) #correlation = corCAR1(form = ~Month | Year | Village)
te(Month,Year)

summary(DiganaliWaterBodiesResults$gam)

plot(DiganaliWaterBodiesResults$gam) #interaction plot

plot(DiganaliWaterBodiesResults$gam, scheme = 2)

datas <- rbindlist(list(DiganaliWaterBodiesYears[, .(Count, date_time)],
                        data.table(Count = DiganaliWaterBodiesResults$fitted.values,
                                   data_time = DiganaliWaterBodiesYears[, date_time])))


#https://drmowinckels.io/blog/2019/plotting-gamm-interactions-with-ggplot2/

#https://campus.datacamp.com/courses/nonlinear-modeling-with-generalized-additive-models-gams-in-r/spatial-gams-and-interactions?ex=4 
#how to interpret

smooth_interact <- gamm(round(Count) ~s(Month, Year),
                        family = quasipoisson(), correlation = corARMA(form = ~ 1|Year, p = 1),
                        data = DiganaliWaterBodiesYears)

plot(smooth_interact$gam)
 
# ggplot() + geom_line(data = DiganaliWaterBodiesResultsplotMonth,aes(x=Month, y=fit)) +  #fitted line
#   geom_ribbon(data = DiganaliWaterBodiesResultsplotMonth,aes(x=Month, y=fit, ymin = ll,ymax=ul),alpha=0.2) + #recreates plot smooth
#   geom_point(data=DiganaliWaterBodiesYears,aes(x=Month,y=Count)) #raw data
# 
# 
# DiganaliWaterBodiesResultsplotYear=plot_smooth(DiganaliWaterBodiesResults$gam, view="Year",lwd=2,
#                                            transform=exp,se=1, shade=T, #ylim = c(0,50),
#                                            ylab="Water Body Count",rug=F,
#                                            hide.label=T, n.grid = 100)$fv
# 
# ggplot() + geom_line(data = DiganaliWaterBodiesResultsplotYear,aes(x=Year, y=fit)) +  #fitted line
#   geom_ribbon(data = DiganaliWaterBodiesResultsplotYear,aes(x=Year, y=fit, ymin = ll,ymax=ul),alpha=0.2) + #recreates plot smooth
#   geom_point(data=DiganaliWaterBodiesYears,aes(x=Year,y=Count)) #raw data


