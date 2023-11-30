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
 

#######extract data from other villages.....Asso 2

#2019
Asso22019 = filter(Ponds2019numeric,Village == "Asso 2") 
Asso22019$`Water Body` = toupper(Asso22019$`Water Body`)
unique(Asso22019$`Water Body`)

Asso22019_summary <- Asso22019 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

Asso22019_summary$Village = as.factor(Asso22019_summary$Village) 

#2020
Asso22020 = filter(Ponds2020numeric,Village == "Asso 2") 
Asso22020$`Water Body` = toupper(Asso22020$"Nom de la source d'eau")
unique(Asso22020$`Water Body`)

Asso22020_summary <- Asso22020 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

Asso22020_summary$Village = as.factor(Asso22020_summary$Village) 

#2021

#more data cleaning, some are in village column and others in zone 
Ponds2021 <- Ponds2021 %>% mutate(Date = as.Date(Date, format = "%m/%d/%y")) 

Ponds2021$Month = months(as.Date(Ponds2021$Date))

Ponds2021numeric = Ponds2021 %>%
  mutate(Month = dplyr::recode(Month,
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

Asso22021Zone = Ponds2021numeric %>% filter(Ponds2021$Zone == "Asso 2")
Asso22021Village = Ponds2021numeric %>% filter(Ponds2021$Village == "Asso 2") 
Asso22021 = bind_rows(Asso22021Zone,Asso22021Village)

month_names = c("Janvier", "Fevrier", "Mars", "Avril", "Mai","Juin","Juillet", "Aout","Septembre","Octobre", "Novembre", "Decembre")
Asso22021$Waterbody = ifelse(Asso22021$`Nom de la source d'eau` %in% month_names, Asso22021$`Niveau de surveillance`, Asso22021$`Nom de la source d'eau`)
Asso22021$`Water Body` = toupper(Asso22021$"Waterbody")
unique(Asso22021$`Water Body`)

Asso22021_summary <- Asso22021 %>%
  group_by(Month) %>%  #ignore column for village because names of villages in both zone and village column, but all Diganali
  summarise(Count = n()) 

Asso2 = rep("Asso 2", times = 7)
as.data.frame(Asso2)

Asso22021_summary = cbind(Asso2,Asso22021_summary) #add a column for Diganali before combining with data from 2019 and 2020
colnames(Asso22021_summary) = c("Village","Month", "Count" )
Asso22021_summary$Village = as.factor(Asso22021_summary$Village) 

#swap columns for village and month so same order as other years


##2022

Asso22022 = filter(Ponds2022numeric,village == "asso 2") 

Asso22022_summary <- Asso22022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(Asso22022_summary) = c("Month", "Village", "Count")

Asso22022_summary[Asso22022_summary=="asso 2"] <- "Asso 2"

Asso22022_summary$Village = as.factor(Asso22022_summary$Village) 



######Mailao
#2019
Mailao2019 = filter(Ponds2019numeric,Village == "Mailao") 
Mailao2019$`Water Body` = toupper(Mailao2019$`Water Body`)
unique(Mailao2019$`Water Body`)

Mailao2019_summary <- Mailao2019 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

Mailao2019_summary$Village = as.factor(Mailao2019_summary$Village) 

#2020
Mailao2020 = filter(Ponds2020numeric,Village == "Mailao") 
Mailao2020$`Water Body` = toupper(Mailao2020$"Nom de la source d'eau")
unique(Mailao2020$`Water Body`)

Mailao2020_summary <- Mailao2020 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

Mailao2020_summary$Village = as.factor(Mailao2020_summary$Village) 

#last row of data does not have month info, confirmed this is not a coding error but that it is just missing from data set
Mailao2020_summary = Mailao2020_summary[1:3,] 

#2021

#more data cleaning, some are in village column and others in zone 
Ponds2021 <- Ponds2021 %>% mutate(Date = as.Date(Date, format = "%m/%d/%y")) 

Ponds2021$Month = months(as.Date(Ponds2021$Date))

Ponds2021numeric = Ponds2021 %>%
  mutate(Month = dplyr::recode(Month,
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

MailaoZone = Ponds2021numeric %>% filter(Ponds2021$Zone == "Mailao")
MailaoVillage = Ponds2021numeric %>% filter(Ponds2021$Village == "Mailao") 
Mailao2021 = bind_rows(MailaoZone,MailaoVillage)

month_names = c("Janvier", "Fevrier", "Mars", "Avril", "Mai","Juin","Juillet", "Aout","Septembre","Octobre", "Novembre", "Decembre")
Mailao2021$Waterbody = ifelse(Mailao2021$`Nom de la source d'eau` %in% month_names, Mailao2021$`Niveau de surveillance`, Mailao2021$`Nom de la source d'eau`)
Mailao2021$`Water Body` = toupper(Mailao2021$"Waterbody")
unique(Mailao2021$`Water Body`)

Mailao2021_summary <- Mailao2021 %>%
  group_by(Month) %>%  #ignore column for village because names of villages in both zone and village column, but all Diganali
  summarise(Count = n()) 

Mailao = rep("Mailao", times = 11)
as.data.frame(Mailao)

Mailao2021_summary = cbind(Mailao,Mailao2021_summary) #add a column for Diganali before combining with data from 2019 and 2020
colnames(Mailao2021_summary) = c("Village","Month", "Count" )
Mailao2021_summary$Village = as.factor(Mailao2021_summary$Village) 

#swap columns for village and month so same order as other years


##2022

Mailao2022 = filter(Ponds2022numeric,village == "mailao") 

Mailao2022_summary <- Mailao2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(Mailao2022_summary) = c("Month", "Village", "Count")

Mailao2022_summary[Mailao2022_summary=="mailao"] <- "Mailao"

Mailao2022_summary$Village = as.factor(Mailao2022_summary$Village) 



####Loumia Centre
#2019
LC2019 = filter(Ponds2019numeric,Village == "Loumia Centre") 
LC2019$`Water Body` = toupper(LC2019$`Water Body`)
unique(LC2019$`Water Body`)

LC2019_summary <- LC2019 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

LC2019_summary$Village = as.factor(LC2019_summary$Village) 

#last row of data does not have month info
LC2019_summary = LC2019_summary[1:7,]

#2020
LC2020 = filter(Ponds2020numeric,Village == "Loumia Centre") 
LC2020$`Water Body` = toupper(LC2020$"Nom de la source d'eau")
unique(LC2020$`Water Body`)

LC2020_summary <- LC2020 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

LC2020_summary$Village = as.factor(LC2020_summary$Village) 

#last row of data does not have month info
LC2020_summary = LC2020_summary[1:7,] 

#2021

#more data cleaning, some are in village column and others in zone 
Ponds2021 <- Ponds2021 %>% mutate(Date = as.Date(Date, format = "%m/%d/%y")) 

Ponds2021$Month = months(as.Date(Ponds2021$Date))

Ponds2021numeric = Ponds2021 %>%
  mutate(Month = dplyr::recode(Month,
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

LCZone = Ponds2021numeric %>% filter(Ponds2021$Zone == "Loumia Centre")
LCVillage = Ponds2021numeric %>% filter(Ponds2021$Village == "Loumia Centre") 
LC2021 = bind_rows(LCZone,LCVillage)

month_names = c("Janvier", "Fevrier", "Mars", "Avril", "Mai","Juin","Juillet", "Aout","Septembre","Octobre", "Novembre", "Decembre")
LC2021$Waterbody = ifelse(LC2021$`Nom de la source d'eau` %in% month_names, LC2021$`Niveau de surveillance`, LC2021$`Nom de la source d'eau`)
LC2021$`Water Body` = toupper(LC2021$"Waterbody")
unique(LC2021$`Water Body`)

LC2021_summary <- LC2021 %>%
  group_by(Month) %>%  #ignore column for village because names of villages in both zone and village column, but all Diganali
  summarise(Count = n()) 

LoumiaCentre = rep("Loumia Centre", times = 9)
as.data.frame(LoumiaCentre)

LC2021_summary = cbind(LoumiaCentre,LC2021_summary) #add a column for Diganali before combining with data from 2019 and 2020
colnames(LC2021_summary) = c("Village","Month", "Count" )
LC2021_summary$Village = as.factor(LC2021_summary$Village) 

#swap columns for village and month so same order as other years


##2022

LC2022 = filter(Ponds2022numeric,village == "loumia centre") 

LC2022_summary <- LC2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(LC2022_summary) = c("Month", "Village", "Count")

LC2022_summary[LC2022_summary=="loumia centre"] <- "Loumia Centre"

LC2022_summary$Village = as.factor(LC2022_summary$Village) 



####Loumia Centre
#2019
LC2019 = filter(Ponds2019numeric,Village == "Loumia Centre") 
LC2019$`Water Body` = toupper(LC2019$`Water Body`)
unique(LC2019$`Water Body`)

LC2019_summary <- LC2019 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

LC2019_summary$Village = as.factor(LC2019_summary$Village) 

#last row of data does not have month info
LC2019_summary = LC2019_summary[1:7,]

#2020
LC2020 = filter(Ponds2020numeric,Village == "Loumia Centre") 
LC2020$`Water Body` = toupper(LC2020$"Nom de la source d'eau")
unique(LC2020$`Water Body`)

LC2020_summary <- LC2020 %>%
  group_by(Month, Village) %>%
  summarise(Count = n()) 

LC2020_summary$Village = as.factor(LC2020_summary$Village) 

#last row of data does not have month info
LC2020_summary = LC2020_summary[1:7,] 

#2021

#more data cleaning, some are in village column and others in zone 
Ponds2021 <- Ponds2021 %>% mutate(Date = as.Date(Date, format = "%m/%d/%y")) 

Ponds2021$Month = months(as.Date(Ponds2021$Date))

Ponds2021numeric = Ponds2021 %>%
  mutate(Month = dplyr::recode(Month,
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

LCZone = Ponds2021numeric %>% filter(Ponds2021$Zone == "Loumia Centre")
LCVillage = Ponds2021numeric %>% filter(Ponds2021$Village == "Loumia Centre") 
LC2021 = bind_rows(LCZone,LCVillage)

month_names = c("Janvier", "Fevrier", "Mars", "Avril", "Mai","Juin","Juillet", "Aout","Septembre","Octobre", "Novembre", "Decembre")
LC2021$Waterbody = ifelse(LC2021$`Nom de la source d'eau` %in% month_names, LC2021$`Niveau de surveillance`, LC2021$`Nom de la source d'eau`)
LC2021$`Water Body` = toupper(LC2021$"Waterbody")
unique(LC2021$`Water Body`)

LC2021_summary <- LC2021 %>%
  group_by(Month) %>%  #ignore column for village because names of villages in both zone and village column, but all Diganali
  summarise(Count = n()) 

LoumiaCentre = rep("Loumia Centre", times = 9)
as.data.frame(LoumiaCentre)

LC2021_summary = cbind(LoumiaCentre,LC2021_summary) #add a column for Diganali before combining with data from 2019 and 2020
colnames(LC2021_summary) = c("Village","Month", "Count" )
LC2021_summary$Village = as.factor(LC2021_summary$Village) 

#swap columns for village and month so same order as other years


##2022

LC2022 = filter(Ponds2022numeric,village == "loumia centre") 

LC2022_summary <- LC2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

colnames(LC2022_summary) = c("Month", "Village", "Count")

LC2022_summary[LC2022_summary=="loumia centre"] <- "Loumia Centre"

LC2022_summary$Village = as.factor(LC2022_summary$Village) 


##2022 Kolemara Sara 

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



##2022 Guizeme 

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


##2022 Bem Bem-2

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




######Asso2, Diganali, Mailao, Loumia Centre, Guimeze, BemBem2
Asso2 = bind_rows(Asso22019_summary,Asso22020_summary,Asso22021_summary,Asso22022_summary)
Diganali = bind_rows(Diganali2019_summary,Diganali2020_summary,Diganali2021_summary,Diganali2022_summary)
Mailao = bind_rows(Mailao2019_summary,Mailao2020_summary,Mailao2021_summary,Mailao2022_summary)
LoumiaCentre = bind_rows(LC2019_summary,LC2020_summary,LC2021_summary,LC2022_summary)


YearsDiganali = data.frame(c(rep("2019", times = 8), rep("2020", times = 10), rep("2021", times = 11), rep("2022", times = 11)))
YearsAsso2 = data.frame(c(rep("2019", times = 8), rep("2020", times = 5), rep("2021", times = 7), rep("2022", times = 12)))
YearsMailao = data.frame(c(rep("2019", times = 7), rep("2020", times = 3), rep("2021", times = 11), rep("2022", times = 12)))
YearsLoumia = data.frame(c(rep("2019", times = 7), rep("2020", times = 7), rep("2021", times = 9), rep("2022", times = 12)))

Diganali = cbind(Diganali,YearsDiganali)
colnames(Diganali) = c("Month","Village", "Count", "Year")

Asso2 = cbind(Asso2,YearsAsso2)
colnames(Asso2) = c("Month","Village", "Count", "Year")

Mailao = cbind(Mailao,YearsMailao)
colnames(Mailao) = c("Month","Village", "Count", "Year")  #why does 2021 have so many more water bodies? 

LoumiaCentre = cbind(LoumiaCentre,YearsLoumia)
colnames(LoumiaCentre) = c("Month","Village", "Count", "Year")

#other villages from Stephanie's list  
#Toukra Mousgoum
#KobÈ
#Madjira 1
#Kournari Urbain (control?)
#Nangoto (control?)


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


SelectVillages = bind_rows(Diganali,Asso2, Mailao,LoumiaCentre, KolemaraSara,Guimeze,BemBem2,ToukraMousgoum,Kob,Madjira1) 



#analyze for 2022 only

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



####Look at data for villages for other years


####2019
Select2019 = filter(Ponds2019numeric, Village == "Diganali" |Village == "Asso 2" | Village == "Mailao" | Village == "Loumia Centre" | Village == "Kolemara Sara" | Village == "Guimeze" | Village == "Bem-Bem 2" | Village == "Toukra Mousgoum" | Village == "Kob" | Village == "Madjira 1") 

Summary_Select2019 <- Select2019 %>%
  group_by(Month, Village) %>%
  summarise(Count = n())    

#data from 2019 is very incomplete

####



####2020
Select2020 = filter(Ponds2020numeric, Village == "Diganali" |Village == "Asso 2" | Village == "Mailao" | Village == "Loumia Centre" | Village == "Kolemara Sara" | Village == "Guimeze" | Village == "Bem-Bem 2" | Village == "Toukra Mousgoum" | Village == "Kob" | Village == "Madjira 1") 

Summary_Select2020 <- Select2020 %>%
  group_by(Month, Village) %>%
  summarise(Count = n())  

#data from 2020 is very incomplete other than for Diganali 

Diganali2020 = filter(Summary_Select2020, Village == "Diganali")

Diganali2020result = gamm(round(Count) ~ s(Month),
                         family = quasipoisson(), correlation = corCAR1(form = ~Month),
                         data = Diganali2020)  

plot_smooth(Diganali2020result$gam, view="Month",lwd=2,
            transform=exp,se=1, shade=T, #ylim = c(0,50),
            ylab="Water Body Count",rug=F,
            hide.label=T, n.grid = 100)$fv
####


####2021
Select2021Zone = filter(Ponds2021numeric, Zone == "Diganali" |Zone == "Asso 2" | Zone == "Mailao" | Zone == "Loumia Centre" | Zone == "Kolemara Sara" | Zone == "Guimeze" | Zone == "Bem-Bem 2" | Zone == "Toukra Mousgoum" | Zone == "Kob" | Zone == "Madjira 1") 
Select2021Zone
Select2021Village = filter(Ponds2021numeric, Village == "Diganali" |Village == "Asso 2" | Village == "Mailao" | Village == "Loumia Centre" | Village == "Kolemara Sara" | Village == "Guimeze" | Village == "Bem-Bem 2" | Village == "Toukra Mousgoum" | Village == "Kob" | Village == "Madjira 1") 
Select2021Village

Summary_Select2021Zone <- Select2021Zone %>%
  group_by(Month, Zone) %>%
  summarise(Count = n())  

#disregard zone, looks like there might be a zone and a village both called mailao

Summary_Select2021Village <- Select2021Village %>%
  group_by(Month, Village) %>%
  summarise(Count = n())  

#Diganali has fairly complete data for 2021

Diganali2021 = filter(Summary_Select2021Village, Village == "Diganali")

Diganali2021result = gamm(round(Count) ~ s(Month),
                         family = quasipoisson(), correlation = corCAR1(form = ~Month),
                         data = Diganali2021)  

plot_smooth(Diganali2021result$gam, view="Month",lwd=2,
            transform=exp,se=1, shade=T, #ylim = c(0,50),
            ylab="Water Body Count",rug=F,
            hide.label=T, n.grid = 100)$fv

#why does the water body count start so high? doesn't make sense when December 2020 only had 20 water bodies.

####
