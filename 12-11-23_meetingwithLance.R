Ponds2022 <- read_csv("chad_abate_2022_10-27-23.csv",locale=locale(encoding="latin1"))

range(Ponds2022$volume) #three ponds listed with no size or volume

abatequantityNoZeros2022 = Ponds2022$abatequantity[Ponds2022$abatequantity>0] 

abatevolumeNoZeros2022 = Ponds2022$volume[Ponds2022$abatequantity>0] 

range(abatequantityNoZeros2022/abatevolumeNoZeros2022) 

hist(abatequantityNoZeros2022/abatevolumeNoZeros2022)

sort(abatequantityNoZeros2022/abatevolumeNoZeros2022, decreasing = TRUE)[1:100] #one water body with ratio of 2005

abatevolumeNoZeros2022[abatequantityNoZeros2022/abatevolumeNoZeros2022>500] #very different than others, potentially typo, should remove from analysis. Wide shallow waterbopdy that received a lot of abate.

hist(abatequantityNoZeros2022[(abatequantityNoZeros2022/abatevolumeNoZeros2022)<50]/abatevolumeNoZeros2022[(abatequantityNoZeros2022/abatevolumeNoZeros2022)<50]) 

#most received relatively small abatement per unit

Ponds2022 <- Ponds2022 %>% mutate(Date = as.Date(treatmentdate, format = "%m/%d/%y")) 

Ponds2022$Month = months(as.Date(Ponds2022$treatmentdate))

Ponds2022numeric <- Ponds2022 %>%
  mutate(Month = as.numeric(factor(Month, levels = month.name)))

Ponds2022_summary <- Ponds2022numeric %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

unique(Ponds2022_summary$village) #700 unique villages

library(ggplot2)
ggplot(Ponds2022_summary, aes(x = Month, y = Count)) +                #plot with individual lines for each village
  geom_line(aes(color = village)) + theme(legend.position="none")

Ponds2022_summary$village = as.factor(Ponds2022_summary$village)

Villages2022 = gamm(round(Count) ~ s(Month) + s(village, bs = "re"),
                         family = quasipoisson(), correlation = corCAR1(form = ~Month | village),
                         data = Ponds2022_summary) 


Villages2022plot =plot_smooth(Villages2022$gam, view="Month",lwd=2,
                                  transform=exp,se=1, shade=T, #ylim = c(0,50),
                                  ylab="Water Body Count",rug=F,
                                  hide.label=T, n.grid = 100)$fv

ggplot() + geom_line(data = SelectResults2022plot,aes(x=Month, y=fit)) +  #fitted line
  geom_ribbon(data = SelectResults2022plot,aes(x=Month, y=fit, ymin = ll,ymax=ul),alpha=0.2) + #recreates plot smooth
  geom_point(data=SelectVillages2022,aes(x=Month,y=Count)) #raw data


###

Villages2022 <- Ponds2022 %>% group_by(watername,zone) #filter by individual villages 

BemBem2 <-  filter(Test,village == "bem-bem 2") 

#Look for evidence of water bodies combining in the data set. Start with villages that are also being ground truthed. Of the water bodies I can reasonably assume are the same, how does their size change each month?
#Look at size composition of water bodies over the course of the year to determine if the initial peak is an increase in small water bodies.


#Bouta Sidiri, look at changes in water body size over time

BoutaSidiri = dplyr::filter(BemBem2, grepl("bouta si",watername))
BoutaSidiri = dplyr::filter(BoutaSidiri, !grepl("bouta simpo",watername))

BoutaSidiri <- BoutaSidiri %>% mutate(Date = as.Date(treatmentdate, format = "%m/%d/%y")) 

BoutaSidiri$Month = months(as.Date(BoutaSidiri$treatmentdate))

BoutaSidiri = BoutaSidiri %>% mutate(Month = as.numeric(factor(Month, levels = month.name)))

#manually rename waternames to fix typos (find easier way to do this?)
waternamecorrected = data.frame(c(rep("Bouta Sidiri 3", times = 2),"BoutaSidiri 1", "Bouta Sidiri 2", rep("Bouta Sidiri 3", times = 2), "Bouta Sidiri 4", rep("Bouta Sidiri 3", times = 2), "Bouta Sidiri 1", "Bouta Sidiri 5", "Bouta Sidiri b", "Bouta Sidiri c", "Bouta Sidiri 5", "Bouta Sidiri 3c", "Bouta Sidiri 4"))
BoutaSidiri = cbind(waternamecorrected,BoutaSidiri)
colnames(BoutaSidiri)[1] = "waternamecorrected" 

library(RColorBrewer)
colourCount = length(unique(BoutaSidiri$waternamecorrected))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colrs<- getPalette(colourCount)
names(colrs) <- unique(BoutaSidiri$waternamecorrected)
BoutaSidiri$color <- colrs[BoutaSidiri$waternamecorrected]  
BoutaSidiri$color
                
ggplot() + geom_point(data=BoutaSidiri,aes(x=Month,y=volume), color = BoutaSidiri$color, size = 5) 


#What villages may have more complete data sets? 

kob2022 = filter(Ponds2022numeric,village == "kob") 
MareCarriereBrique = dplyr::filter(kob2022, grepl("mare c",watername))
MareCarriereBrique = dplyr::filter(MareCarriereBrique, !grepl("zara",watername))

MareCarriereBrique$Month = as.factor(MareCarriereBrique$Month)

# MCB =  gamm(round(volume) ~ s(Month),   #months without data, shouldn't run gamm? 
#             family = quasipoisson(),
#             data = MareCarriereBrique) 
# 

ggplot() + geom_point(data=MareCarriereBrique,aes(x=Month,y=volume))

#look at others

Guimeze2022 = filter(Ponds2022numeric,village == "guimeze") 

KS2022 = filter(Ponds2022numeric,village == "kolemara sara") 

LC2022 = filter(Ponds2022numeric,village == "loumia centre") 

ggplot() + geom_point(data=LC2022,aes(x=Month,y=volume)) #volume peaks in ~July

LC2022_summary <- LC2022 %>%
  group_by(Month, village) %>%
  summarise(Count = n()) 

ggplot() + geom_point(data=LC2022_summary,aes(x=Month,y=Count)) #count peaks between March and June

