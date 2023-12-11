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
ggplot(Ponds2022_summary, aes(x = Month, y = Count)) + 
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