library(ggplot2)
library(cowplot)
library(stringr)
library(NatParksPalettes)
library(tidyverse)
library(dplyr)

setwd("~/Desktop/Rscripts")
Week1 <- read_csv("Data_Week1_UGA_2-20-24.csv",locale=locale(encoding="latin1"))

result <- Week1 %>%
  group_by(Sample) %>%
  summarize(mean_value = mean(Chlorophyll),
            std_error = sd(Chlorophyll) / sqrt(n()))


ggplot(result, aes(x = Sample, y = mean_value, ymin = mean_value - std_error, ymax = mean_value + std_error)) +
  geom_point() +  # Add points for means
  geom_errorbar(width = 0.2) +  # Add error bars for standard errors
  labs(x = "Sample", y = "Mean Chlorophyll", title = "Mean Chlorophyll Values with Standard Errors by Site") +
  theme_minimal()

summary(aov(Chlorophyll ~ Sample, data = Week1)) 

#figure with actual data, not summary statistics 

ggplot(Week1, aes(x = Sample, y = Chlorophyll)) +
  geom_boxplot() +  # Add points for means
  #geom_errorbar(width = 0.2) +  # Add error bars for standard errors
  labs(x = "Sample", y = "Mean Chlorophyll", title = "Mean Chlorophyll Values with Standard Errors by Site") +
  theme_minimal()


CopepodsWeek1 <- read_csv("Copepod_Totals_Week1.csv",locale=locale(encoding="latin1"))

CopepodsWeek1 = CopepodsWeek1 %>% slice(1:30)

result = result %>% slice(1:30)

data = cbind(CopepodsWeek1,result)

data = data %>% select(1,2,4,5)

colnames(data) = c("Site","TotalCopepods","Chlorophyll","STDE")

ggplot(data, aes(x=TotalCopepods, y = Chlorophyll)) + geom_point()



