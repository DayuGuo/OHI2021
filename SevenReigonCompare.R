library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)

library(readr)

ohi <- read_csv("OHI-Total.csv")
ohi$region = as.factor(ohi$region)

ohi$region = ordered(ohi$region,levels = c('Sub-Saharan Africa','South Asia',"North America",'Middle East and North Africa',
                                           'Latin America and The Caribbean','Europe and Central Asia','East Asia and Pacific'))


ggplot(ohi, aes(x=region, y=total, fill=region)) +
  geom_boxplot(show.legend = FALSE)+coord_flip()+ylab("Scores")+xlab("Regions")+
  geom_jitter(shape=16, position=position_jitter(0.2), show.legend = FALSE)+
  scale_y_continuous(breaks = c(30,35,40,45,50,55,60,65))+
  theme_classic()+
  theme(legend.title = element_blank())


## CDI

ohi$region = as.factor(ohi$region)

ohi$region = ordered(ohi$region,levels = c('Sub-Saharan Africa','South Asia',"North America",'Middle East and North Africa',
                                           'Latin America and The Caribbean','Europe and Central Asia','East Asia and Pacific'))


ggplot(ohi, aes(x=region, y=CDI, fill=region)) +
  geom_boxplot(show.legend = FALSE)+coord_flip()+ylab("Scores")+xlab("Regions")+
  geom_jitter(shape=16, position=position_jitter(0.2), show.legend = FALSE)+
  scale_y_continuous(breaks = c(30,35,40,45,50,55,60,65))+
  theme_classic()+
  theme(legend.title = element_blank())

## CDEFG
ohi$region = as.factor(ohi$region)

ohi$region = ordered(ohi$region,levels = c('Sub-Saharan Africa','South Asia',"North America",'Middle East and North Africa',
                                           'Latin America and The Caribbean','Europe and Central Asia','East Asia and Pacific'))


ggplot(ohi, aes(x=region, y=G, fill=region)) +
  geom_boxplot(show.legend = FALSE)+coord_flip()+ylab("Scores")+xlab("Regions")+
  geom_jitter(shape=16, position=position_jitter(0.2), show.legend = FALSE)+
  scale_y_continuous(breaks = c(30,35,40,45,50,55,60,65))+
  theme_classic()+
  theme(legend.title = element_blank())
