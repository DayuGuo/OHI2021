library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)

library(readr)
df <- read_csv("B-IDI/IDI.csv")

df$region = as.factor(df$region)

df$region = ordered(df$region,levels = c('Sub-Saharan Arica','South Asia',"North America",'Middle East and North Africa',
                                           'Latin America and The Caribbean','Europe and Central Asia','East Asia and Pacific'))


ggplot(df, aes(x=region, y=B, fill=region)) +
  geom_boxplot(show.legend = FALSE)+coord_flip()+ylab("Scores")+xlab("Regions")+
  geom_jitter(shape=16, position=position_jitter(0.2), show.legend = FALSE)+
  scale_y_continuous(breaks = c(30,35,40,45,50,55,60,65))+
  theme_classic()+
  theme(legend.title = element_blank())
