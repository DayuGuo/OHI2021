library(readxl)
Exp <- read_excel("Exp.xlsx")
library(readr)
OHI一级指标 <- read_csv("OHI一级指标.csv")

library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)

data = left_join(OHI一级指标,Exp, by = "country")





test <- data %>%
  mutate(
    region = case_when(
      region == "Middle East and North Africa" ~ "Middle East\nand North Africa",
      region == "Europe and Central Asia" ~ "Europe and\nCentral Asia",
      region == "East Asia and Pacific" ~ "East Asia and\n Pacific",
      region == "Latin America and The Caribbean" ~ "Latin America and\n The Caribbean",
      region == "North America" ~ "North America",
      region == "South Asia" ~ "South Asia",
      region == "Sub-Saharan Arica" ~ "Sub-Saharan Arica",
      TRUE ~ region # All the other remain the same
    )
  )

country_highlight <- c(
  "Germany", "Norway", "United States of America", "Greece", "Singapore", "Rwanda", 
  "Russia", "Venezuela", "Sudan", "Iraq", "Ghana", "Niger", "Chad", "Kuwait", 
  "Qatar", "Myanmar", "Nepal", "Chile", "Argentina", "Japan", "China"
)

test <- test %>%
  mutate(
    label = ifelse(country %in% country_highlight, country, "")
  )


ggplot(test, aes(exp, total)) +
  geom_smooth(method="lm", formula = 'y ~ poly(x,2)' , col="black") +
  geom_point(
    aes(color = region, fill = region),
    size = 2.5, alpha = 0.5, 
    shape = 21 # This is a dot with both border (color) and fill.
  ) +
  # Add auto-positioned text
  geom_text_repel(
    aes(label = label),
    color = "black",
    size = 9/.pt, # font size 9 pt
    point.padding = 0.1, 
    box.padding = 0.6,
    min.segment.length = 0,
    max.overlaps = 1000,
    seed = 7654 # For reproducibility reasons
  ) +
  xlab("Life Expec") +
  ylab("One Health Index")+
  theme_test() 



## GDP-OHI
library(readxl)
library(readxl)
GDP <- read_excel("GDP per cap.xls")
data = left_join(OHI一级指标,GDP, by = "iso")
test <- data %>%
  mutate(
    region = case_when(
      region == "Middle East and North Africa" ~ "Middle East\nand North Africa",
      region == "Europe and Central Asia" ~ "Europe and\nCentral Asia",
      region == "East Asia and Pacific" ~ "East Asia and\n Pacific",
      region == "Latin America and The Caribbean" ~ "Latin America and\n The Caribbean",
      region == "North America" ~ "North America",
      region == "South Asia" ~ "South Asia",
      region == "Sub-Saharan Arica" ~ "Sub-Saharan Arica",
      TRUE ~ region # All the other remain the same
    )
  )

country_highlight <- c(
  "Germany", "Norway", "United States of America", "Greece", "Singapore", "Rwanda", 
  "Russia", "Venezuela", "Sudan", "Iraq", "Ghana", "Niger", "Chad", "Kuwait", 
  "Qatar", "Myanmar", "Nepal", "Chile", "Argentina", "Japan", "China"
)

test <- test %>%
  mutate(
    label = ifelse(country %in% country_highlight, country, "")
  )

ggplot(test, aes(x=log(gdp), y=total))+
  geom_smooth(method="lm", formula = 'y ~ x' , col="black")+
  geom_point(
    aes(color = region, fill = region),
    size = 2.5, alpha = 0.5, 
    shape = 21 # This is a dot with both border (color) and fill.
  ) +
  # Add auto-positioned text
  geom_text_repel(
    aes(label = label),
    color = "black",
    size = 9/.pt, # font size 9 pt
    point.padding = 0.1, 
    box.padding = 0.6,
    min.segment.length = 0,
    max.overlaps = 1000,
    seed = 7654 # For reproducibility reasons
  ) +
  xlab("GDP per capita (current US$)") +
  ylab("One Health Index")+
  theme_test() 


## B&Exp
library(readxl)
Exp <- read_excel("Exp.xlsx")
library(readr)
OHI一级指标 <- read_csv("OHI一级指标.csv")

library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)

data = left_join(OHI一级指标,Exp, by = "country")





test <- data %>%
  mutate(
    region = case_when(
      region == "Middle East and North Africa" ~ "Middle East\nand North Africa",
      region == "Europe and Central Asia" ~ "Europe and\nCentral Asia",
      region == "East Asia and Pacific" ~ "East Asia and\n Pacific",
      region == "Latin America and The Caribbean" ~ "Latin America and\n The Caribbean",
      region == "North America" ~ "North America",
      region == "South Asia" ~ "South Asia",
      region == "Sub-Saharan Arica" ~ "Sub-Saharan Arica",
      TRUE ~ region # All the other remain the same
    )
  )

country_highlight <- c(
  "Germany", "Norway", "United States of America", "Greece", "Singapore", "Rwanda", 
  "Russia", "Venezuela", "Sudan", "Iraq", "Ghana", "Niger", "Chad", "Kuwait", 
  "Qatar", "Myanmar", "Nepal", "Chile", "Argentina", "Japan", "China"
)

test <- test %>%
  mutate(
    label = ifelse(country %in% country_highlight, country, "")
  )


ggplot(test, aes(exp, B)) +
  geom_smooth(method="lm", formula = 'y ~ poly(x,2)' , col="black") +
  geom_point(
    aes(color = region, fill = region),
    size = 2.5, alpha = 0.5, 
    shape = 21 # This is a dot with both border (color) and fill.
  ) +
  # Add auto-positioned text
  geom_text_repel(
    aes(label = label),
    color = "black",
    size = 9/.pt, # font size 9 pt
    point.padding = 0.1, 
    box.padding = 0.6,
    min.segment.length = 0,
    max.overlaps = 1000,
    seed = 7654 # For reproducibility reasons
  ) +
  xlab("Life Expec") +
  ylab("IDI")+
  theme_test() 
