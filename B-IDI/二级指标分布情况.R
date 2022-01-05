library(readxl)
data <- read_excel("B-IDI/二级指标分布.xlsx")
library(ggridges)
library(ggplot2)

data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

# Plot
data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Score") +
  ylab("Density")

