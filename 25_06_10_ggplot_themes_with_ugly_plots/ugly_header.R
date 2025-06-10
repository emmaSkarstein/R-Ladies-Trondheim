library(ggplot2)
library(ggpattern)
library(emoGG)

# install.packages("remotes")
# remotes::install_github("dill/emoGG")

library(tidyverse)
library(rtweet)
library(rvest)
# devtools::install_github("clauswilke/ggtext")
library(ggtext)
library(emo)



data <- data.frame(x = c("a", "b", "c", "d", "e"), y = rgamma(5, shape = 10), 
                   animal = c("Cat", "Horse", "Dog", "Ostrich", "Rat"))

ggplot(data, aes(x = x, y = y)) +
  geom_col_pattern(
    aes(pattern = x, fill = x, pattern_fill = x), 
    colour                   = 'black', 
    pattern_density          = 0.35, 
    pattern_key_scale_factor = 1.3) +
  geom_text(aes(label = animal, y = y + 1, color = x, fontface = c("bold", "italic", "bold", "bold", "italic")),size = 8) +
  scale_pattern_fill_manual(values = c(a='blue', b='red', c='yellow', d='darkgreen', e='gold')) + 
  xlab("Animals") +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "lightyellow"),
        plot.background = element_rect(fill = "goldenrod"),
        axis.text.x = element_text(angle = 180, size = 13),
        axis.title.y = element_text(family = "serif", face = "bold", size = 20),
        axis.title.x = element_text(family = "mono", face = "bold", size = 20),
        axis.text.y = element_text(size = 13),
        legend.position = "none")

ggsave("ugly_plot.png", height = 5, width = 8.8)
