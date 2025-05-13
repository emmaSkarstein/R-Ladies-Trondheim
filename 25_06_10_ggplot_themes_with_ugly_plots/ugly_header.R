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


library(ggplot2)
# sample data for plot ----
points <- 
  data.frame(
    x = rep(1:10,3), 
    y = rep(1:10,3), 
    z = sort(rep(letters[1:2], 15)),
    w = rep(letters[3:4], 15)
  )
# ggplot using many theme options ----
ggplot(data = points, 
       mapping = aes(x = x, y = y, col = factor(x))) + 
  geom_point(size = 5) + 
  facet_grid(w ~ z, switch = "y") +
  theme(
    
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 30, hjust = 0.25),
    plot.subtitle = element_text(size = 20, hjust = 0.75, color = "mediumvioletred", family = "serif"),
    plot.caption = element_text(size = 10, face = "italic", angle = 25),
    
    panel.background = element_rect(fill = 'lightblue', colour = 'darkred', size = 4),
    panel.border = element_rect(fill = NA, color = "green", size = 2),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 3),
    panel.grid.minor.y = element_blank(),
    
    axis.title.x = element_text(face = "bold.italic", color = "blue"),
    axis.title.y = element_text(family = "mono", face = "bold", size = 20, hjust = 0.25),
    axis.text = element_text(face = "italic", size = 15),
    axis.text.x.bottom = element_text(angle = 180), # note that axis.text options from above are inherited
    
    strip.background = element_rect(fill = "magenta"),
    strip.text.y = element_text(color = "white"),
    strip.placement = "outside",
    
    legend.background = element_rect(fill = "orangered4"), # generally will want to match w plot background
    legend.key = element_rect(fill = "orange"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(family = "serif", color = "white"),
    legend.text = element_text(family = "mono", face = "italic", color = "limegreen")
    
  ) +
  labs(title = "test title",
       subtitle = "test subtitle",
       x = "my x axis",
       y = "my y axis",
       caption = "this is a caption",
       col = "Renamed Legend") 
