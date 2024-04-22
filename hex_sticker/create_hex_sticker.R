# Creating hex-sticker

#install.packages("hexSticker")

library(hexSticker)
library(ggplot2)
library(showtext)
library(ggimage)

font_add_google("Courier Prime", "courier")
## Automatically use showtext to render text for future devices
showtext_auto()

# Colors: , ,
dark_purple <- "#39156C"
light_purple <- "#7B5191"
pink_purple <- "#94268D"

gg <- ggplot() +
  theme_void() +
  theme_transparent() +
  xlim(-1, 1) +
  ylim(-1, 1) +
  geom_text(aes_(x=0, y=0.15), label="R",
            color=dark_purple, family="courier", size=150) +
  geom_image(aes(x=-0.014, y=0.4, image="hex_sticker/lykkeporten.png"), size = 1) +
  geom_image(aes(x=0, y=-0.88, image="hex_sticker/rose.png"), size = 0.2) +
  geom_text(aes(x=0, y=-0.26), label="ladies",
            color=dark_purple, family="courier", size=30) +
  geom_text(aes(x=0, y=-0.5), label="trondheim",
            color=dark_purple, family="courier", fontface = "bold", size=45) +
  coord_fixed()


sticker(subplot = gg,
        package="",
        # Spotlight
        #spotlight = TRUE,
        # Subplot
        s_x=1, s_y=1, s_width=3, s_height = 1.8,
        # Hexagon
        h_fill = "white", h_color = dark_purple,
        dpi = 800,
        filename="hex_sticker/hex_trondheim.png")



