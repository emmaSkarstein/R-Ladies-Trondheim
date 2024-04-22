# Follow-along code for workshop on ggplot2
# R Ladies Trondheim
# Spring 2024

# Intro to ggplot2 -------------------------------------------------------------

# Load some packages.
library(ggplot2)
library(palmerpenguins)
library(showtext)

# This activates the showtext package, for changing fonts.
showtext_auto()
# Add the font Josefin Sans:
font_add_google(name = "Josefin Sans", family = "Josefin Sans")


# We make a simple figure from the penguin data:
ggplot(data = penguins,  # Add the data
       aes(x = flipper_length_mm, y = body_mass_g)) + # Map the x and y axis to the respective variables
  geom_point(aes(color = species)) # Add points, and map the color to the penguin species


# Here is the end plot with some customization:
ggplot(
  # Add the data
  data = penguins,
  # Map the x and y axis to the respective variables
  aes(x = flipper_length_mm, y = body_mass_g)) +
  # Add points, and map the color to the penguin species
  geom_point(aes(color = species)) +
  # Change the default colors
  scale_color_manual(values = c("orange", "darkcyan", "hotpink")) +
  # Change labels in the plot
  labs(
    # Change the legend title
    color = "Species",
    # Change the axis titles
    x = "Flipper length (mm)", y = "Body mass (g)") +
  # Use a different theme
  theme_bw() +
  # Customize the theme
  theme(
    # Move the legend to the top
    legend.position = "top",
    # Change the font
    text = element_text(family = "Josefin Sans"))

# Flowers! ---------------------------------------------------------------------

# We will need the ggforce package to plot ellipses
library(ggforce)

n_petals <- 10

# First we plot a chain
ggplot() +
  geom_ellipse(aes(x0 = 1:n_petals, y0 = 0, a = 0.5, b = 0.5, angle = 0))

# Slight detour into pie charts etc.
ggplot(penguins, aes(x = factor(1), fill = species)) +
  geom_bar(width = 1)

ggplot(penguins, aes(x = factor(1), fill = species)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y")

# Try adding coord_polar to this:
ggplot() +
  geom_hline(yintercept = 1)

# Now back to the chain
ggplot() +
  geom_ellipse(aes(x0 = 1:n_petals, y0 = 0, a = 0.5, b = 0.5, angle = 0))+
  coord_polar()

# Try to:
# - add a theme to remove the background
# - fill the petals with color
# - change the parameters for the ellipses
# - other things?

ggplot() +
  geom_ellipse(aes(x0 = 1:n_petals, y0 = 0, a = 0.5, b = 0.5, angle = 0,
                   fill = factor(1:n_petals))) +
  coord_polar() +
  theme_void()


# Here is a function that lets us change the parameters more easily:
flower <- function(n_petals,
                   fill_values = "white",
                   color_values = "black",
                   a = 0.5,
                   b = 0.5,
                   angle = 0,
                   linewidth = 1){
  # Make color and fill vectors if only one color is given:
  if(length(fill_values) == 1){
    fill_values <- rep(fill_values, n_petals)
  }
  if(length(color_values) == 1){
    color_values <- rep(color_values, n_petals)
  }

  ggplot() +
    geom_ellipse(aes(x0 = 1:n_petals, y0 = 0, a = a, b = b, angle = angle,
                     fill = as.factor(1:n_petals),
                     color = as.factor(1:n_petals)),
                 linewidth = linewidth) +
    coord_polar() +
    scale_fill_manual(values = fill_values) +
    scale_color_manual(values = color_values) +
    theme_void() +
    theme(legend.position = "none")
}

# Pansy
pansy_colors <- c("#EFE94F", "#8488D7", "#1E0E47", "#8488D7", "#EFE94F")
(pansy <- flower(5, fill_values = pansy_colors, a = 0.7))

# Poppy
(poppy <- flower(8, fill_values = "red3", a = 1.1))

# Sunflower
(sunflower <- flower(15, color_values = "#36180F", fill_values = "gold"))

# Calendula
(calendula <- flower(20, fill_values = "darkorange", a = 0.7))

# Pink pansy
(pink_pansy <- flower(
  5, a = 0.6,
  fill_values = c("#EAC8D4", "#CB9DA7", "#B85186", "#CB9DA7", "#EAC8D4")))

# Orange
(orange_pansy <- flower(
  5, a = 0.7,
  fill_values = c("#804590", "#E1662D", "#C64826", "#E1662D", "#804590")))

# Pink nastertium
(nastertium <- flower(5, fill_values = "#D8647E", a = 0.48))

# Cream flower
(cream_flower <- flower(9, fill_values = "papayawhip", a = 0.46))

# Flax flower
(flax <- flower(5, fill_values = "#6280DF", a = 0.56))



library(patchwork)

(pansy | calendula | nastertium | orange_pansy | flax |
    poppy | cream_flower | sunflower | pink_pansy)
