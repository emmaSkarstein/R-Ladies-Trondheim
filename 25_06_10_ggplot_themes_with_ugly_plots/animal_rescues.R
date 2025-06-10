# R-ladies Trondheim workshop, June 6th 2025

library(tidyverse) # Includes ggplot2 and dplyr, which we will use here!

# Loading and examining the data -----------------------------------------------
# Load the data from an URL
longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')

# What variables doe we have?
glimpse(longbeach) # Prints list of columns and some entries for each
View(longbeach) # Opens a "spreadsheet" view of the dataframe

# What are the different animals that are rescued?
unique(longbeach$animal_type)

# Some plots -------------------------------------------------------------------
# For tips and examples: https://r-graph-gallery.com/

# A basic bar plot of animal types
ggplot(longbeach) +
  geom_bar(aes(x = animal_type))



# A spatial plot

library(rnaturalearth)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot2::ggplot(longbeach) +
  ggplot2::geom_sf(data = world) +
  geom_point(aes(x = longitude, y = latitude)) +
  ggplot2::coord_sf(
    xlim = c(-120, -117),
    ylim = c(33, 34.5),
    expand = FALSE)
