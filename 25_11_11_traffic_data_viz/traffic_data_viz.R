# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R-Ladies Trondheim: Traffic data workshop
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code by Emma Skarstein, with assi

library(httr) # ghql
library(jsonlite)
library(tidyverse)
library(lubridate)

source("25_11_11_traffic_data_viz/traffic_data_prep.R")

bicycles_at_moholt <- bike_data
weather_at_moholt <- weather_daily
bicycles_in_trondheim <- bike_data_all_stations

# ~~~~~~~~~~~~~~~~~~~~bike_data_all_stations# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exploratory plots with ggplot2 ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Time series
ggplot(bicycles_at_moholt, aes(x = date, y = count)) +
  geom_line(color = "#2E86AB", linewidth = 0.8) +
  labs(
    title = "Daily bicycle counts",
    x = NULL,
    y = "Number of bicycles"
  ) +
  theme_minimal(base_size = 14)

# Time series with dots
ggplot(bicycles_at_moholt, aes(x = date, y = count)) +
  geom_line(color = "#2E86AB", linewidth = 0.8) +
  geom_point(aes(color = is_weekend)) +
  labs(
    title = "Daily bicycle counts",
    x = NULL,
    y = "Number of bicycles"
  ) +
  theme_minimal(base_size = 14)

# Time series with only weekdays
bicycles_at_moholt %>% filter(!is_weekend) %>%
  ggplot(aes(x = date, y = count)) +
  geom_line(color = "#2E86AB", linewidth = 0.8) +
  labs(
    title = "Daily bicycle counts (weekdays)",
    x = NULL,
    y = "Number of bicycles"
  ) +
  theme_minimal(base_size = 14)


# Adding weather data
bike_with_weather <- left_join(bicycles_at_moholt, weather_at_moholt)

# Time series with only weekdays
bike_with_weather %>% filter(!is_weekend) %>%
  ggplot(aes(x = date, y = temp_mean)) +
  geom_line(color = "goldenrod", linewidth = 0.8) +
  labs(
    title = "Daily bicycle counts (weekdays)",
    x = NULL,
    y = "Number of bicycles"
  ) +
  theme_minimal(base_size = 14)


scale <- 100

bike_with_weather %>%
  filter(!is_weekend) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = count), color = "#2e86ab", linewidth = 0.8) +
  geom_line(aes(y = temp_mean * scale), color = "goldenrod", linewidth = 0.8) +
  labs(
    title = "Daily bicycle counts (weekdays)",
    x = NULL
  ) +
  scale_y_continuous(
    name = "Number of bicycles",
    sec.axis = sec_axis(as.formula(~ . / scale), name = "Road surface temperature")
  ) +
  theme_minimal(base_size = 14)




# Seasonal pattern
ggplot(bicycles_at_moholt, aes(x = month, y = count)) +
  geom_boxplot(fill = "#A23B72", alpha = 0.7) +
  labs(
    title = "Seasonal cycling patterns in Trondheim",
    x = "Month",
    y = "Daily bicycle count"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Weekday vs Weekend
ggplot(bicycles_at_moholt, aes(x = weekday, y = count, fill = is_weekend)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#2E86AB", "#F18F01"), guide = "none") +
  labs(
    title = "Bicycle counts by day of week",
    x = NULL,
    y = "Daily bicycle count"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Interactive map with Leaflet ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(leaflet)

# Create a color palette based on average daily traffic
# Define breaks for traffic levels
color_pal <- colorNumeric(
  palette = "RdYlGn",  # Red-Yellow-Green palette (reversed so high = green)
  domain = station_summaries$avg_daily,
  reverse = TRUE  # High traffic = green (good for cycling!)
)

# Alternative: Use bins for clearer categories
color_bins <- colorBin(
  palette = c("#d73027", "#fee08b", "#1a9850"),  # Red -> Yellow -> Green
  domain = station_summaries$avg_daily,
  bins = c(0, 200, 500, 1500),
  pretty = FALSE
)

# Create the map
trondheim_map <- leaflet(station_summaries) %>%
  # Add base map
  addTiles() %>%

  # Center on Trondheim
  setView(lng = 10.4, lat = 63.43, zoom = 12) %>%

  # Add circle markers with color based on traffic
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 8,
    color = "white",
    weight = 2,
    fillColor = ~color_pal(avg_daily),
    fillOpacity = 0.8,
    popup = ~popup_text,
    label = ~name  # Shows name on hover
  ) %>%

  # Add legend
  addLegend(
    position = "bottomright",
    pal = color_pal,
    values = ~avg_daily,
    title = "Average daily<br/>bicycle count",
    opacity = 1
  )

# Display the map
trondheim_map

# Optional: Save the map as HTML
library(htmlwidgets)
saveWidget(trondheim_map, "trondheim_bicycle_map.html")
