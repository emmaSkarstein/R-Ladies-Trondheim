# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R-Ladies Trondheim: Traffic data workshop
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(lubridate)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean the Moholt data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_edges <- readRDS("25_11_11_traffic_data_viz/raw_data/moholt_bicycle_raw.rds")

bike_data <- all_edges %>%
  unnest(node) %>% # Deal with nested structure
  mutate(
    date = as.Date(from), # Convert to actual date
    count = total$volumeNumbers$volume, # Rename count column
    .keep = "none" # Delete original columns
  ) %>%
  mutate( # Create some more useful variables
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE, locale = "English"),
    month_num = month(date),
    weekday = wday(date, label = TRUE, abbr = FALSE, locale = "English"),
    is_weekend = weekday %in% c("Saturday", "Sunday")
  )

glimpse(bike_data)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weather data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://vegvar.atlas.vegvesen.no/#/stasjon/15/tabell

library(readxl)
moholt_weather_raw <- read_excel("25_11_11_traffic_data_viz/raw_data/moholt_weather_raw.xlsx")

# Remove columns that are entirely NA
weather_clean <- moholt_weather_raw %>%
  select(where(~!all(is.na(.)))) %>%
  rename(
    road_temp = matches("Vegbanetemperatur"),
    road_condition = matches("reforhold"),
    friction = matches("Friksjon")
  )

# Check what columns we have left
glimpse(weather_clean)

# Convert date-time and temperature to proper formats, then aggregate to daily
weather_daily <- weather_clean %>%
  mutate(
    datetime = dmy_hm(Tidspunkt),
    date = as.Date(datetime),
    temp = as.numeric(road_temp)
  ) %>%
  group_by(date) %>%
  summarize(
    temp_mean = mean(temp, na.rm = TRUE),
    temp_min = min(temp, na.rm = TRUE),
    temp_max = max(temp, na.rm = TRUE),
    n_obs = sum(!is.na(temp)),
    .groups = "drop"
  ) %>%
  filter(!is.na(temp_mean))

glimpse(weather_daily)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate summary statistics for leaflet popup ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bike_data_all_stations <- read.csv("25_11_11_traffic_data_viz/raw_data/trondheim_all_bicycle_data.csv")
trondheim_stations <- read.csv("25_11_11_traffic_data_viz/raw_data/trondheim_stations.csv")

station_summaries <- bike_data_all_stations %>%
  group_by(station_id, station_name) %>%
  summarize(
    # Overall average
    avg_daily = round(mean(count, na.rm = TRUE)),

    # Seasonal patterns
    summer_avg = round(mean(count[month_num %in% 6:8], na.rm = TRUE)),
    winter_avg = round(mean(count[month_num %in% c(12, 1, 2)], na.rm = TRUE)),

    # Weekday vs weekend
    weekday_avg = round(mean(count[!is_weekend], na.rm = TRUE)),
    weekend_avg = round(mean(count[is_weekend], na.rm = TRUE)),

    # Peak month
    peak_month = {
      monthly_means <- tapply(count, month, mean, na.rm = TRUE)
      names(monthly_means)[which.max(monthly_means)]
    },

    # Number of observations (for data quality check)
    n_days = n(),

    .groups = "drop"
  )

# Join with station locations
station_summaries <- trondheim_stations %>%
  left_join(station_summaries, by = c("id" = "station_id", "name" = "station_name"))

# Create HTML popup text
# Filter and create popup text with NA and NaN handling
station_summaries <- station_summaries %>%
  filter(!is.na(avg_daily)) %>%
  mutate(
    # Replace NaN with NA for easier handling
    across(c(summer_avg, winter_avg, weekday_avg, weekend_avg),
           ~ifelse(is.nan(.), NA, .)),

    # Now create popup text
    popup_text = sprintf(
      "<b>%s</b><br/>
      <b>Daily average:</b> %s bicycles<br/>
      <br/>
      <b>Seasonal averages:</b><br/>
      Summer: %s | Winter: %s<br/>
      <br/>
      <b>Weekday vs weekend averages:</b><br/>
      Weekdays: %s | Weekends: %s<br/>
      <br/>
      <b>Peak month:</b> %s<br/>
      <small>(%s days of data)</small>",
      name,
      as.character(round(avg_daily)),
      ifelse(is.na(summer_avg), "N/A", as.character(round(summer_avg))),
      ifelse(is.na(winter_avg), "N/A", as.character(round(winter_avg))),
      ifelse(is.na(weekday_avg), "N/A", as.character(round(weekday_avg))),
      ifelse(is.na(weekend_avg), "N/A", as.character(round(weekend_avg))),
      ifelse(is.na(peak_month), "N/A", peak_month),
      as.character(n_days)
    )
  )

# View the results
print(station_summaries)




