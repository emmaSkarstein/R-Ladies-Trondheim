moholt_bicycles_raw <- read.csv("25_11_11_traffic_data_viz/moholt_bicycle.csv", sep = ";")


bicycles <- moholt_bicycles_raw %>%
  mutate()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R-Ladies Trondheim: Bicycle Traffic Data Workshop ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simple script to download and visualize bicycle traffic data from trafikkdata.no

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to fetch all data with pagination ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fetch_all_traffic_data <- function(station_id, start_date, end_date) {

  all_data <- list()
  has_next_page <- TRUE
  after_cursor <- NULL
  page_num <- 1

  while (has_next_page) {

    cat("Fetching page", page_num, "...\n")

    # Build query with pagination
    if (is.null(after_cursor)) {
      # First page
      query <- sprintf('{
        trafficData(trafficRegistrationPointId: "%s") {
          volume {
            byDay(from: "%s", to: "%s", first: 100) {
              pageInfo {
                hasNextPage
                endCursor
              }
              edges {
                node {
                  from
                  to
                  total {
                    volumeNumbers {
                      volume
                    }
                  }
                }
              }
            }
          }
        }
      }',
                       station_id,
                       format(start_date, "%Y-%m-%dT00:00:00+01:00"),
                       format(end_date, "%Y-%m-%dT23:59:59+01:00")
      )
    } else {
      # Subsequent pages
      query <- sprintf('{
        trafficData(trafficRegistrationPointId: "%s") {
          volume {
            byDay(from: "%s", to: "%s", first: 100, after: "%s") {
              pageInfo {
                hasNextPage
                endCursor
              }
              edges {
                node {
                  from
                  to
                  total {
                    volumeNumbers {
                      volume
                    }
                  }
                }
              }
            }
          }
        }
      }',
                       station_id,
                       format(start_date, "%Y-%m-%dT00:00:00+01:00"),
                       format(end_date, "%Y-%m-%dT23:59:59+01:00"),
                       after_cursor
      )
    }

    # Make API request
    response <- POST(
      api_url,
      body = list(query = query),
      encode = "json"
    )

    # Parse response
    raw_data <- content(response, as = "text", encoding = "UTF-8") %>%
      fromJSON()

    # Extract data
    page_data <- raw_data$data$trafficData$volume$byDay

    # Store this page's data
    all_data[[page_num]] <- page_data$edges

    # Check if there are more pages
    has_next_page <- page_data$pageInfo$hasNextPage
    after_cursor <- page_data$pageInfo$endCursor

    page_num <- page_num + 1

    # Be nice to the API - add a small delay
    Sys.sleep(0.5)
  }

  # Combine all pages
  combined_data <- bind_rows(all_data)

  cat("✓ Fetched", nrow(combined_data), "total days\n")

  return(combined_data)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is the station ID of a bicycle counter at Moholt
station_id <- "52632B1993994"

# Get data from the last 12 months
end_date <- today()
start_date <- end_date - months(12)

# Fetch all data
all_edges <- fetch_all_traffic_data(station_id, start_date, end_date)

# Clean it
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
# Download 100 days of data from one bicycle counter ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# API endpoint
api_url <- "https://www.vegvesen.no/trafikkdata/api/"

# Example station ID - we'll update this with a real Trondheim ID
# (Run the other script first to get a list of available stations!)
station_id <- "52632B1993994"

# Date range: last 12 months
end_date <- today()
start_date <- end_date - months(12)

# Build the GraphQL query
query <- sprintf('{
  trafficData(trafficRegistrationPointId: "%s") {
    volume {
      byDay(from: "%s", to: "%s") {
        edges {
          node {
            from
            to
            total {
              volumeNumbers {
                volume
              }
            }
          }
        }
      }
    }
  }
}',
                 station_id,
                 format(start_date, "%Y-%m-%dT00:00:00+01:00"),
                 format(end_date, "%Y-%m-%dT23:59:59+01:00")
)

# Make the API request
response <- POST(
  api_url,
  body = list(query = query),
  encode = "json"
)

# Parse response
raw_data <- content(response, as = "text", encoding = "UTF-8") %>%
  fromJSON()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean the data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract the bicycle counts
bike_data <- raw_data$data$trafficData$volume$byDay$edges %>%
  unnest(node) %>%
  mutate(
    date = as.Date(from),
    count = total$volumeNumbers$volume,
    .keep = "none"
  ) %>%
  mutate(
    # Add useful date components
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE),
    month_num = month(date),
    weekday = wday(date, label = TRUE, abbr = FALSE, locale = "English"),
    is_weekend = weekday %in% c("Saturday", "Sunday")
  )

glimpse(bike_data)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weather data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
moholt_weather_raw <- read_excel("25_11_11_traffic_data_viz/moholt_weather.xlsx")

# Remove columns that are entirely NA
weather_clean <- moholt_weather_raw %>%
  select(where(~!all(is.na(.))))

# Check what columns we have left
glimpse(weather_clean)

# Convert date-time and temperature to proper formats, then aggregate to daily
weather_daily <- weather_clean %>%
  mutate(
    # Parse the datetime (adjust format if needed based on your exact format)
    datetime = dmy_hm(Tidspunkt),
    date = as.Date(datetime),
    # Convert temperature from character to numeric
    temp = as.numeric(`Vegbanetemperatur (ºC)`)
  ) %>%
  # Group by date and calculate daily statistics
  group_by(date) %>%
  summarize(
    temp_mean = mean(temp, na.rm = TRUE),
    temp_min = min(temp, na.rm = TRUE),
    temp_max = max(temp, na.rm = TRUE),
    # Count how many non-NA observations we have
    n_obs = sum(!is.na(temp)),
    .groups = "drop"
  ) %>%
  # Remove days with no valid temperature data
  filter(!is.na(temp_mean))

glimpse(weather_daily)

bike_with_weather <- left_join(bike_data, weather_daily)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exploratory plots with ggplot2 ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Time series
ggplot(bike_data, aes(x = date, y = count)) +
  geom_line(color = "#2E86AB", linewidth = 0.8) +
  labs(
    title = "Daily bicycle counts",
    x = NULL,
    y = "Number of bicycles"
  ) +
  theme_minimal(base_size = 14)

# Time series with dots
ggplot(bike_data, aes(x = date, y = count)) +
  geom_line(color = "#2E86AB", linewidth = 0.8) +
  geom_point(aes(color = is_weekend)) +
  labs(
    title = "Daily bicycle counts",
    x = NULL,
    y = "Number of bicycles"
  ) +
  theme_minimal(base_size = 14)

# Time series with only weekdays
bike_data %>% filter(!is_weekend) %>%
  ggplot(aes(x = date, y = count)) +
  geom_line(color = "#2E86AB", linewidth = 0.8) +
  labs(
    title = "Daily bicycle counts (weekdays)",
    x = NULL,
    y = "Number of bicycles"
  ) +
  theme_minimal(base_size = 14)


# Adding weather data
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
    sec.axis = sec_axis(as.formula(~ . / scale), name = "Road surface temperature")  # Divide, not multiply!
  ) +
  theme_minimal(base_size = 14)








# Seasonal pattern
ggplot(bike_data, aes(x = month, y = count)) +
  geom_boxplot(fill = "#A23B72", alpha = 0.7) +
  labs(
    title = "Seasonal cycling patterns in Trondheim",
    x = "Month",
    y = "Daily bicycle count"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Weekday vs Weekend
ggplot(bike_data, aes(x = weekday, y = count, fill = is_weekend)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#2E86AB", "#F18F01"), guide = "none") +
  labs(
    title = "Bicycle counts by day of week",
    x = NULL,
    y = "Daily bicycle count"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




