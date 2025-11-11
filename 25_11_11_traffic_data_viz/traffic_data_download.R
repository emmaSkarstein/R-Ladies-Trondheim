# Download data from API

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R-Ladies Trondheim: Traffic data workshop
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(httr) # an alternative is ghql
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

  api_url <- "https://www.vegvesen.no/trafikkdata/api/"

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

  cat("Fetched", nrow(combined_data), "total days\n")

  return(combined_data)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download bicycle data from Moholt ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is the station ID of a bicycle counter at Moholt
station_id <- "52632B1993994"

# Get data from the last 12 months
end_date <- today()
start_date <- end_date - months(12)

# Fetch all data
all_edges <- fetch_all_traffic_data(station_id, start_date, end_date)

saveRDS(all_edges, "25_11_11_traffic_data_viz/raw_data/moholt_bicycle_raw.rds")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# List of bicycle stations in Trondheim ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# API Setup
api_url <- "https://www.vegvesen.no/trafikkdata/api/"

# Find bicycle registration points in Trondheim

# GraphQL query
query_stations <- '{
  trafficRegistrationPoints(
    searchQuery: {
      roadCategoryIds: [F, K, E, R, P]
      trafficType: BICYCLE
      countyNumbers: [50]
    }
  ) {
    id
    name
    location {
      coordinates {
        latLon {
          lat
          lon
        }
      }
      municipality {
        name
      }
    }
    direction {
      from
      to
    }
  }
}'

# Send the request
response_stations <- POST(
  api_url,
  body = list(query = query_stations),
  encode = "json",
  content_type_json()
)

# Parse the response
stations_data <- content(response_stations, as = "text", encoding = "UTF-8") %>%
  fromJSON()


# Extract station information
stations <- stations_data$data$trafficRegistrationPoints %>%
  mutate(
    lat = location$coordinates$latLon$lat,
    lon = location$coordinates$latLon$lon,
    municipality = location$municipality$name,
    direction_from = direction$from,
    direction_to = direction$to
  ) %>%
  select(id, name, lat, lon, municipality, direction_from, direction_to)

# Filter for Trondheim after getting the data
trondheim_stations <- stations %>%
  filter(municipality == "Trondheim")

write.csv(trondheim_stations, "25_11_11_traffic_data_viz/raw_data/trondheim_stations.csv")



# Extract daily data for those stations

# Date range: last 12 months
end_date <- today()
start_date <- end_date - months(12)

# Use the fetch_all_traffic_data function we created earlier

# Download data for all Trondheim stations
all_station_data <- list()

cat("Downloading data for", nrow(trondheim_stations), "stations...\n\n")

for (i in 1:nrow(trondheim_stations)) {

  station_id <- trondheim_stations$id[i]
  station_name <- trondheim_stations$name[i]

  cat("Station", i, "of", nrow(trondheim_stations), ":", station_name, "\n")

  tryCatch({
    # Fetch all data for this station
    edges <- fetch_all_traffic_data(station_id, start_date, end_date)

    # Clean the data
    station_data <- edges %>%
      unnest(node) %>%
      transmute(
        date = as.Date(from),
        count = total$volumeNumbers$volume
      ) %>%
      mutate(
        station_id = station_id,
        station_name = station_name,
        year = year(date),
        month = month(date, label = TRUE, abbr = FALSE),
        month_num = month(date),
        weekday = wday(date, label = TRUE, abbr = FALSE),
        is_weekend = weekday %in% c("Saturday", "Sunday")
      )

    all_station_data[[i]] <- station_data

  }, error = function(e) {
    cat("  ⚠ Error downloading data for", station_name, "\n")
    cat("  ", e$message, "\n")
  })

  # Small delay between stations to be nice to the API
  Sys.sleep(1)
}

# Combine all station data
bike_data_all_stations <- bind_rows(all_station_data)


write_csv(bike_data_all_stations, "25_11_11_traffic_data_viz/raw_data/trondheim_all_bicycle_data.csv")




