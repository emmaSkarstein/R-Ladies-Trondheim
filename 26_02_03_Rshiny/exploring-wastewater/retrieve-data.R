#################################################################################
# Script for retrieving data from the WISEdb - this is slightly slow so the idea
# is to speed things up for the workshop.
#
# Run this script once before working with the app.
#################################################################################

# Required packages
# install.packages("stringr")
# install.packages("RCurl")
# install.packages("data.table")
# install.packages("jsonlite")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("readr")
# install.packages("zoo")
# install.packages("dplyr")

# Function to retrieve data from the WISEdb
get_data <- function(url, accept) {
  full_url <- URLencode(url)
  if (stringr::str_detect(accept, "csv")) {
    csv <- RCurl::getURL(full_url, httpheader = c(Accept = accept))
    df <- data.table::fread(text = csv, sep = ",", header = TRUE, stringsAsFactors = FALSE)
    if (nrow(df) == 0) {
      warning(paste("No data received from", url))
    } else {
      df <- df[, -1] # Return w/o index
    }
  } else if (stringr::str_detect(accept, "json")) {
    json <- RCurl::getURL(full_url, httpheader = c(Accept = accept), .encoding = "UTF-8")
    df <- jsonlite::parse_json(json, simplifyVector = TRUE)
    if (length(df) == 0) {
      warning(paste("No data received from", url))
    } else {
      df <- df[, -1] # Return w/o index
    }
  }
  return(df)
}

library(dplyr)

# Configure API for reading data
api <- "https://wisedb.ethz.ch/api/"

# URls for data after the ? are filtering conditions
pcr_url <- paste0(api, "public/export/dpcr_filtered_averaged/?assay=RESPV6&from=2024-01-01&to=2026-01-01")
wwtp_url <- paste0(api, "public/metadata/wastewater_treatment_plant/?sampling=True")
re_url <- paste0(api, "public/export/r_estimation/?from=2024-01-01&to=2026-01-01")

# Load PCR data from API
PCRdata <- get_data(pcr_url, "text/csv") %>%
  mutate(
    collection_date = lubridate::ymd(collection_date)
  ) %>%
  group_by(wastewater_treatment_plant.name, target) %>%
  tidyr::complete(collection_date = tidyr::full_seq(collection_date, period = 1),
                  fill = list(load = NA)) %>%
  mutate(load_median = zoo::rollapply(load,
                                 width = 7,
                                 FUN = function(x) median(x, na.rm = TRUE),
                                 align = "center",
                                 fill = TRUE,
                                 partial = TRUE)) %>%
  ungroup()

# Write PCR data to csv file
if (dim(PCRdata)[1] != 0) {
  readr::write_csv(PCRdata, "26_02_03_Rshiny/exploring-wastewater/data/PCRdata.csv")
}

# Load wwtp info from API
wwtps <- get_data(wwtp_url, "application/json") %>%
  mutate(across(.cols = c("canton", "name"), .fns = factor))

# Write wwtp info to csv file
if (dim(wwtps)[1] != 0) {
  readr::write_csv(wwtps, "26_02_03_Rshiny/exploring-wastewater/data/wwtps.csv")
}

# Load Re data from API
Redata <- get_data(re_url, "text/csv") %>%
  mutate(date = lubridate::ymd(date))

# Write Re data to csv file
if (dim(Redata)[1] != 0) {
  readr::write_csv(Redata, "26_02_03_Rshiny/exploring-wastewater/data/Redata.csv")
}
