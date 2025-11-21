## API identification of Non-Dwellings

# This script attempts to assign place types to permit applicatons we have for TMKS that were not present in our dwell data
# We'll use the address column and the Open Street Map Api to try and do this. 


# ---------- Load Packages and set up File Paths ------------------------------------ #


library(httr)
library(jsonlite)
library(tidyverse)
library(sf)
library(here)
library(janitor)

source(here("workflow/setup.R"))

# ------------ Read in IWS Permit data that did not match any Dwell data ------- #

iws_permits_withnodwell <- read_csv(file.path(driver_dir,"4_OSDS_N_Runoff/output-data/qaqc/iws_oahu_permits_nodwellings.csv")) 

# reformat an address column with all address information

iws_nodwell_address <- iws_permits_withnodwell %>% 
  clean_names() %>%
  mutate(full_address = ifelse(!is.na(project_street_address2),
                               paste(project_street_address,project_street_address2,project_city,project_zip_code, island),
                               paste(project_street_address,project_city,project_zip_code, island))
         ) %>% 
  mutate(full_address = str_replace_all(full_address,"NA",""))

unique_full_address <- unique(iws_nodwell_address$full_address)

# --------- Function to return info from OSM --------------- #

get_place_type_osm <- function(address) {
  url <- "https://nominatim.openstreetmap.org/search"
  
  res <- GET(url, query = list(
    q = address,
    format = "json",
    addressdetails = 1
  ), user_agent("GetPlaces (mmtb@hawaii.edu"))  # user agent is
  
  raw_txt <- content(res, as = "text", encoding = "UTF-8")
  data <- fromJSON(raw_txt)
  
  if (length(data) > 0) {
    return(list(
      name = data$display_name[1],
      class = data$class[1],
      type = data$type[1]
    ))
  } else {
    return(list(name = NA, class = NA, type = NA))
  }
}

# Test with an address for a park:
get_place_type_osm("3840 Paki Avenue, Honolulu, HI")

# Try with a list

results <- lapply(unique_full_address, get_place_type_osm)

results <- unlist(results)


# ---------------------- Function to reverse Geocode from Lat and Lons -------------------------- #

get_reverse_osm <- function(lat, lon) {
  url <- "https://nominatim.openstreetmap.org/reverse"
  
  res <- GET(
    url,
    query = list(
      lat = lat,
      lon = lon,
      format = "json",
      zoom = 18,
      addressdetails = 1,
      polygon_geojson = 1
    ),
    user_agent("MyAddressLookup/1.0 (your_email@example.com)")
  )
  
  txt <- content(res, as = "text", encoding = "UTF-8")
  fromJSON(txt)
}
