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

# ------------ Clean up addresses with custom function ------------------ #

clean_oahu_address <- function(x) {
  
  # convert to character
  x <- as.character(x)
  
  # trim whitespace
  x <- str_trim(x)
  
  # Remove multiple spaces
  x <- str_replace_all(x,"\\s+", " ")
  
  # replace 'OAHU' (any case) with 'HI'
  x <- str_replace_all(x, "(?i)\\bOahu\\b", "HI")
  
  # expand common Hawaii abbreviations, spec for Kam Highway
  x <- str_replace_all(x, "(?i)\\bKAM HWY\\b|\\bKAME HWY\\b", "Kamehameha Hwy")
  x <- str_replace_all(x, "(?i)\\bHWY\\b", "Highway")
  
  # add comma before HI
  x <- str_replace_all(x, " HI\\b", ", HI")
  
  # add comma between city and HI if missing
  # example: "Kapolei HI" → "Kapolei, HI"
  x <- str_replace(x, "([A-Za-z])\\s*,?\\s*HI", "\\1, HI")
  
  # clean up double commas
  x <- str_replace_all(x, ",+", ",")
  
  # normalize comma spacing: " , " → ", "
  x <- str_replace_all(x, "\\s*,\\s*", ", ")
  
  # fix stray spaces again if any
  x <- str_replace_all(x, "\\s+", " ")
  
  return(x)
}

clean_addresses <- lapply(unique_full_address, clean_oahu_address)

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
get_place_type_osm("67-224 Kupahu Street Waialua 96791 Oahu")

# Try with a list

results <- lapply(unique_full_address, get_place_type_osm)

#results <- unlist(results) # ok most of these are NA


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
    user_agent("GetPlaces (mmtb@hawaii.edu)")
  )
  
  txt <- content(res, as = "text", encoding = "UTF-8")
  fromJSON(txt)
}

# ------------------- Try using Google Places -------------------------------- # 

get_place_type_google <- function(address){
  url <- "https://maps.googleapis.com/maps/api/place/findplacefromtext/json"
  
  res <- GET(url, query = list(
    input = address,
    inputtype = "textquery",
    fields = "name,types,formatted_address",
    key = google_key
  ))
  
  txt <- content(res, as = "text", encoding = "UTF-8")
  data <- fromJSON(txt)
  
  if (length(data$candidates) == 0) {
    return(list(name = NA, address = NA, type = NA))
  }
  
  out <- data$candidates
  
  # Handles both data.frame and list cases
  if (is.data.frame(out)) {
    name  <- out$name[1]
    addr  <- out$formatted_address[1]
    types <- paste(out$types[[1]], collapse = ", ")
  } else if (is.list(out)) {
    name  <- out[[1]]$name
    addr  <- out[[1]]$formatted_address
    types <- paste(out[[1]]$types, collapse = ", ")
  } else {
    return(list(name = NA, address = NA, type = NA))
  }
  
  return(list(name = name, address = addr, type = types))
  
  }


get_place_type_google("3840 Paki Avenue, Honolulu, HI")


# test it on our address list 

clean_address_10 <- head(clean_addresses, 30)


results_30 <- lapply(clean_address_10,get_place_type_google)

df_30 <- data.frame(
  input_address = unlist(clean_address_10),
  name  = sapply(results_10, function(x) x$name),
  full_address = sapply(results_10, function(x) x$address),
  type  = sapply(results_10, function(x) x$type),
  stringsAsFactors = FALSE
)

df_30

