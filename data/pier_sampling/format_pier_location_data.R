
# To-do
# Fix missing and potentially incorrect locations!!!!

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(lubridate)
library(tidyverse)
library(ggmap)
library(sf)
library(mapview)

# Directories
inputdir <- "data/pier_sampling/raw"
outputdir <- "data/pier_sampling/processed"
plotdir <- "data/pier_sampling/figures"

# Read data
data_orig <- import(file.path(inputdir, "pier_sampling_locations.xlsx"))

# Geocoding in R
# https://www.jessesadler.com/post/geocoding-with-r/


# Format data
################################################################################

# Google Maps API key
register_google(key="AIzaSyBa20Ku9Nx7MBgsie2pWotZbQTEAtNBz_k")

# Geocode locations
locations <- mutate_geocode(data_orig, long_name)

# Convert to SF
locations_sf <- st_as_sf(filter(locations, !is.na(lon)), coords = c("lon", "lat"), crs = 4326)

# Plot to check
mapview(locations_sf)

# Format
locations <- locations %>% 
  rename(long_dd=lon, lat_dd=lat)

# Export
write.csv(locations, file=file.path(outputdir, "pier_sampling_locations.csv"), row.names=F)



