
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# THREDDS package
# devtools::install_github("bocinsky/thredds")
library(thredds)

# Packages
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "data/charm/figures"
datadir <- "data/charm/processed"
bathydir <- "data/bathymetry/processed"

# Read fishing grounds (100-fathoms) polygon
bathy100 <- readRDS(file.path(bathydir, "CA_100_fathoms_polgyon_crude.Rds"))


# Build data
################################################################################

# C-HARM variables
vars <- c("PN", "DAP", "DAC")

# Loop through C-HARM variables and build data
x <- vars[1]
data <- purrr::map_df(vars, function(x) {
  
  # Read data
  infile <- paste0("CHARM_", x, "_20140305_to_present_imputed.grd")
  brick_orig <- raster::brick(file.path(datadir, infile))

  # Mask data by 100-fathoms
  brick_mask <- raster::mask(brick_orig, bathy100)
  # plot(brick_mask)
  
  # Convert to data frame
  brick_df <- as.data.frame(brick_mask, xy=T) %>% 
    # Eliminate empty cells
    na.omit() %>% 
    # Cather wide-to-long
    gather(key="date", value="risk", 3:ncol(.)) %>% 
    # Rename columns
    rename(long_dd=x, lat_dd=y) %>% 
    # Format date
    mutate(date = gsub("X", "", date) %>%  ymd()) %>% 
    # Arrange columns
    select(date, everything())
  
  # Calculate date for Hovmöller diagram
  brick_stats <- brick_df %>% 
    group_by(date, lat_dd) %>% 
    summarize(n=n(),
              risk_avg=mean(risk)) %>% 
    mutate(variable=x) %>% 
    select(variable, everything())
  
})

# Export data
saveRDS(data, file=file.path(datadir, "CHARM_20140305_to_present_imputed_hovmoller_data.Rds"))


