
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
gisdir <- "data/cdfw/gis_data/raw/StateWaterJurisdiction/"

# Read data
pn_brick <- raster::brick(file.path(datadir, "CHARM_PN_20140305_to_present.grd"))
dap_brick <- raster::brick(file.path(datadir, "CHARM_DAP_20140305_to_present.grd"))
dac_brick <- raster::brick(file.path(datadir, "CHARM_DAC_20140305_to_present.grd"))


# Build data
################################################################################

# Function to impute missing values
# rbrick <- dap_brick
impute_missing_rasters <- function(rbrick){
  
  # Dates
  dates_avail <- names(rbrick) %>% gsub("X", "", .) %>% ymd()
  dates_full <- seq(min(dates_avail), max(dates_avail), "1 day")
  dates_missing <- dates_full[!dates_full %in% dates_avail]
  message(paste0(length(dates_missing), " days missing."))
  
  # Create brick for missing layers
  brick_missing <- rbrick[[1:length(dates_missing)]]
  values(brick_missing) <- NA
  names(brick_missing) <- dates_missing
  
  # Add layers for missing dates
  brick_exp <- stack(rbrick, brick_missing) %>% brick()
  
  # Reorder raster brick by date
  dates_curr <- names(brick_exp) %>% gsub("X", "", .) %>% ymd()
  dates_reorder <- match(dates_full, dates_curr)
  brick_exp1 <- brick_exp[[dates_reorder]] %>% brick()
  
  # Confirm that dates are correct
  date_check <- names(brick_exp1) %>% gsub("X", "", .) %>% ymd() %>% diff() # should all be 1's
  if(any(date_check!=1)){"The raster layers are not all one day apart."}
  
  # Test imputation
  ###################################
  
  # Packages
  # zoo::na.approx
  # raster::approxNA
  # spatialEco::smooth.time.series
  
  if(F){
  
    # Prepare data to test
    dates_test <- seq(ymd("2014-06-05"), ymd("2014-06-07"), "1 day")
    dates_test_names <- dates_test %>% gsub("-", ".", .) %>% paste0("X", .)
    brick_test <- brick_exp1[[dates_test_names]] %>% brick()
    plot(brick_test)
    
    # Perform test imputation using raster::approxNA()
    brick_test_imp <- raster::approxNA(x=brick_test)
    plot(brick_test_imp)
  
  }
  
  
  # Full imputation
  ###################################
  
  # Perform full imputation
  brick_imp <- raster::approxNA(x=brick_exp1)
  
  # Add names
  names(brick_imp) <- dates_full

  # Return data
  output <- list(brick=brick_imp, dates_imputed=dates_missing)
  return(output)
  
}

# Impute missing layers
dap_brick_imp <- impute_missing_rasters(dap_brick)
dac_brick_imp <- impute_missing_rasters(dac_brick)
pn_brick_imp <- impute_missing_rasters(pn_brick)

# Gather imputate dates
imp_dates <- bind_rows(tibble(var="PN", date=pn_brick_imp$dates_imputed),
                       tibble(var="pDA", date=dap_brick_imp$dates_imputed),
                       tibble(var="cDA", date=dac_brick_imp$dates_imputed))

# Export imputed dates
saveRDS(imp_dates, file=file.path(datadir, "CHARM_20140305_to_present_imputed_dates.Rds"))

# Export imputed layers
writeRaster(pn_brick_imp$brick, file=file.path(datadir, "CHARM_PN_20140305_to_present_imputed.grd"), overwrite=T)
writeRaster(dap_brick_imp$brick, file=file.path(datadir, "CHARM_DAP_20140305_to_present_imputed.grd"), overwrite=T)
writeRaster(dac_brick_imp$brick, file=file.path(datadir, "CHARM_DAC_20140305_to_present_imputed.grd"), overwrite=T)





