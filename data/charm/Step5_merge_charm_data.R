
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

# Directories
datadir <- "data/charm/processed"

# Read THREDDS data (3/5/2014-5/7/2019)
pn_thredds <- raster::brick(file.path(datadir, "CHARM_THREDDS_PN_20140305_to_20190507.grd"))
dap_thredds <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAP_20140305_to_20190507.grd"))
dap_thredds_fixed <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAP_20140305_to_20190507_bias_corrected.grd"))
dac_thredds <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAC_20140305_to_20190507.grd"))

# Read ERDDAP data (6/19/2018-present)
pn_erddap <- raster::brick(file.path(datadir, "CHARM_ERDDAP_PN_20180619_to_present.grd"))
dap_erddap <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAP_20180619_to_present.grd"))
dac_erddap <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAC_20180619_to_present.grd"))


# Build data
################################################################################

# Use ERDDAP data over THREDDs data
# Because it is newer and represents the future

# THREDDS dates
thredds_dates <- seq("2014-03-05" %>% ymd(), "2018-06-18" %>% ymd(), by="1 day")
thredds_dates_char <- thredds_dates %>% as.character(.) %>% gsub("-", ".", .) %>% paste0("X", .)

# Remove dates with ERDDAP data from THREDDS data
pn_thredds1 <- pn_thredds[[thredds_dates_char]]
dap_thredds1 <- dap_thredds[[thredds_dates_char]]
dap_thredds_fixed1 <- dap_thredds_fixed[[thredds_dates_char]]
dac_thredds1 <- dac_thredds[[thredds_dates_char]]

# Merge
pn_brick <- stack(pn_thredds1, pn_erddap) %>% brick()
dap_brick <- stack(dap_thredds_fixed1, dap_erddap) %>% brick()
dac_brick <- stack(dac_thredds1, dac_erddap) %>% brick()

# Confirm that layer names are dates
names(pn_brick) %>% gsub("X", "", .) %>% ymd()
names(dap_brick) %>% gsub("X", "", .) %>% ymd()
names(dac_brick) %>% gsub("X", "", .) %>% ymd()

# Export bricks
writeRaster(pn_brick, file=file.path(datadir, "CHARM_PN_20140305_to_present.grd"), overwrite=T)
writeRaster(dap_brick, file=file.path(datadir, "CHARM_DAP_20140305_to_present.grd"), overwrite=T)
writeRaster(dac_brick, file=file.path(datadir, "CHARM_DAC_20140305_to_present.grd"), overwrite=T)

  
  
  
  
  
  
  