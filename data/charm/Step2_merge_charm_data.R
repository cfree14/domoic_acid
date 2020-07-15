
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
pn_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_PN_20140305_to_20190507.grd"))
dap_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAP_20140305_to_20190507.grd"))
dac_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAC_20140305_to_20190507.grd"))

# Read ERDDAP data (5/8/2019-present)
pn_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_PN_20180619_to_present.grd"))
dap_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAP_20180619_to_present.grd"))
dac_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAC_20180619_to_present.grd"))

# Remove overlapping layers
overlapping_layers <- which(names(pn_brick19) %in% names(pn_brick14))
pn_brick19a <- dropLayer(pn_brick19, i = overlapping_layers)
dap_brick19a <- dropLayer(dap_brick19, i = overlapping_layers)
dac_brick19a <- dropLayer(dac_brick19, i = overlapping_layers)

# Merge
pn_brick <- stack(pn_brick14, pn_brick19a) %>% brick()
dap_brick <- stack(dap_brick14, dap_brick19a) %>% brick()
dac_brick <- stack(dac_brick14, dac_brick19a) %>% brick()

# Confirm that layer names are dates
names(pn_brick) %>% gsub("X", "", .) %>% ymd()

# Export bricks
writeRaster(pn_brick, file=file.path(datadir, "CHARM_PN_20140305_to_present.grd"), overwrite=T)
writeRaster(dap_brick, file=file.path(datadir, "CHARM_DAP_20140305_to_present.grd"), overwrite=T)
writeRaster(dac_brick, file=file.path(datadir, "CHARM_DAC_20140305_to_present.grd"), overwrite=T)

  
  
  
  
  
  
  