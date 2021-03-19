

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(zoo)
library(caret)
library(raster)
library(tidyverse)
library(tidymodels)
library(lubridate)

# Directories
cpdhdir <- "data/da_sampling/processed"
charmdir <- "data/charm/processed"
codedir <- "code/functions"
modeldir <- "output/candidate_models"
preddir <- "output/model_preds"
plotdir <- "figures"

# External directories
depthdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/bathymetry/processed/"

# Read data
# data <- readRDS(file.path(datadir, "CDPH_crab_bivalve_domoic_acid_data.Rds"))
pn_brick <- raster::brick(file.path(charmdir, "CHARM_PN_20140305_to_present_imputed.grd"))
dap_brick <- raster::brick(file.path(charmdir, "CHARM_DAP_20140305_to_present_imputed.grd"))
dac_brick <- raster::brick(file.path(charmdir, "CHARM_DAC_20140305_to_present_imputed.grd"))

# Source helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Projections
wgs84_text <- "+init=epsg:4326"

# Read 100 fathoms polygon
bathy100 <- readRDS(file=file.path(depthdir, "CA_100_fathoms_polgyon_crude.Rds"))



# Make predictions
################################################################################

# Best models
best_models <- c("dungeness_crab_model_rf_cda.Rds",
                 "rock_crab_model_brt_cda.Rds",
                 "spiny_lobster_model_rf_cda.Rds",
                 "razor_clam_model_rf_cda.Rds")


# Loop through models
i <- 1
for(i in 1:length(best_models)){
  
  # Load model
  model_do <- best_models[i]
  model_obj_do <-readRDS(file.path(modeldir, model_do))
  
  # Dates
  # Dates start 31 days in so first prediction has 30 days history
  dates <- names(dap_brick) %>% gsub("X", "", .) %>% ymd()
  # dates_do <- dates[31:50]
  dates_do <- dates[31:length(dates)]
  
  # Loop through dates
  j <- 1
  for(j in 1:length(dates_do)){
    
    # Predict contamination on date
    print(paste(j, dates_do[j]))
    df <- predict_da_contam(date=dates_do[j], model=model_do, model_obj=model_obj_do, plot=F) %>% 
      select(long_dd, lat_dd, pcontam)
    
    # Convert to raster
    ras <- rasterFromXYZ(xyz=df, crs=wgs84_text)
    
    # Merge with other rasters
    if(j==1){rstack <- ras}else{rstack <- stack(rstack, ras)}
    
  }
  
  # Format raster stack names
  names(rstack) <- dates_do 
  
  # Plot check
  # plot(rstack)
  
  # Mask by 100 fathoms
  rstack_mask <- raster::mask(x=rstack, mask=bathy100)
  
  # Plot check
  # plot(rstack_mask)
  
  
  # Export
  ################################################################################
  
  # Build outfile names
  outfile_preds <- paste0(gsub(".Rds", "", model_do), "_predictions.grd")
  outfile_preds_masked <- paste0(gsub(".Rds", "", model_do), "_predictions_100fathoms.grd")
  
  # Export rasters
  writeRaster(rstack, file.path(preddir, outfile_preds), overwrite=T)
  writeRaster(rstack_mask, file.path(preddir, outfile_preds_masked), overwrite=T)
  
}

