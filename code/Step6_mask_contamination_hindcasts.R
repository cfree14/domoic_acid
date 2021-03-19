

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
inputdir <- "input"
preddir <- "output/model_preds"

# Read species ranges
ranges <- readRDS(file.path(inputdir, "species_ranges.Rds"))

# Files with predictions
list.files(preddir, "predictions.gri")



# Mask predictions
################################################################################

# Best models
best_models <- c("dungeness_crab_model_rf_cda.Rds",
                 "rock_crab_model_brt_cda.Rds",
                 "spiny_lobster_model_rf_cda.Rds",
                 "razor_clam_model_rf_cda.Rds")


# Loop through models
i <- 1
for(i in 1:length(best_models)){
  
  # Load predictions
  print(i)
  infile <- best_models[i] %>% gsub(".Rds", "", .) %>% paste0(., "_predictions.grd")
  preds <- brick(file.path(preddir, infile))
  
  # Mask by range
  spp <- strsplit(infile, split="_model")[[1]][1] %>% gsub("_", " ", .) %>% stringr::str_to_sentence()
  srange <- ranges %>% 
    filter(species==spp)
  
  # Mask by 100 fathoms
  preds_mask <- raster::mask(x=preds, mask=srange)
  
  # Export
  outfile_preds_masked <- paste0(gsub(".Rds", "", best_models[i]), "_predictions_range_mask.grd")
  writeRaster(preds_mask, file.path(preddir, outfile_preds_masked), overwrite=T)
  
}

