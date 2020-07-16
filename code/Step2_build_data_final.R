

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(caret)
library(raster)
library(tidyverse)
library(tidymodels)
library(lubridate)

# Directories
inputdir <- "input"
datadir_cpdh <- "data/da_sampling/processed"
datadir_charm <- "data/charm/processed"
codedir <- "code/functions"
outputdir <- "output"

# Read sample data
data <- readRDS(file.path(inputdir, "CDPH_crab_bivalve_domoic_acid_data.Rds"))

# Read C-HARM data
pn_brick <- raster::brick(file.path(datadir_charm, "CHARM_PN_20140305_to_present_imputed.grd"))
dap_brick <- raster::brick(file.path(datadir_charm, "CHARM_DAP_20140305_to_present_imputed.grd"))
dac_brick <- raster::brick(file.path(datadir_charm, "CHARM_DAC_20140305_to_present_imputed.grd"))

# Source helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Species
table(data$comm_name)

# Species to do
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", 
            "Razor clam", "Bay mussel", "Sea mussel")
var_do <- c("cda")


# 1. Build data
################################################################################

# Build data
if(F){

  # Build data
  data_dcrab <- build_data(species="Dungeness crab", lag=30, action_ppm=30)
  data_rcrab <- build_data(species="Rock crab", lag=30, action_ppm=20)
  data_lobster <- build_data(species="Spiny lobster", lag=30, action_ppm=20)
  data_smussel <- build_data(species="Sea mussel", lag=30, action_ppm=20)
  data_bmussel <- build_data(species="Bay mussel", lag=30, action_ppm=20)
  data_rclam <- build_data(species="Razor clam", lag=30, action_ppm=20)
  data_oyster <- build_data(species="Pacific oyster", lag=30, action_ppm=20)
  
  # Inspect data
  freeR::complete(data_dcrab)
  freeR::complete(data_rcrab)
  freeR::complete(data_lobster)
  freeR::complete(data_smussel) # missing some b/c goes far back
  freeR::complete(data_bmussel) # missing some b/c goes far back
  freeR::complete(data_rclam)
  
  # Export data
  saveRDS(data_dcrab, file=file.path(inputdir, "dungeness_crab_data.Rds"))
  saveRDS(data_rcrab, file=file.path(inputdir, "rock_crab_data.Rds"))
  saveRDS(data_lobster, file=file.path(inputdir, "spiny_lobster_data.Rds"))
  saveRDS(data_smussel, file=file.path(inputdir, "sea_mussel_data.Rds"))
  saveRDS(data_bmussel, file=file.path(inputdir, "bay_mussel_data.Rds"))
  saveRDS(data_rclam, file=file.path(inputdir, "razor_clam_data.Rds"))

}

# Merge individual datasets are built 
x <- spp_do[1]
data_use <- purrr::map_df(spp_do, function(x){
  
  # Read species data
  infile <- x %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., "_data.Rds")
  sdata <- readRDS(file.path(inputdir, infile))
  sdata_use <- sdata %>% 
    filter(!is.na(cda30))
  
  
})

# Export data
saveRDS(data_use, file.path(inputdir, "CDPH_crab_bivalve_domoic_acid_data_use.Rds"))







