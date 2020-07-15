

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

# External directories
cpdhdir <- "data/da_sampling/processed"
charmdir <- "data/charm/processed"

# Internal directories
datadir <- "data"
codedir <- "code/functions"
outputdir <- "output"

# Read data
data <- readRDS(file.path(datadir, "CDPH_crab_bivalve_domoic_acid_data.Rds"))
pn_brick <- raster::brick(file.path(charmdir, "CHARM_PN_20140305_to_present_imputed.grd"))
dap_brick <- raster::brick(file.path(charmdir, "CHARM_DAP_20140305_to_present_imputed.grd"))
dac_brick <- raster::brick(file.path(charmdir, "CHARM_DAC_20140305_to_present_imputed.grd"))

# Source helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Outline steps
# 1. Build data (link CDPH and C-HARM data)
# 2. Divide into training/testing data
# 3. Impute missing values
# 4. Train models
# 5. Inspect model performance

# Species
table(data$comm_name)

# Species to do
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", 
            "Razor clam", "Bay mussel", "Sea mussel")
var_do <- c("pda", "cda")


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
  
  # Inspect data
  freeR::complete(data_dcrab)
  freeR::complete(data_rcrab)
  freeR::complete(data_lobster)
  freeR::complete(data_smussel)
  freeR::complete(data_bmussel)
  freeR::complete(data_rclam)
  
  # Export data
  saveRDS(data_dcrab, file=file.path(outputdir, "dungeness_crab_data.Rds"))
  saveRDS(data_rcrab, file=file.path(outputdir, "rock_crab_data.Rds"))
  saveRDS(data_lobster, file=file.path(outputdir, "spiny_lobster_data.Rds"))
  saveRDS(data_smussel, file=file.path(outputdir, "sea_mussel_data.Rds"))
  saveRDS(data_bmussel, file=file.path(outputdir, "bay_mussel_data.Rds"))
  saveRDS(data_rclam, file=file.path(outputdir, "razor_clam_data.Rds"))

}

# 2. Split data
################################################################################

# Loop through species
i <- 1
for(i in 1:length(spp_do)){
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data.Rds")
  sdata <- readRDS(file.path(outputdir, infile))
  
  # Divide data
  sdata_split <- split_data(sdata, ptrain=0.8)
  
  # Export data
  outfile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  saveRDS(sdata_split, file=file.path(outputdir, outfile))
  
}


# 3. Train models
################################################################################

# Logistic models
#######################################

# Loop through species
for(i in 1:length(spp_do)){
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  sdata <- readRDS(file.path(outputdir, infile))
  
  # Fit models
  sdata_train <- sdata[["data_train"]]
  glm_pda <- fit_glm(data=sdata_train, variable="pda")
  glm_cda <- fit_glm(data=sdata_train, variable="cda")
  
  # Export models
  outfile_pda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_glm_pda.Rds")
  outfile_cda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_glm_cda.Rds")
  saveRDS(glm_pda, file=file.path(outputdir, outfile_pda))
  saveRDS(glm_cda, file=file.path(outputdir, outfile_cda))
  
}

# Random forest models
#######################################

# Loop through species
i <- 1
for(i in 1:length(spp_do)){
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  sdata <- readRDS(file.path(outputdir, infile))
  
  # Fit models
  sdata_train <- sdata[["data_train"]]
  rf_pda <- fit_rf(data=sdata_train, variable="pda")
  rf_cda <- fit_rf(data=sdata_train, variable="cda")
  
  # Export models
  outfile_pda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_pda.Rds")
  outfile_cda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_cda.Rds")
  saveRDS(rf_pda, file=file.path(outputdir, outfile_pda))
  saveRDS(rf_cda, file=file.path(outputdir, outfile_cda))
  
}

# Boosted regression tree models
#######################################

# Loop through species
i <- 1
for(i in 1:length(spp_do)){
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  sdata <- readRDS(file.path(outputdir, infile))
  
  # Training data
  sdata_train <- sdata[["data_train"]]
  
  # Fit  and export models
  
  # pDA
  brt_pda <- fit_brt(data=sdata_train, variable="pda")
  outfile_pda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_brt_pda.Rds")
  saveRDS(brt_pda, file=file.path(outputdir, outfile_pda))
  
  # cDA
  brt_cda <- fit_brt(data=sdata_train, variable="cda")
  outfile_cda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_brt_cda.Rds")
  saveRDS(brt_cda, file=file.path(outputdir, outfile_cda))
  
}


# 4. Test models
################################################################################

# Loop through species
i <- 1
for(i in 1:length(spp_do)){
  
  # Read test data
  print(i)
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  sdata <- readRDS(file.path(outputdir, infile))
  sdata_test <- sdata[["data_test"]]
  
  # Read models
  model_names <- c("Logistic regression (cDA)", "Logistic regression (pDA)",
                   "Random forest (cDA)", "Random forest (pDA)",
                   "Boosted regression trees (cDA)", "Boosted regression trees (pDA)")
  glm_cda <- readRDS(file.path(outputdir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_glm_cda.Rds")))
  glm_pda <- readRDS(file.path(outputdir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_glm_pda.Rds")))
  rf_cda <- readRDS(file.path(outputdir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_cda.Rds")))
  rf_pda <- readRDS(file.path(outputdir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_pda.Rds")))
  brt_cda <- readRDS(file.path(outputdir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_brt_cda.Rds")))
  brt_pda <- readRDS(file.path(outputdir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_brt_pda.Rds")))
  model_list <- list(glm_cda, glm_pda, rf_cda, rf_pda, brt_cda, brt_pda)
  # model_list <- list(glm_cda, glm_pda, rf_cda, rf_pda)
  
  
  # Evaluate models
  model_eval <- evaluate_models(models=model_list, model_names=model_names, test_data=sdata_test)
  
  # Export evaluation
  outfile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_evaluation.Rds")
  saveRDS(model_eval, file=file.path(outputdir, outfile))
  
}


















