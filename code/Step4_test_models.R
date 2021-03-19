

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
codedir <- "code/functions"
inputdir <- "input"
modeldir <- "output/candidate_models"
testdir <- "output/model_tests"

# Source helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Species to do
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", 
            "Razor clam", "Bay mussel", "Sea mussel")

# 4. Test models
################################################################################

# Loop through species
i <- 2
for(i in 1:length(spp_do)){
  
  # Read test data
  print(i)
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  sdata <- readRDS(file.path(inputdir, infile))
  sdata_test <- sdata[["data_test"]]
  
  # Read models
  model_names <- c("Logistic regression (cDA)", "Random forest (cDA)",  "Boosted regression trees (cDA)")
  
  # Read models
  glm_cda <- readRDS(file.path(modeldir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_glm_cda.Rds")))
  rf_cda <- readRDS(file.path(modeldir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_cda.Rds")))
  # rf_cda_lat <- readRDS(file.path(modeldir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_cda_lat.Rds")))
  brt_cda <- readRDS(file.path(modeldir, paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_brt_cda.Rds")))
  
  # Merge into list
  model_list <- list(glm_cda, rf_cda,  brt_cda)
  #  model_list <- list(glm_cda, rf_cda, brt_cda)
  
  # Evaluate models
  model_eval <- evaluate_models(models=model_list, model_names=model_names, test_data=sdata_test)
  
  # Export evaluation
  outfile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_evaluation.Rds")
  saveRDS(model_eval, file=file.path(testdir, outfile))
  
}





