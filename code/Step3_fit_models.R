

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
outputdir <- "output/candidate_models"

# Read sample data
data <- readRDS(file.path(inputdir, "CDPH_crab_bivalve_domoic_acid_data_use.Rds"))

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
var_do <- c("cda")


# 2. Split data
################################################################################

# Loop through species
i <- 1
for(i in 1:length(spp_do)){
  
  # Read data
  sdata <- data  %>% 
    filter(comm_name==spp_do[i])
  
  # Divide data
  sdata_split <- split_data(sdata, ptrain=0.8)
  
  # Export data
  outfile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  saveRDS(sdata_split, file=file.path(inputdir, outfile))
  
}


# 3. Train models
################################################################################

# Logistic models
#######################################

# Loop through species
i <- 1
for(i in 1:length(spp_do)){
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  sdata <- readRDS(file.path(inputdir, infile))
  sdata_train <- sdata[["data_train"]]
  
  # # Fit cDA models
  # glm_cda <- fit_glm(data=sdata_train, variable="cda")
  # outfile_cda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_glm_cda.Rds")
  # saveRDS(glm_cda, file=file.path(outputdir, outfile_cda))
  
  # Fit cDA models + latitude
  glm_cda <- fit_glm(data=sdata_train, variable="cda", lat=T)
  outfile_cda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_glm_cda_lat.Rds")
  saveRDS(glm_cda, file=file.path(outputdir, outfile_cda))
  
  # Fit pDA models
  # glm_pda <- fit_glm(data=sdata_train, variable="pda")
  # outfile_pda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_glm_pda.Rds")
  # saveRDS(glm_pda, file=file.path(outputdir, outfile_pda))

}

# Random forest models
#######################################

# Loop through species
# i <- 1
for(i in 1:2){
# for(i in 1:length(spp_do)){
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  sdata <- readRDS(file.path(inputdir, infile))
  sdata_train <- sdata[["data_train"]]
  
  # # Fit cDA models
  # rf_cda <- fit_rf(data=sdata_train, variable="cda")
  # outfile_cda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_cda.Rds")
  # saveRDS(rf_cda, file=file.path(outputdir, outfile_cda))
  
  # Fit cDA models
  rf_cda <- fit_rf(data=sdata_train, variable="cda", lat=T)
  outfile_cda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_cda_lat.Rds")
  saveRDS(rf_cda, file=file.path(outputdir, outfile_cda))
  
  # Fit pDA models
  # rf_pda <- fit_rf(data=sdata_train, variable="pda")
  # outfile_pda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_rf_pda.Rds")
  # saveRDS(rf_pda, file=file.path(outputdir, outfile_pda))
  
}

# Boosted regression tree models
#######################################

# Loop through species
i <- 1
for(i in 1:length(spp_do)){
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_data_split.Rds")
  sdata <- readRDS(file.path(inputdir, infile))
  sdata_train <- sdata[["data_train"]]
  
  # Fit cDA models
  brt_cda <- fit_brt(data=sdata_train, variable="cda")
  outfile_cda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_brt_cda.Rds")
  saveRDS(brt_cda, file=file.path(outputdir, outfile_cda))
  
  # Fit pDA models
  # brt_pda <- fit_brt(data=sdata_train, variable="pda")
  # outfile_pda <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_brt_pda.Rds")
  # saveRDS(brt_pda, file=file.path(outputdir, outfile_pda))
  
}













