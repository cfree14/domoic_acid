

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
preddir <- "output/model_preds"
hovmollerdir <- "output/hovmoller"


# Build data
################################################################################

# Best models
best_models <- c("dungeness_crab_model_rf_cda.Rds",
                 "rock_crab_model_brt_cda.Rds",
                 "spiny_lobster_model_rf_cda.Rds",
                 "razor_clam_model_rf_cda.Rds")

# Loop through models
i <- 1
for(i in 1:length(best_models)){
  
  # Read data
  model_do <- best_models[i]
  infile <- gsub(".Rds", "", model_do) %>% paste0(., "_predictions_range_mask.grd")
  data_orig <- brick(file.path(preddir, infile))
  print(paste(i, model_do))
  
  # Convert to dataframe
  data_df <- as.data.frame(data_orig, xy=T) %>% 
    # Eliminate empty cells
    na.omit() %>% 
    # Cather wide-to-long
    gather(key="date", value="pcontam", 3:ncol(.)) %>% 
    # Rename columns
    rename(long_dd=x, lat_dd=y) %>% 
    # Format date
    mutate(date = gsub("X", "", date) %>%  ymd()) %>% 
    # Arrange columns
    select(date, everything())
    
  # Calculate date for Hovmöller diagram
  data_stats <- data_df %>% 
    group_by(date, lat_dd) %>% 
    summarize(n=n(),
              pcontam_avg=median(pcontam)) %>% 
    ungroup()
    
  # Export data
  outfile <- gsub(".Rds", "", model_do) %>% paste0(., "_predictions_range_mask_hovmoller.Rdata")
  save(data_df, data_stats, file=file.path(hovmollerdir, outfile))
    
}

