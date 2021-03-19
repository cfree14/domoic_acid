

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
library(fasterize)

# Directories
closuredir <- "data/closures/data"
hovmollerdir <- "output/contam_events"

# Study species
study_species <- c("Dungeness crab", "Rock crab", 
                   "Spiny lobster", "Razor clam")

# Model key for species
model_key <- tibble(species = study_species,
                    model = c("dungeness_crab_model_rf_cda.Rds",
                              "rock_crab_model_brt_cda.Rds",
                              "spiny_lobster_model_rf_cda.Rds",
                              "razor_clam_model_rf_cda.Rds"))

# Read and format advisories data
################################################################################

# Read closures data
closures_orig <- readRDS(file.path(closuredir, "CDFW_2015_2020_fishery_closures.Rds"))
advisories_orig <- readRDS(file.path(closuredir, "CDPH_2014_2020_health_advisories.Rds"))

# Format for merge
advisories_use <- advisories_orig %>% 
  # Reduce to study species
  filter(comm_name %in% c("Dungeness crab", "Rock crab", "Spiny lobster", "Clams")) %>% 
  # TEMPORARY: rename clamas
  mutate(comm_name=recode(comm_name, "Clams"="Razor clam")) 


# Function to build data
################################################################################


species <- "Dungeness crab"
build_data <- function(species){
  
  # Species
  species_do <- species
  
  # Read data
  ##############################
  
  # Read Hovmoller data
  model_do <- model_key %>% filter(species==species_do) %>% pull(model) %>% gsub(".Rds", "", .)
  infile <- paste0(model_do, "_predictions_range_mask_hovmoller_imputed_events.Rdata")
  load(file=file.path(hovmollerdir, infile))
  
  # Get advisory data
  advisories <- advisories_use %>% 
    filter(comm_name==species)
  
  # Format data
  ##############################
  
  # Build date key
  range(advisories$date)
  range(data_hov_imputed$date)
  date_key <- tibble(date=sort(unique(data_hov_imputed$date))) %>% 
    mutate(date_id=1:n())
  
  # Build advisories raster
  advisories_ras <- advisories %>% 
    # Clip to match Hovmoller data
    filter(date >= min(data_hov_imputed$date) & date <= max(data_hov_imputed$date)) %>% 
    # Add date id
    left_join(date_key) %>% 
    # Reduce
    select(date_id, lat_dd, advisory) %>% 
    # Reclassify advisories
    mutate(advisory=recode(advisory, "Out-of-season"=0, "None"=1, "Partial"=2, "Full"=2) %>% as.numeric()) %>% 
    # Convert to raster
    raster::rasterFromXYZ()
  image(advisories_ras)
    
  # Build contamination rasters
  risk_hi <- 0.5
  risk_hi_ras <- data_hov_imputed %>% 
    # Add date id
    left_join(date_key) %>% 
    # Add classification
    mutate(class=ifelse(pcontam_avg>=risk_hi, 50, 25)) %>% 
    # Reduce
    select(date_id, lat_dd, class) %>% 
    # Convert to raster
    raster::rasterFromXYZ()
  image(risk_hi_ras)
  
  # Low risk
  risk_lo <- 1/6
  risk_lo_ras <- data_hov_imputed %>% 
    # Add date id
    left_join(date_key) %>% 
    # Add classification
    mutate(class=ifelse(pcontam_avg<=risk_lo, 50, 25)) %>% 
    # Reduce
    select(date_id, lat_dd, class) %>% 
    # Convert to raster
    raster::rasterFromXYZ()
  image(risk_lo_ras)
  
  # Inspect resolutions
  res(risk_hi_ras)
  res(advisories_ras)
  
  # Scale closures to match contamination
  advisories_ras_scaled <- raster::resample(x=advisories_ras, y=risk_lo_ras, method="ngb")
  image(advisories_ras_scaled)
  res(advisories_ras_scaled)
 
  # Identify in-season areas with high DA risk and no advisory (missing closures)
  ras1 <- advisories_ras_scaled + risk_hi_ras
  ras1_df <- ras1 %>% 
    # Convert to dataframe
    as.data.frame(xy=T) %>% 
    # Give names
    setNames(c("date_id", "lat_dd", "catg_code")) %>% 
    # Add real date
    left_join(date_key) %>% 
    # Create categories
    mutate(risk=ifelse(catg_code>=50, "High", "Low"),
           status=recode(catg_code, 
                         "25"="Out-of-season",
                         "26"="In season",
                         "27"="Closure",
                         "50"="Out-of-season",
                         "51"="In season",
                         "52"="Closure"),
           catg_name=recode(catg_code, 
                            "25"="Out-of-season (low risk)",
                            "26"="In season (low risk)",
                            "27"="Closure (low risk)",
                            "50"="Out-of-season (high risk)",
                            "51"="In season (high risk)",
                            "52"="Closure (high risk)")) %>% 
    # Arrange columns
    mutate(comm_name=species, 
           type="Identifying overlooked closures") %>% 
    select(comm_name, type, date_id, date, lat_dd, catg_name, status, risk)
  
  # Plot
  g <- ggplot(ras1_df, aes(x=date, y=lat_dd, fill=risk)) +
    geom_tile()
  g
  
  # Identify in-season areas with low DA risk but an advisory none-the-less (precautionary closures)
  ras2 <- advisories_ras_scaled + risk_lo_ras
  ras2_df <- ras2 %>% 
    # Convert to dataframe
    as.data.frame(xy=T) %>% 
    # Give names
    setNames(c("date_id", "lat_dd", "catg_code")) %>% 
    # Add real date
    left_join(date_key) %>% 
    # Create categories
    mutate(risk=ifelse(catg_code>=50, "Low", "Moderate-high"),
           status=recode(catg_code, 
                         "25"="Out-of-season",
                         "26"="In season",
                         "27"="Closure",
                         "50"="Out-of-season",
                         "51"="In season",
                         "52"="Closure"),
           catg_name=recode(catg_code, 
                            "25"="Out-of-season (moderate-high risk)",
                            "26"="In season (moderate-high risk)",
                            "27"="Closure (moderate-high risk)",
                            "50"="Out-of-season (low risk)",
                            "51"="In season (low risk)",
                            "52"="Closure (low risk)")) %>% 
    # Arrange columns
    mutate(comm_name=species, 
           type="Identifying precautionary closures") %>% 
    select(comm_name, type, date_id, date, lat_dd, catg_name, status, risk)
  
  # Plot
  g <- ggplot(ras2_df, aes(x=date, y=lat_dd, fill=catg_name)) +
    geom_tile()
  g
  
  # Merge data
  data <- bind_rows(ras1_df, ras2_df)
  
  # Return
  return(data)
  
}

# Build data
################################################################################

# Build data
data <- purrr::map_df(study_species, function(x) build_data(species=x))

# Export data
saveRDS(data, file=file.path(hovmollerdir, "mismatch_in_advisories_and_contamination.Rds"))








