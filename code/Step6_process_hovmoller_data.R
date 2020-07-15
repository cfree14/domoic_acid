

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
datadir <- "models/contamination/paper/data"
outputdir <- "models/contamination/paper/output"


# Build data
################################################################################

# Best models
best_models <- c("dungeness_crab_model_rf_cda.Rds",
                 "rock_crab_model_rf_pda.Rds",
                 "spiny_lobster_model_rf_pda.Rds",
                 "razor_clam_model_rf_cda.Rds")

# Loop through models
i <- 4
for(i in 1:length(best_models)){
  
  # Read data
  ###############################################
  
  # Read data
  model_do <- best_models[i]
  infile <- gsub(".Rds", "", model_do) %>% paste0(., "_predictions_range_mask_hovmoller.Rdata")
  load(file.path(outputdir, infile))
  rm(data_df)
  
  
  # Impute values for missing latitudes
  ##############################################
  
  # Impute missing values
  lats_in <- sort(unique(data_stats$lat_dd))  %>% round(., 2)
  lats_should <- seq(min(lats_in), max(lats_in), 0.03)  %>% round(., 2)
  lats_missing <- setdiff(lats_should, lats_in)
  
  # Impute based on razor clam or not razor clam
  if(i!=4){
  
    # Non-razor clam
    # Endpoints for linear interpolation
    data_imputed <- data_stats %>% 
      # Ungroup
      ungroup() %>% 
      # Round latitudes
      mutate(lat_dd=round(lat_dd, 2)) %>% 
      # Remove number of cells
      select(-n) %>% 
      # Reduce to data for latitudes on either side of missing latitudes
      filter(lat_dd >= 36.069 & lat_dd <= 36.221) %>%
      # Spread to ease interpolation
      spread(key="lat_dd", value="pcontam_avg") %>% 
      # Add new columns
      mutate("36.10"=NA,
             "36.16"=NA, 
             "36.19"=NA) %>% 
      # Arrange columns
      select(date, "36.07", "36.10", "36.13", "36.16", "36.19", "36.22") %>% 
      rename("lat1"="36.07", "lat2"="36.10", "lat3"="36.13", "lat4"="36.16", "lat5"="36.19", "lat6"="36.22") %>% 
      # Perform interpolation
      mutate(lat2 = lat1 + (lat3-lat1)/2,
             lat4 = lat3 + (lat6-lat3)/3,
             lat5 = lat6 - (lat6-lat3)/3) %>% 
      # Reduce to interpolated data to be appended to data frame
      select(date, lat2, lat4, lat5) %>% 
      gather(key="lat_dd", value="pcontam_avg", 2:ncol(.)) %>% 
      mutate(n=NA,
             lat_dd=recode(lat_dd, "lat2"="36.10", "lat4"="36.16", "lat5"="36.19") %>% as.numeric()) %>% 
      # Arrange to match data
      select(date, lat_dd, n, pcontam_avg)
  
  }else{
  
    # Razor clam
    # Endpoints for linear interpolation
    data_imputed <- data_stats %>% 
      # Ungroup
      ungroup() %>% 
      # Round latitudes
      mutate(lat_dd=round(lat_dd, 2)) %>% 
      # Remove number of cells
      select(-n) %>% 
      # Reduce to data for latitudes on either side of missing latitudes
      filter(lat_dd >= 39.909 & lat_dd <= 39.971) %>%
      # Spread to ease interpolation
      spread(key="lat_dd", value="pcontam_avg") %>% 
      # Add new columns
      mutate("39.94"=NA) %>% 
      # Arrange columns
      select(date, "39.91", "39.94", "39.97") %>% 
      rename("lat1"="39.91", "lat2"="39.94", "lat3"="39.97") %>% 
      # Perform interpolation
      mutate(lat2 = lat1 + (lat3-lat1)/2) %>% 
      # Reduce to interpolated data to be appended to data frame
      select(date, lat2) %>% 
      gather(key="lat_dd", value="pcontam_avg", 2:ncol(.)) %>% 
      mutate(n=NA,
             lat_dd=recode(lat_dd, "lat2"="39.94") %>% as.numeric()) %>% 
      # Arrange to match data
      select(date, lat_dd, n, pcontam_avg)
  
  }
  
  # Any
  ###########################################
  
  # Build data
  data <- data_stats %>% 
    # Round latitude
    mutate(lat_dd=round(lat_dd,2)) %>% 
    # Append imputed data
    bind_rows(., data_imputed) %>% 
    # Arrange
    arrange(date, lat_dd)

  # Visualize imputation
  g <- ggplot(data, aes(x=date, y=lat_dd, z=pcontam_avg, fill=pcontam_avg)) +
    # Raster
    geom_raster() +
    # Legend
    scale_fill_gradientn(name="p(contaminated)", colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
    # Theme
    theme_bw()
  g
  
  # Extract contours
  ##############################################
  
  # Convert to raster
  event_thresh <- 0.5
  
  # Visualize imputation
  g <- ggplot(data, aes(x=date, y=lat_dd, z=pcontam_avg, fill=pcontam_avg)) +
    # Raster
    geom_raster() +
    # Contours
    geom_contour(breaks= event_thresh, color="black", lwd=0.4) +
    # Legend
    scale_fill_gradientn(name="p(contaminated)", colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
    # Theme
    theme_bw()
  g
  
  # Format dataframe for conversion
  data_df <- data %>% 
    ungroup() %>% 
    # Reduce to important columns
    select(lat_dd, date, pcontam_avg) %>% 
    # Format columns
    mutate(event=ifelse(pcontam_avg>event_thresh, T, NA),
           day=difftime(date, min(date), units="days") %>% as.numeric(), 
           lat=lat_dd - min(lat_dd)) %>% 
    # Reduce to important columns
    select(lat, day, event)
  
  # Plot check
  g <- ggplot(data_df, aes(x=day, y=lat, z=event, fill=event)) +
    geom_raster()
  g
  
  # Convert to raster (first stage)
  data_ras1 <- data_df %>% 
    rasterFromXYZ()
  
  # Convert to polygon (SF)
  data_sf <- data_ras1 %>% 
    # Convert to polygons (SP)
    rasterToPolygons() %>% 
    # Convert to SF
    sf::st_as_sf() %>% 
    # Dissolve into single polygon
    summarize() %>% 
    # Break into seperate polygons
    sf::st_cast("POLYGON") %>%
    sf::st_cast("MULTIPOLYGON") %>%
    # Add event number
    mutate(eventid=1:n())
  
  # Convert SF back to rasters
  data_ras2 <- fasterize(sf=data_sf, raster=data_ras1, field="eventid")
  
  # Convert to dataframe
  data_df2 <- data_ras2 %>% 
    as.data.frame(xy=T) %>% 
    rename(day=y, lat=x, eventid=layer) %>% 
    filter(!is.na(eventid))
  
  # Plot events
  g <- ggplot(data_df2, aes(x=day, y=lat, fill=as.factor(eventid))) +
    geom_tile() +
    theme_bw() +
    labs(title=model_do) +
    theme(legend.position = "none")
  print(g)
  
  # Characterize events
  events_stats <- data_df2 %>% 
    group_by(eventid) %>% 
    summarize(length_days=n_distinct(day),
              height_lat=max(lat) - min(lat),
              ncells=n()) %>% 
    ungroup() %>% 
    gather(key="metric", value="value", 2:ncol(.))
  
  # Plot event characteristics
  g <- ggplot(events_stats, aes(x=value)) +
    facet_wrap(~metric, ncol=3, scales="free") +
    geom_histogram() + theme_bw()
  g
  
  # Save things
  ##############################################
  
  # Clean up names
  events_ras_df <- data_df2
  data_hov_imputed <- data
  
  # Export
  outfile <-   infile <- gsub(".Rds", "", model_do) %>% paste0(., "_predictions_range_mask_hovmoller_imputed_events.Rdata")
  save(data_hov_imputed, events_ras_df, events_stats, 
       file=file.path(outputdir, outfile))
  
}

