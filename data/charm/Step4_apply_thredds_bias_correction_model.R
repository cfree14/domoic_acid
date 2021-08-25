
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(mgcv)
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)
library(betareg)

# Directories
plotdir <- "data/charm/figures"
datadir <- "data/charm/processed"
gisdir <- "data/cdfw/gis_data/raw/StateWaterJurisdiction/"

# Read THREDDS data (3/5/2014-5/7/2019)
pda_thredds <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAP_20140305_to_20190507.grd"))

# Read ERDDAP data (6/19/2018-present)
pda_erddap <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAP_20180619_to_present.grd"))

# Read THREDDS to ERDDAP model
load(file.path(datadir, "thredds2erddap_data_and_model.Rdata"))


# Apply model to THREDDS data
################################################################################

#
if(F){
  
  # Establish work flow with one layer
  ras <- pda_thredds[[1]]
  names(ras) <- "pda_thredds"
  pred <- raster::predict(object=ras, model=gamfit, type="response")
  plot(pred)
  
  # Make function to predict
  make_pred <- function(ras){
    orig_name <- names(ras)
    names(ras) <- "pda_thredds"
    pred <- raster::predict(object=ras, model=gamfit, type="response")
    names(pred) <- orig_name
    return(pred)
  }
  
  # Test function
  make_pred(ras=pda_thredds[[1]])
  
  # Loop through layers
  n <- nlayers(pda_thredds)
  pred_list <- vector("list", n)
  for(i in 1:n){
    print(i)
    pred_list[[i]] <- make_pred(ras=pda_thredds[[i]])
  }
  
  # Convert to brick
  pred_stack <- raster::stack(pred_list)
  pred_brick <- raster::brick(pred_stack)
  
  # Export
  writeRaster(pred_brick, file=file.path(datadir, "CHARM_THREDDS_DAP_20140305_to_20190507_bias_corrected.grd"), overwrite=T)
  
}

# Confirm that it worked
################################################################################

if(F){
  
  # Read data
  pda_thredds_fixed <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAP_20140305_to_20190507_bias_corrected.grd"))
  
  # Read statewide means
  charm_means <- read.csv(file=file.path(datadir, "CHARM_pn_pda_pca_means_by_server.csv"), as.is=T) %>% 
    mutate(date=ymd(date), 
           variable=recode(variable, "Particulate domoic acid"="pDA"))
  
  # Function
  calc_avg_by_layer <- function(rbrick){
    vals <- sapply(1:nlayers(rbrick), function(x) mean(getValues(rbrick[[x]]), na.rm=T))
    return(vals)
  }
  
  # Build data frame
  build_df <- function(rbrick, var, server){
    vals <- calc_avg_by_layer(rbrick)
    dates <- names(rbrick) %>% gsub("X", "", .) %>% ymd()
    df <- tibble(server=server, variable=var, date=dates, risk=vals)
    return(df)
  }
  
  # Calculate mean of corrected
  pda_thredds_fixed_avg <- build_df(pda_thredds_fixed, var="pDA", server="THREDDS-fixed")
  write.csv(pda_thredds_fixed_avg, file=file.path(datadir, "CHARM_pda_means_thredds_fixed.csv"))
  
  # Merge and plot
  charm_means1 <- bind_rows(charm_means, pda_thredds_fixed_avg) %>% 
    filter(variable=="pDA" & server!="THREDDS")
  
  # Plot data
  g <- ggplot(charm_means1, aes(x=date, y=risk, color=server)) +
    # Add lines
    geom_line(lwd=0.3) + 
    # Axes
    ylim(0,1) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    # Labels
    labs(x="", y="Risk") +
    scale_color_discrete(name="Server") +
    theme_bw()
  g
  
}
  



