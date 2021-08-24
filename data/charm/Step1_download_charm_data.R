
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

# THREDDS package
# devtools::install_github("bocinsky/thredds")
library(thredds)

# ERDDAP packages
# https://cran.r-project.org/web/packages/rerddap/vignettes/Using_rerddap.html
library(rerddap)

# Directories
inputdir <- "data/charm/raw"
outputdir <- "data/charm/processed"


# THREDDS download (3/5/2014 - 5/5/2019)
################################################################################

# Look up datasets in CeNCOOS THREDDS server
cencoos <- "http://thredds.cencoos.org/thredds/catalog.html"
datasets <- thredds::tds_list_datasets(thredds_url = cencoos)

# Download CHARM nowcast NetCDFs
# Datasets: HAB Pseudo Nitzschia Nowcast, HAB Cellular Domoic Acid Nowcast, HAB Particulate Domoic Acid Nowcast
dataset <- "HAB Cellular Domoic Acid Nowcast"
download_charm_nowcasts <- function(dataset){
  
  # Dataset URL
  url <- datasets$path[datasets$dataset==dataset]
  
  # Available services
  services <- tds_list_services(url)
  
  # NetCDF URL
  ncdf_url <- services$path[services$service=="NetcdfSubset"]
  ncdf_vars <- tds_ncss_list_vars(ncss_url = ncdf_url)
  
  # Download NetCDF
  if(dataset=="HAB Pseudo Nitzschia Nowcast"){outfile_name <- "CHARM_THREDDS_PN_20140305_to_20190507.nc"}
  if(dataset=="HAB Cellular Domoic Acid Nowcast"){outfile_name <- "CHARM_THREDDS_DAC_20140305_to_20190507.nc"}
  if(dataset=="HAB Particulate Domoic Acid Nowcast"){outfile_name <- "CHARM_THREDDS_DAP_20140305_to_20190507.nc"}
  ncdf <- tds_ncss_download(ncss_url = ncdf_url,
                            out_file = file.path(inputdir, outfile_name),
                            var=ncdf_vars$name,
                            bbox = NULL, # download all grid cells
                            ncss_args=list(temporal = "all", accept="netcdf4", addLatLon="true"), # download all dates
                            overwrite=T)
  
  # Read NetCDF as raster brick
  # charm_ncdf <- nc_open(file.path(inputdir, outfile_name)) # deprecated - don't use this
  charm_brick <- raster::brick(ncdf)
  layer_names <- names(charm_brick)

  # Transpose x to y and double flip the map
  # https://stackoverflow.com/questions/44425548/plotting-netcdf-file-with-raster-package-leads-to-distorted-representation-r
  charm_brick_flipped <- flip(flip(t(charm_brick), direction = "y"), direction = "x")
  
  # Rotate from 0:360 to -180:180
  charm_brick_rotated <- rotate(charm_brick_flipped)
  
  # Restore lost layer names
  names(charm_brick_rotated) <- layer_names
  
  # Assign coordinate system
  crs(charm_brick_rotated) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  # Plot raster
  plot_1raster(date="2016-05-20", data=charm_brick_rotated, title=dataset)
  
  # Export brick
  # if(dataset=="HAB Pseudo Nitzschia Nowcast"){outfile_name <- "PN_nowcast_2014present_brick.Rds"}
  # if(dataset=="HAB Cellular Domoic Acid Nowcast"){outfile_name <- "DAC_nowcast_2014present_brick.Rds"}
  # if(dataset=="HAB Particulate Domoic Acid Nowcast"){outfile_name <- "DAP_nowcast_2014present_brick.Rds"}
  # saveRDS(charm_brick_rotated, file=file.path(outputdir, outfile_name))
  
  # Export raster
  if(dataset=="HAB Pseudo Nitzschia Nowcast"){outfile_name <- "CHARM_THREDDS_PN_20140305_to_20190507.grd"}
  if(dataset=="HAB Cellular Domoic Acid Nowcast"){outfile_name <- "CHARM_THREDDS_DAC_20140305_to_20190507.grd"}
  if(dataset=="HAB Particulate Domoic Acid Nowcast"){outfile_name <- "CHARM_THREDDS_DAP_20140305_to_20190507.grd"}
  writeRaster(charm_brick_rotated, file=file.path(outputdir, outfile_name), overwrite=T)
    
}

# Plot one layer
plot_1raster <- function(date, data, title){
  
  # Subset one date
  date1 <- gsub("-", ".", paste0("X", date))
  df <- data[[date1]] %>% 
    as.data.frame(xy=T) %>% 
    setNames(c("long_dd", "lat_dd", "prob"))
  
  # Plot one date
  g <- ggplot(df, aes(x=long_dd, y=lat_dd, fill=prob)) +
    geom_raster() +
    labs(x="", y="") +
    ggtitle(paste0(title, ": ", date)) +
    scale_fill_continuous(name="Probability") +
    theme_bw()
  g
  
}


# Download data
download_charm_nowcasts(dataset="HAB Cellular Domoic Acid Nowcast")
download_charm_nowcasts(dataset="HAB Particulate Domoic Acid Nowcast")
download_charm_nowcasts(dataset="HAB Pseudo Nitzschia Nowcast")


# ERDDAP download (5/8/2019 - present)
################################################################################

# Search ERDDAP server for datasets
charmSearch <- rerddap::ed_search("charm", url="https://coastwatch.pfeg.noaa.gov/erddap/")

# Info on C-HARM datasets
charmInfo <- rerddap::info('charmForecast0day', url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# Download C-HARM datasets
charmData <- rerddap::griddap(x=charmInfo, fields=c("pseudo_nitzschia", "particulate_domoic", "cellular_domoic"), 
                               time=c("2018-06-19", "2021-08-23"),
                               url="https://coastwatch.pfeg.noaa.gov/erddap/", fmt="nc") 

# Inspect data
range(charmData$data$time)

# Convert download to raster bricks
pn_brick <- brick(charmData$summary$filename, varname="pseudo_nitzschia")
dap_brick <- brick(charmData$summary$filename, varname="particulate_domoic")
dac_brick <- brick(charmData$summary$filename, varname="cellular_domoic")

# Get dates
dates <- unique(charmData$data$time) %>% gsub("T12:00:00Z", "", .) %>% ymd()
length(dates)
nlayers(pn_brick)
nlayers(dap_brick)
nlayers(dac_brick)
names(pn_brick) <- names(dap_brick) <- names(dac_brick) <- dates
  
# Rotate from 0 to 360 to -180 to 180
pn_brick_rotated <- rotate(pn_brick)
dap_brick_rotated <- rotate(dap_brick)
dac_brick_rotated <- rotate(dac_brick)

# Plot to check
plot(pn_brick_rotated[[1]])
plot(dap_brick_rotated[[1]])
plot(dac_brick_rotated[[1]])

# Export bricks
writeRaster(pn_brick_rotated, file=file.path(outputdir, "CHARM_ERDDAP_PN_20180619_to_present.grd"), overwrite=T)
writeRaster(dap_brick_rotated, file=file.path(outputdir, "CHARM_ERDDAP_DAP_20180619_to_present.grd"), overwrite=T)
writeRaster(dac_brick_rotated, file=file.path(outputdir, "CHARM_ERDDAP_DAC_20180619_to_present.grd"), overwrite=T)


