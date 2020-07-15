
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(tidyverse)

# Directories
inputdir <- "data/raw/charm"
outputdir <- "data/processed"
plotdir <- "data/figures"


# Inspect one file
################################################################################

# Files
files <- list.files(inputdir)

# Read example file
exdata <- nc_open(file.path(inputdir, files[1]))

# Inspect exdata
print(exdata) # summary
exdata$nvars # number of variables
names(exdata) # properties
names(exdata$dim) # dimensions
names(exdata$var) # variables


# Merge files into a dataframe
################################################################################

# Dates
dates <- files %>% 
  gsub("charm_forecast_0day_", "", .) %>% 
  gsub(".nc", "", .) %>% 
  lubridate::ymd()

# Create raster lists
pn_ras_list <- list()
dap_ras_list <- list()
dac_ras_list <- list()

# Loop through and merge
# i <- 1
# for(i in 1:10){
for(i in 1:length(files)){
  
  # Filename
  filename <- files[i]
  
  # Read NetCDF variables as rasters
  pn_ras <- raster(file.path(inputdir, filename), varname="pseudo_nitzschia")
  dap_ras <- raster(file.path(inputdir, filename), varname="particulate_domoic")
  dac_ras <- raster(file.path(inputdir, filename), varname="cellular_domoic")
  
  # Add rasters to list
  pn_ras_list[[i]] <- pn_ras
  dap_ras_list[[i]] <- dap_ras
  dac_ras_list[[i]] <- dac_ras
  
}

# Create raster bricks
pn_brick <- brick(pn_ras_list) %>% setNames(dates)
dap_brick <- brick(dap_ras_list) %>% setNames(dates)
dac_brick <- brick(dac_ras_list) %>% setNames(dates)

# Export raster bricks
saveRDS(pn_brick, file=file.path(outputdir, "charm_pn_nowcast_brick.Rds"))
saveRDS(dap_brick, file=file.path(outputdir, "charm_dap_nowcast_brick.Rds"))
saveRDS(dac_brick, file=file.path(outputdir, "charm_dac_nowcast_brick.Rds"))

# Convert to dataframe for plotting
pn_df <- as.data.frame(pn_brick, xy=T) %>% 
  gather(key="date", value="prob", 3:ncol(.)) %>% 
  rename(long_dd=x, lat_dd=y) %>% 
  select(date, everything()) %>% 
  mutate(date=as.Date(gsub("\\.", "-", gsub("X", "", date))))

# Export data frame
saveRDS(pn_df, file=file.path(outputdir, "charm_pn_nowcast_dataframe.Rds"))


# Quick plot
################################################################################

# Subset one data
ras_df <- filter(pn_df, date=="2018-06-19")

# Plot raster
g <- ggplot(ras_df, aes(x=long_dd, y=lat_dd, fill=prob)) +
  geom_raster() +
  labs(x="", y="") +
  scale_fill_continuous(name="Probability") +
  theme_bw()
g



