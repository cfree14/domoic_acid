
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)
library(ggplot2)
library(cowplot)

# Directories
inputdir <- "data/bathymetry/raw"
outputdir <- "data/bathymetry/processed"

# Read data
# Source: https://earthworks.stanford.edu/catalog/stanford-kd180vp0173
data_orig <- sf::st_read(dsn=file.path(inputdir, "stanford-kd180vp0173-shapefile"), layer="kd180vp0173")

# Check if any of these random columns are unique IDs
anyDuplicated(data_orig$fnode_)
anyDuplicated(data_orig$tnode_)
anyDuplicated(data_orig$nbath50f_) # this one is
anyDuplicated(data_orig$nbath50f_i)

# Format data
data <- data_orig %>% 
  dplyr::select(nbath50f_, depth) %>% 
  rename(contour_id=nbath50f_, depth_ft=depth) %>% 
  mutate(depth_fathom=depth_ft/6) %>% 
  dplyr::select(contour_id, depth_fathom, depth_ft, everything())

data_poly <- sf::st_polygonize(data)
plot(data_poly)

# Plot data
g <- ggplot(data) +
  geom_sf(aes(color=depth_fathom)) +
  scale_color_continuous(name="Depth (fathoms)") +
  theme_bw()
g

# Export data
saveRDS(data, file=file.path(outputdir, "CA_50ft_depth_contours.Rds"))




