
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)
library(ggplot2)
library(cowplot)

# Directories
inputdir <- "data/bathymetry/raw"
outputdir <- "data/bathymetry/processed"

# Read data
#  Grid registered is authoritative; cell registered is derived from grid registered
# https://catalog.data.gov/dataset/etopo1-1-arc-minute-global-relief-model
data <- raster(file.path(inputdir, "ETOPO1_Ice_c_geotiff.tif"), crs=CRS("+init=epsg:4326"))


# Build and export data
################################################################################

# Clip data to CA
# 134W to 116W abdd 31N to 48N.
ca <- extent(matrix(data=c(-134, -116, 31, 48), nrow=2, byrow=T))
ca_plus <- extent(matrix(data=c(-134.5, -115.5, 30.5, 48.5), nrow=2, byrow=T))
ca_bathy <- crop(data, ca_plus)
plot(ca_bathy)

# Calculate slope
ca_slope <- terrain(ca_bathy, opt="slope", unit="degrees", neighbors = 8)
ca_aspect <- terrain(ca_bathy, opt="aspect", unit="degrees", neighbors = 8)

# Export
save(ca_bathy, ca_slope, ca_aspect, file=file.path(outputdir, "ca_bathymetry_data.Rdata"))


# Plot data
################################################################################

# Format bathy
ca_bathy_df <- ca_bathy %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Format slope
ca_slope_df <- ca_slope %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Format aspect
ca_aspect_df <- ca_aspect %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  setNames(c("value", "x", "y"))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_text(size=5),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.title=element_text(size=5),
                  plot.title=element_text(size=7),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9),
                  legend.position = "bottom",
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot bathy
g1 <- ggplot() +
  # Plot bathymetry
  geom_raster(data=ca_bathy_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_continuous(name="Depth (m)") +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("A. Bathymetric depth") +
  theme_bw() + my_theme
# g1  

# Plot slope
g2 <- ggplot() +
  # Plot bathymetry
  geom_raster(data=ca_slope_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_continuous(name="Slope (degrees)") +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("B. Bathymetric slope") +
  theme_bw() + my_theme
# g2  

# Plot slope
g3 <- ggplot() +
  # Plot bathymetry
  geom_raster(data=ca_aspect_df, mapping=aes(x=x, y=y, fill=value)) +
  scale_fill_continuous(name="Aspect (degrees)") +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Small things
  labs(x="", y="") +
  ggtitle("C. Bathymetric aspect") +
  theme_bw() + my_theme
# g3

# Merge
g <- plot_grid(g1, g2, g3, ncol=3)
g


















