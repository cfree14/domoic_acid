
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sp)
library(rgeos)
library(raster)
library(tidyverse)

# Directories
datadir <- "data/bathymetry/processed"
charmdir <- "data/charm/processed"

# Read data
load(file=file.path(datadir, "ca_bathymetry_data.Rdata"))

# Get USA boundary
usa <- rnaturalearth::ne_countries(country="United States of America", scale="large", returnclass = "sp")

# Get C-HARM brick
charm <- raster::brick(file.path(charmdir, "CHARM_DAC_20140305_to_present_imputed.grd"))


# Build and export data
################################################################################

# Convert from meters to feet
bathy_ft <- ca_bathy * 3.2808


# Reclassify to inside/outside 100 fathoms (600 feet)
reclass_mat <- matrix(data=c(-Inf, -600, NA,
                             -600, 0, 1,
                             0, Inf, NA), byrow=T, ncol=3)
bathy_100fathoms <- reclassify(bathy_ft, rcl=reclass_mat)

plot(bathy_100fathoms)

# Convert to polygon
bathy_100fathoms_poly <- rasterToPolygons(bathy_100fathoms)

# Plot check
plot(bathy_100fathoms_poly)

# Dissolve polygons
bathy_100fathoms_poly_dissolve <- bathy_100fathoms_poly %>% 
  # Dissolve
  sf::st_as_sf() %>% 
  summarize() %>% 
  # Break into multipolyon
  sf::st_cast("POLYGON") %>%
  sf::st_cast("MULTIPOLYGON") %>%
  # Add unique identifier
  mutate(id=1:n()) %>%
  dplyr::select(id, everything()) 

# Clip polyogons to raster
extent(charm) 
xmin <- bbox(charm)[1]
ymin <- bbox(charm)[2]
xmax <- bbox(charm)[3]
ymax <- bbox(charm)[4]
poly <- bathy_100fathoms_poly_dissolve %>% 
  sf::st_crop(sf::st_bbox(c(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))) %>% 
  summarize()

# Check polygon
plot(poly)

# Export polygon
saveRDS(poly, file=file.path(datadir, "CA_100_fathoms_polgyon_crude.Rds"))

# Test on C-HARM data
dac_means <- raster::extract(x = charm, y=poly, method="simple", fun=mean, na.rm=T)

plot(dac_means %>% as.numeric, type="line" )




