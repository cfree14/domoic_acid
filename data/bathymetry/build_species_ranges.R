
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
bathy100dir <- "data/bathymetry/processed"
rangedir <- "models/contamination/paper/data"

# Read fishing grounds
bathy100 <- readRDS(file=file.path(bathy100dir, "CA_100_fathoms_polgyon_crude.Rds"))

# Read state waters
wstate <- sf::st_read("data/cdfw/gis_data/raw/StateWaterJurisdiction/MAN_CA_StateWater.shp") %>% 
  sf::st_transform(crs=crs(bathy100))


# Build ranges
################################################################################

# Lines of latitude:
# Razor clam: North of Pismo Beach - 35.148333
# Dungeness crab: North of Point Conception - 34.448113
# Spiny lobster: South of Monterey Bay (North) - 36.9596007
# Sea mussel and bay mussel - full coast

# Build Dungeness crab range
dcrab_bbox <- sf::st_bbox(bathy100)
dcrab_bbox$ymin <- 34.47
dcrab_bbox <- dcrab_bbox %>% unlist()
dcrab <- sf::st_crop(x=bathy100, y=dcrab_bbox) %>% 
  mutate(species="Dungeness crab")
plot(dcrab)

# Build lobster range
lobster_bbox <- sf::st_bbox(bathy100)
lobster_bbox$ymax <- 36.9596007
lobster_bbox <- lobster_bbox %>% unlist()
lobster <- sf::st_crop(x=bathy100, y=lobster_bbox) %>% 
  mutate(species="Spiny lobster")
plot(lobster)

# Build rock crab range
rcrab <- bathy100 %>% 
  mutate(species="Rock crab")

# Build razor clam range
rclam_bbox <- sf::st_bbox(wstate)
rclam_bbox$ymin <- 35.148333
rclam_bbox <- rclam_bbox %>% unlist()
rclam <- sf::st_crop(x=wstate, y=rclam_bbox) %>% 
  mutate(species="Razor clam") %>% 
  select(species) %>% 
  sf::st_cast("MULTIPOLYGON")
plot(rclam)

# Build sea mussel range
smussel <- wstate %>% 
  mutate(species="Sea mussel") %>% 
  select(species) %>% 
  sf::st_cast("MULTIPOLYGON")

# Build bay mussel range
bmussel <- wstate %>% 
  mutate(species="Bay mussel") %>% 
  select(species)%>% 
  sf::st_cast("MULTIPOLYGON")

# Merge ranges
ranges <- rbind(dcrab, rcrab, lobster, rclam, smussel, bmussel) %>% 
  group_by(species) %>% 
  summarize()

# Export ranges
saveRDS(ranges, file=file.path(rangedir, "species_ranges.Rds"))



