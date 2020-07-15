
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)
library(lubridate)

# External directories
indir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/da_sampling/2020_request/processed"
charmdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/charm/processed"

# Internal directories
outdir <- "data"
tabledir <- "tables"
plotdir <- "figures"

# Read data
crabs_orig <- readRDS(file.path(indir, "CDPH_crab_viscera_da_data.rds"))
bivalves_orig <- readRDS(file.path(indir, "CDPH_mollusc_viscera_da_data.rds"))


# Merge data
################################################################################

# Columns
colnames(crabs_orig)
colnames(bivalves_orig)
table(bivalves_orig$type)
table(bivalves_orig$tissue)

# Format crabs
crabs <- crabs_orig %>%
  # Add columns
  mutate(catg="Crab",
         date=ymd(date),
         type="wild",
         tissue="viscera") %>% 
  # Arrange columns
  dplyr::select(catg, sampleid, year, date, region, port, area, block_id, block_long_dd, block_lat_dd, long_dd, lat_dd, 
         comm_name, species, type, tissue, da_ppm_prefix, da_ppm) %>% 
  # Determine lat/long
  mutate(latlong=ifelse(!is.na(lat_dd), "site", "block"),
         lat_dd_use=ifelse(!is.na(lat_dd), lat_dd, block_lat_dd),
         long_dd_use=ifelse(!is.na(long_dd), long_dd, block_long_dd)) %>% 
  # Reduce and renae
  dplyr::select(catg, sampleid, year, date, region, port, area, latlong, long_dd_use, lat_dd_use, 
         comm_name, species, type, tissue, da_ppm_prefix, da_ppm) %>% 
  rename(lat_dd=lat_dd_use, 
         long_dd=long_dd_use) %>% 
  # Remove crabs with unknown date
  filter(year!=1900)

# Format bivalves
bivalves <- bivalves_orig %>% 
  # Add columns
  mutate(catg="Bivalve",
         date=ymd(date),
         latlong="site",
         region=ifelse(lat_dd>=38.77, "Northern", "Central")) %>% 
  # Arrange columns
  dplyr::select(catg, sampleid, year, date, region, county, location, latlong, long_dd, lat_dd, comm_name, species, type, tissue, da_ppm_prefix, da_ppm) %>% 
  # Rename columns
  rename(port=county, area=location)
  
# Merge data
data <- bind_rows(crabs, bivalves) %>% 
  mutate(year=year(date)) %>% 
  dplyr::select(catg, sampleid, year, date, everything())

# Inspect data
freeR::complete(data)


# Align with C-HARM grid
################################################################################

# Read C-HARM data
pn_brick <- raster::brick(file.path(charmdir, "CHARM_DAP_20140305_to_present_imputed.grd"))

# Find C-HARM cell centroids
charm_grid_df <- pn_brick[[1]] %>% 
  raster::as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", "risk")) %>% 
  filter(!is.na(risk))

# Format C-HARM cell centroids as sf object
charm_grid_sf <- charm_grid_df %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=raster::crs(pn_brick))

# Check C-HARM cell centroids
g <- ggplot(charm_grid_df, aes(x=long_dd, y=lat_dd, fill=risk)) +
  geom_raster() +
  theme_bw()
g

# Check if CDPH coordinates overlap wth C-HARM grid

# Check if sample coordinates are inside C-HARM brick
check1 <- raster::extract(x= pn_brick[[1]], y=dplyr::select(data, long_dd, lat_dd), df=T) %>% 
  cbind(sampleid=data$sampleid) %>% 
  setNames(c("row", "risk", "sampleid")) %>% 
  dplyr::select(sampleid, risk) %>% 
  mutate(charm_overlap=!is.na(risk)) %>% 
  left_join(data %>% dplyr::select(sampleid, long_dd, lat_dd), by="sampleid")

# Assign closest C-HARM grid cell to each centroid
  
# Convert to sf
check1_sf <- check1 %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=raster::crs(pn_brick)) 

# Ids of neasest C-HARM grid centroid
charm_grid_key_ids <- check1_sf %>% 
  st_nearest_feature(y=charm_grid_sf)

# Key for nearest C-HARM grid centroid
charm_grid_key <- charm_grid_df[charm_grid_key_ids,] %>% 
  dplyr::select(-risk) %>% 
  rename(lat_dd_charm=lat_dd, long_dd_charm=long_dd) %>% 
  cbind(check1) %>% 
  dplyr::select(sampleid, long_dd, lat_dd, risk, charm_overlap, long_dd_charm, lat_dd_charm)

# Check C-HARM overlap with new coordinates
check2 <- raster::extract(x= pn_brick[[1]], y=dplyr::select(charm_grid_key, long_dd_charm, lat_dd_charm), df=T) %>% 
  cbind(sampleid=data$sampleid) %>% 
  setNames(c("row", "risk2", "sampleid")) %>% 
  dplyr::select(sampleid, risk2) %>% 
  mutate(charm_overlap2=!is.na(risk2)) %>% 
  left_join(charm_grid_key, by="sampleid") %>% 
  dplyr::select(sampleid, long_dd:lat_dd_charm, risk2, charm_overlap2, everything())

# Check to see if IDed risks are equivalent
plot(risk2 ~ risk, check2)

# Add C-HARM cell lat/long to data
data2 <- data %>% 
  # Add lat/long of nearest C-HARM cell centroid
  left_join(dplyr::select(charm_grid_key, sampleid, charm_overlap, lat_dd_charm, long_dd_charm), by="sampleid") %>% 
  # For samples not overlapping C-HARM cell centroid, assign coordinates of cell centroid
  mutate(lat_dd_use=ifelse(charm_overlap==F, lat_dd_charm, lat_dd),
         long_dd_use=ifelse(charm_overlap==F, long_dd_charm, long_dd))

# Inspect
freeR::complete(data2)
  
# Export data
saveRDS(data2, file.path(outdir, "CDPH_crab_bivalve_domoic_acid_data.Rds"))


# Build table
################################################################################

# Build table
stats <- data %>% 
  # Calculate n by species-type
  group_by(catg, comm_name, species, type) %>% 
  summarize(nsamples=n()) %>% 
  ungroup() %>% 
  # Summarize by species
  group_by(catg, comm_name, species) %>% 
  summarize(nsamples_tot=sum(nsamples),
            nsamples_by_type=paste(paste(nsamples, type), collapse=", ")) %>% 
  ungroup() %>% 
  # Reduce to usable species
  rename(nsamples=nsamples_tot) %>% 
  arrange(catg, desc(nsamples)) %>% 
  filter(nsamples>=100) %>% 
  # Format table
  mutate(species_name=paste0(comm_name, " (", species, ")")) %>% 
  # Arrange
  dplyr::select(catg, species_name, nsamples, nsamples_by_type)

# Export table
write.csv(stats, file=file.path(tabledir, "Table1_study_species.csv"), row.names=F)
  




