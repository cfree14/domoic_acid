
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)

# Directories
datadir <- "models/contamination/paper/data"
outputdir <- "models/contamination/paper/output"
plotdir <- "models/contamination/paper/figures"

# Study species
study_species <- c("Dungeness crab", "Rock crab", 
                  "Spiny lobster", "Razor clam")


# Build data
################################################################################

# Event files
files2merge <- list.files(outputdir, pattern="event")

# Loop though event files
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x) {
  
  # Load file
  load(file.path(outputdir, x))
  
  # Get species
  spp <- strsplit(x, split="_model")[[1]][1] %>% gsub("_", " ", .) %>% stringr::str_to_sentence()
  
  # Format event statistics
  sdata <- events_stats %>% 
    mutate(species=spp) %>% 
    select(species, eventid, metric, value)
  
})

# Format data
table(data_orig$metric)


# Plot data
################################################################################

# 
g <- ggplot(data_orig %>% filter(species!="Razor clam" & ncells>1), aes(x=species, y=value, fill=species)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y")
g





