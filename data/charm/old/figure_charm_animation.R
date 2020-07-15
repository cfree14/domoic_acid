
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(tidyverse)
library(RColorBrewer)
library(gganimate)
library(gifski)

# Directories
datadir <- "data/processed"
plotdir <- "data/figures"

# Read PN probability
data <- readRDS(file.path(datadir, "charm_pn_nowcast_dataframe.Rds"))


# Format for plotting
################################################################################

# Dates
dates <- sort(unique(data$date))

# Subset one
data1 <- filter(data, date==dates[1])

# Plot one
g <- ggplot(data1, aes(x=long_dd, y=lat_dd, fill=prob)) +
  geom_raster() +
  labs(x="", y="") +
  scale_fill_gradientn(name="Probability", 
                       colors=brewer.pal(9, "YlOrRd"),
                       limits=c(0,1)) +
  theme_bw()
g


# Build animation
################################################################################

# Subset a few dates
data2 <- filter(data, date %in% dates[1:5])

# Build title
a_title <- paste0(spp, "\n", rcp, ": {closest_state}")

# Plot animation
p <- ggplot(data2, aes(x=long_dd, y=lat_dd, fill=prob)) +
  geom_raster() +
  labs(x="", y="") +
  scale_fill_gradientn(name="Probability", 
                       colors=brewer.pal(9, "YlOrRd"),
                       limits=c(0,1)) +
  transition_states(date, state_length = 0.7, transition_length = 0) + 
  ggtitle("{closest_state}") +
  theme_bw()
p    

# Save animation
anim_save(filename=file.path(plotdir, "charm_animation.gif"), dpi=600)
  

