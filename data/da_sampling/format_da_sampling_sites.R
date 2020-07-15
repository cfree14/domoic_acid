

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
datadir <- "data/da_sampling"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "2014_crab_da_sampling_sites.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  mutate(long_dd=long_dd*-1)

# Export data
write.csv(data, file=file.path(datadir, "2014_crab_da_sampling_sites.csv"), row.names = F)


# Plot data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")

# Plot data
g <- ggplot() +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Plot sites and labels
  geom_point(data, mapping=aes(x=long_dd, y=lat_dd, color=state)) +
  geom_text(data, mapping=aes(x=long_dd, y=lat_dd, color=state, label=location), hjust=1.1, size=2) +
  # ggrepel::geom_text_repel(data, mapping=aes(x=long_dd, y=lat_dd, color=state, label=location)) +
  # Crop extent
  coord_sf(xlim = c(-128, -120), ylim = c(37, 48)) +
  # Theme
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_text(size=6),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position="none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export figure
ggsave(g, filename=file.path(datadir, "da_sampling_sites_tristates.png"), 
       width=3.5, height=6.5, units="in", dpi=600)
