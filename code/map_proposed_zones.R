

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
datadir <- "data/da_sampling/proposed_expansion"

# Read data
sites_orig <- readxl::read_excel(file.path(datadir, "CDFW_2020_proposed_biotoxin_zones_sites.xlsx"), sheet=1)
zones <- readxl::read_excel(file.path(datadir, "CDFW_2020_proposed_biotoxin_zones_sites.xlsx"), sheet=2)

# Format data
sites <- sites_orig %>% 
  mutate(type=recode_factor(type,
                           "required"="Existing (required)",
                           "informational"="Existing (informational)",
                           "new"="Proposed"))

# Plot data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Zone labels
zone_df <- tibble(label=LETTERS[1:9],
                  long_dd=-126,
                  lat_dd=c(41.6, 41, 40.4, 39.9, # A, B, C, D,
                           39.2, 38.4, 37.6, 36.6, # E, F, G, H, 
                           35.5)) # 1

# Plot data
g <- ggplot() +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Labels
  labs(title="Biotoxin collection sites") +
  # Plot sites and labels
  geom_point(sites, mapping=aes(x=long_dd, y=lat_dd, color=type)) +
  geom_text(sites, mapping=aes(x=long_dd, y=lat_dd, color=type, label=sampling_site), hjust=1.1, size=2) +
  # ggrepel::geom_text_repel(data, mapping=aes(x=long_dd, y=lat_dd, color=state, label=location)) +
  # Plot zone boundaries
  geom_hline(data=zones, mapping=aes(yintercept=lat_dd), linetype="dotted") +
  geom_text(data=zone_df, mapping=aes(x=long_dd, y=lat_dd, label=label), hjust=1, size=3, fontface="bold") +
  # Crop extent
  coord_sf(xlim = c(-126, -116), ylim = c(32, 42)) +
  # Legend
  scale_color_discrete(name="Site type") +
  # Theme
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_text(size=8),
        legend.text=element_text(size=8),
        legend.title =element_text(size=9),
        plot.title =element_text(size=10),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position=c(0.2,0.1), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export figure
ggsave(g, filename=file.path(datadir, "da_sampling_sites_expanded.png"), 
       width=4.5, height=5.75, units="in", dpi=600)
