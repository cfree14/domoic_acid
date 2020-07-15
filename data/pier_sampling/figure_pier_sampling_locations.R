
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Directories
inputdir <- "data/pier_sampling/raw"
outputdir <- "data/pier_sampling/processed"
plotdir <- "data/pier_sampling/figures"

# Read data
data <- read.csv(file.path(outputdir, "pier_sampling_locations.csv"), as.is=T) %>% 
  mutate(type=plyr::revalue(type, c("percent composition"="Percent composition (%)",
                              "continuous"="Concentration (cells / L)")))

# Get US states
usa <- rnaturalearth::ne_states(country = "United States of America")
usa <- sf::st_as_sf(usa)

# Plot data
################################################################################


# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9),
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

g <- ggplot(data=usa) +
  geom_sf(fill="grey80", col="white", size=0.5) +
  geom_point(data, mapping=aes(x=long_dd, y=lat_dd, col=type, label=long_name), size=0.5) +
  # geom_text_repel(data=data, mapping=aes(x=long_dd, y=lat_dd, label=long_name), size=1, segment.size=0.05) +
  coord_sf(xlim = c(-125, -113), ylim = c(32, 42)) +
  scale_color_discrete(name="Sampling type") +
  labs(x="", y="") +
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "pier_sampling_locations.png"), width=4.5, height=3.5, units="in", dpi=600)
