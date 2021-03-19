
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
library(scales)

# Directories
outputdir <-  "output/contam_events"
plotdir <- "figures"

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
    dplyr::select(species, eventid, metric, value)
  
})

# Format data
table(data_orig$metric)


# Plot data
################################################################################

# Format data
data <- data_orig %>% 
  # Convert to wide so you can filter
  spread(key="metric", value="value") %>% 
  # Remove single cell events
  # filter(ncells>1) %>% 
  # Conversions
  mutate(ncells=ncells*0.03, # cells to whole degrees
         height_lat=round(height_lat,2) + 0.03) %>%
  # Format species
  mutate(species=factor(species, levels=study_species))

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title.y = element_text(size=8),
                  axis.title.x = element_blank(),
                  plot.title=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  legend.position = "none")


# Plot max duration
g1 <- ggplot(data, aes(x=species, y=length_days, fill=species)) +
  geom_boxplot(size=0.3, outlier.size = 0.3) +
  # Axes
  scale_y_continuous(trans='log2',
                     breaks = trans_breaks("log2", function(x) 2^x)) +
  # Labels
  labs(x="", y="Duration (days)") +
  # Theme
  theme_bw() + my_theme
g1

# Plot coastal span
g2 <- ggplot(data, aes(x=species, y=height_lat, fill=species)) +
  geom_boxplot(size=0.3, outlier.size = 0.3) +
  # Axes
  scale_y_continuous(trans='log2', 
                     breaks=c(0.03, 0.06, 0.12, 0.25, 0.5, 1, 2, 4, 8),
                     labels=c("0.03", "0.06", "0.12", "0.25", "0.5", "1", "2", "4", "8")) +
  # Labels
  labs(x="", y="Coastal span (degrees latitude)") +
  # Theme
  theme_bw() + my_theme
g2

# Plot size
g3 <- ggplot(data, aes(x=species, y=ncells, fill=species)) +
  geom_boxplot(size=0.3, outlier.size = 0.3) +
  # Axes
  scale_y_continuous(trans='log2',
                     breaks=c(0.03, 0.25, 1, 4, 16, 64, 256, 1024, 4096),
                     labels=c("0.03", "0.25", "1", "4", "16", "64", "256", "1024", "4096")) +
  # Labels
  labs(x="", y="Size (degree days)") +
  # Theme
  theme_bw() + my_theme
g3

# Arrange and save
g <- grid.arrange(g1, g2, g3, ncol=3)

# Export
ggsave(g, filename=file.path(plotdir, "Fig8_contam_event_size_hists.png"), 
       width=6.5, height=2.5, units="in", dpi=600)



