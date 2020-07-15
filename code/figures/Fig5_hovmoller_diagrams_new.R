
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
study_species <- c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam")


# Build example raster data
################################################################################

# Imputed events files
files2merge <- list.files(outputdir, pattern="range_mask_hovmoller.Rdata")

# Loop through files and extract raster for a singe date
x <- files2merge[1]
rdata <- purrr::map_df(files2merge, function(x) {
  # Load file
  load(file.path(outputdir, x))
  # Get species
  spp <- strsplit(x, split="_model")[[1]][1] %>% gsub("_", " ", .) %>% stringr::str_to_sentence() 
  # Format data
  data_df1 <- data_df %>% 
    mutate(species=spp) %>% 
    filter(date==ymd("2016-01-01"))

})

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    plot.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank())

# Function to plot map
plot_map <- function(species, ylim){
  
  # Get US states and Mexico
  usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
  mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
  
  # Subset data
  spp_do <- species
  sdata <- rdata %>% 
    filter(species==spp_do)
  
  # Plot map
  g1 <- ggplot() +
    # Plot p(contamination)
    geom_tile(data=sdata, mapping=aes(x=long_dd, y=lat_dd, fill=pcontam)) +
    # Add California and Mexico
    geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
    geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
    # Crop extent
    coord_sf(xlim = c(-125, -120), ylim = ylim) +
    # Labels
    labs(x=" ", y="") +
    # Legend
    scale_fill_gradientn(name="p(contaminated)", 
                         colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="none",
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.title.y = element_blank())
    
  g1
  
}



# Plot maps
map1 <- plot_map(species="Dungeness crab", ylim=c(34,42))
map2 <- plot_map(species="Rock crab", ylim=c(32,42))
map3 <- plot_map(species="Spiny lobster", ylim=c(32,35))
map4 <- plot_map(species="Razor clam", ylim=c(37,42))

# Plot Hovmoller
hov1 <- ggplot(mapping=aes(x=1:10, y=1:10)) +
  theme_bw() + base_theme

# Merge
map_width <- 0.25
g <- grid.arrange(map1, hov1, hov1, 
                  map2, hov1, hov1, 
                  map3, hov1, hov1, 
                  map4, hov1, hov1, ncol=3, widths=c(map_width, rep((1-map_width)/2, 2)))

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_hovmoller_diagrams_working.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




