
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

# Build sampling sites reference file
################################################################################

# Read sampling data
samples <- readRDS(file.path(datadir, "CDPH_crab_bivalve_domoic_acid_data.Rds")) %>% 
  # Get/rename columns
  select(comm_name, date, lat_dd, da_ppm) %>% 
  rename(species=comm_name) %>% 
  # Reduce to study species and factor
  filter(species %in% study_species) %>% 
  mutate(species=factor(species, study_species)) %>% 
  # Get rid of 1900 values
  filter(year(date)>=2014)
  
# Read and format Dungeness crab ports
dcrab_sites <- read.csv("data/cdfw/landings_public/processed/dungeness_ports.csv", as.is=T) %>% 
  filter(da_sampling=="routine") %>% 
  select(port, lat_dd) %>% 
  rename(site_name=port) %>% 
  mutate(species="Dungeness crab") %>% 
  select(species, everything())

# Read and format razor clam sites
rclam_sites <- read.csv("models/contamination/paper/data/bivalve_sampling_sites.csv", as.is=T) %>% 
  filter(comm_name=="Razor clam") %>% 
  select(comm_name, site, lat_dd) %>% 
  rename(site_name=site, species=comm_name)
  
# Build site key
site_key <- bind_rows(dcrab_sites, rclam_sites)


# Build data
################################################################################

# Build raster data
##################################

# Models
models <- c("dungeness_crab_model_rf_cda.Rds",
            "rock_crab_model_rf_pda.Rds",
            "spiny_lobster_model_rf_pda.Rds",
            "razor_clam_model_rf_cda.Rds")

# Loop through models
x <- models[1]
data_orig <- purrr::map_df(models, function(x) {
  
  # Model
  model_do <- gsub(".Rds", "", x)
  infile <- paste0(model_do, "_predictions_range_mask_hovmoller.Rdata")
  
  # Read model
  load(file=file.path(outputdir, infile))
  
  # Format data
  if(exists("dcrab_df")){data_df <- dcrab_df} # delete this later when you fixed predict code
  df <- data_df %>% 
    mutate(species=gsub("_model_rf_cda|_model_rf_pda", "", model_do) %>% 
             gsub("_", " ", .) %>% stringr::str_to_sentence()) %>% 
    select(species, everything())
  
})

# Format data
data <- data_orig %>% 
  # Format species
  mutate(species=factor(species, levels=study_species))
  

# Build # of event data
##################################

# Number of contamination events at each latitude
nevents <- data %>%
  group_by(species, lat_dd) %>% 
  summarize(nevents=sum(pcontam > 0.5)) %>% 
  ungroup()


# Plot data
################################################################################

# Sample data for fast plotting
data_sample <- data %>% 
  sample_frac(size=0.01)

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=10),
                    plot.title=element_text(size=12),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=10),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank())


# Not razor clam
#################################

# Plot rasters
g1 <- ggplot(data %>% filter(species!="Razor clam"), aes(x=date, y=lat_dd, fill=pcontam)) +
  # Plot rasters
  facet_wrap(~species, ncol=1) +
  geom_tile() +
  # Add sampling lines
  geom_hline(data=site_key %>% filter(species!="Razor clam"), mapping=aes(yintercept=lat_dd), col="grey30") +
  # Add sampling points
  geom_point(samples %>% filter(species!="Razor clam"), mapping=aes(x=date, y=lat_dd), inherit.aes = F, size=0.8, pch=1) +
  # Labels
  labs(x="Day", y="Latitude (°N)") +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  scale_fill_gradientn(name="Proportion\ncontaminated", 
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
g1

# Plot bar plots
g2 <- ggplot(nevents  %>% filter(species!="Razor clam"), aes(x=lat_dd, y=nevents)) +
  # Plot # of events
  facet_wrap(~species, ncol=1) +
  geom_area(fill="grey60") +
  # Add sampling lines
  geom_vline(data=site_key %>% filter(species!="Razor clam"), mapping=aes(xintercept=lat_dd), col="grey30") +
  # Flip vertical
  coord_flip() +
  # Labels
  labs(x="", y="# of events") +
  scale_x_continuous(breaks=seq(0,5000,2500)) +
  # Theme
  theme_bw() + base_theme +
  # Eliminate y-axis
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
g2

# Not razor clam
#################################

# Plot rasters
g3 <- ggplot(data %>% filter(species=="Razor clam"), aes(x=date, y=lat_dd, fill=pcontam)) +
  # Plot rasters
  facet_wrap(~species, ncol=1) +
  geom_tile() +
  # Add sampling lines
  geom_hline(data=site_key %>% filter(species=="Razor clam"), mapping=aes(yintercept=lat_dd), col="grey30") +
  # Add sampling points
  geom_point(samples %>% filter(species=="Razor clam"), mapping=aes(x=date, y=lat_dd), inherit.aes = F, size=1, pch=1) +
  # Labels
  labs(x="Day", y="Latitude (°N)") +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  scale_fill_gradientn(name="Proportion\ncontaminated", 
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
#g1

# Plot bar plots
g4 <- ggplot(nevents  %>% filter(species=="Razor clam"), aes(x=lat_dd, y=nevents)) +
  # Plot # of events
  facet_wrap(~species, ncol=1) +
  geom_area(fill="grey60") +
  # Add sampling lines
  geom_vline(data=site_key %>% filter(species=="Razor clam"), mapping=aes(xintercept=lat_dd), col="grey30") +
  # Flip vertical
  coord_flip() +
  # Labels
  labs(x="", y="# of events") +
  # Theme
  theme_bw() + base_theme +
  # Eliminate y-axis
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
#g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, 
                             g3, g4, ncol=2, widths=c(0.8,0.2), heights=c(0.75, 0.25))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig7_hovmoller_diagrams.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



