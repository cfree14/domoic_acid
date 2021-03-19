
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
inputdir <- "input"
preddir <- "output/model_preds"
hovmollerdir <- "output/contam_events"
plotdir <- "figures"

# Study species
study_species <- c("Dungeness crab", "Rock crab", 
                  "Spiny lobster", "Razor clam")


# Build season lines
################################################################################

# Function to build season key
# species <- "Dungeness crab"; fishery_type <- "Commercial"; region <- "Northern"; open_date <- "12-01"; close_date <- "07-15"
build_season_key <- function(species, fishery_type, region, open_date, close_date){
  dates_open <- paste(2013:2019, open_date, sep="-") %>% ymd()
  dates_close <- paste0(2014:2020, close_date, sep="-")  %>% ymd()
  season_key <- tibble(species=species,
                        fishery_type=fishery_type,
                        region=region, 
                        open_date=dates_open, 
                        close_date=dates_close) %>% 
    mutate(line_group=1:n()) %>% 
    select(species:region, line_group, everything()) %>% 
    gather(key="endpoint", value="date", 5:ncol(.)) %>% 
    arrange(species, fishery_type, region, line_group)
  return(season_key)
}

# Dungeness crab season keys
dcrab_comm_n_key <- build_season_key(species="Dungeness crab", fishery_type="Commercial", 
                                     region="Northern", open_date="12-01", close_date="07-15")
dcrab_comm_c_key <- build_season_key(species="Dungeness crab", fishery_type="Commercial", 
                                     region="Central", open_date="11-15", close_date="06-30")
dcrab_rec_n_key <- build_season_key(species="Dungeness crab", fishery_type="Recreational", 
                                     region="Northern", open_date="11-01", close_date="07-30")
dcrab_rec_c_key <- build_season_key(species="Dungeness crab", fishery_type="Recreational", 
                                     region="Central", open_date="11-01", close_date="06-30")

# Lobster season keys
lobster_comm_key <- build_season_key(species="Spiny lobster", fishery_type="Commercial", 
                                    region="All", open_date="10-01", close_date="03-15")
lobster_rec_key <- build_season_key(species="Spiny lobster", fishery_type="Recreational", 
                                    region="All", open_date="10-01", close_date="03-15")

# Season key
season_key <- bind_rows(dcrab_comm_n_key, dcrab_comm_c_key,
                        dcrab_rec_n_key, dcrab_rec_c_key,
                        lobster_comm_key, lobster_rec_key) %>% 
  # Add latitudes to plot at
  mutate(lat_plot=31.5,
         lat_plot=ifelse(fishery_type=="Commercial", lat_plot+0.3, lat_plot),
         lat_plot=ifelse(region=="Central", lat_plot-0.15, lat_plot)) %>% 
  # Make new line group id (unique)
  mutate(line_group=paste(species, fishery_type, region, line_group), sep="-")



# Build sampling sites reference file
################################################################################

# Read sampling data
samples <- readRDS(file.path(inputdir, "CDPH_crab_bivalve_domoic_acid_data.Rds")) %>% 
  # Get/rename columns
  select(comm_name, date, lat_dd, da_ppm) %>% 
  rename(species=comm_name) %>% 
  # Reduce to study species and factor
  filter(species %in% study_species) %>% 
  mutate(species=factor(species, study_species)) %>% 
  # Get rid of 1900 values
  filter(year(date)>=2014)
  
# Read and format Dungeness crab ports
dcrab_sites <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/cdfw/landings_public/processed/dungeness_ports.csv", as.is=T) %>% 
  filter(da_sampling=="routine") %>% 
  select(port, lat_dd) %>% 
  rename(site_name=port) %>% 
  mutate(species="Dungeness crab") %>% 
  select(species, everything())

# Read and format razor clam sites
rclam_sites <- read.csv("input/bivalve_sampling_sites.csv", as.is=T) %>% 
  filter(comm_name=="Razor clam") %>% 
  select(comm_name, site, lat_dd) %>% 
  rename(site_name=site, species=comm_name)
  
# Build site key
site_key <- bind_rows(dcrab_sites, rclam_sites)


# Build data
################################################################################

# Model
model_key <- tibble(species = study_species,
                    model = c("dungeness_crab_model_rf_cda.Rds",
                              "rock_crab_model_brt_cda.Rds",
                              "spiny_lobster_model_rf_cda.Rds",
                              "razor_clam_model_rf_cda.Rds"))




# Number of contamination events at each latitude
nevents <- data %>%
  group_by(species, lat_dd) %>%
  summarize(nevents=sum(pcontam > 0.5)) %>%
  ungroup()


# Plotting functions
################################################################################

# For testing
species <- "Dungeness crab"
date <- "2016-04-01"

# Function to plot map
plot_map <- function(species, date){
  
  # Build data
  ######################
  
  # Read predictions
  species_do <- species
  model_do <- model_key %>% filter(species==species_do) %>% pull(model) %>% gsub(".Rds", "", .)
  infile <- paste0(model_do, "_predictions_range_mask.grd")
  preds <- brick(file.path(preddir, infile))
  
  # Format predictions
  date_layer <- gsub("-", ".", date) %>% paste0("X", .)
  pred_ras <- preds[[date_layer]]
  pred_df <- pred_ras %>% 
    as.data.frame(xy=T) %>% 
    setNames(c("long_dd", "lat_dd", "pcontam")) %>% 
    filter(!is.na(pcontam))
  
  
  # Build plot
  ######################
  
  # Get US states and Mexico
  usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
  mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
  
  # Plot map
  g1 <- ggplot() +
    # Plot p(contamination)
    geom_tile(data=pred_df, mapping=aes(x=long_dd, y=lat_dd, fill=pcontam)) +
    # Add California and Mexico
    geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
    geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
    # Crop extent
    coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
    # Label species
    annotate("text", x=-125, y=42, label=species_do, hjust=0, fontface="bold", size=2.5) +
    # Labels
    labs(x=" ", y="") +
    # Legend
    scale_fill_gradientn(name="p(contaminated)", 
                         colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", 
                                 barwidth = 0.5, barheight = 4)) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=c(0.2, 0.3),
          legend.title=element_blank(),
          legend.background = element_rect(fill=alpha('blue', 0)),
          axis.text.x=element_text(size=5))
  
  g1
  
}

# Plot raster
plot_raster <- function(species, frac=1){
  
  # Read data
  species_do <- species
  model_do <- model_key %>% filter(species==species_do) %>% pull(model) %>% gsub(".Rds", "", .)
  infile <- paste0(model_do, "_predictions_range_mask_hovmoller_imputed_events.Rdata")
  load(file=file.path(hovmollerdir, infile))
  
  # Sample data if necessary
  data_hov_imputed_use <- sample_frac(data_hov_imputed, size=frac)
  
  # Plot data
  g <- ggplot(data_hov_imputed_use, aes(x=date, y=lat_dd, fill=pcontam_avg)) +
    # Plot raster
    geom_tile() +
    # # Add sampling lines
    # geom_hline(data=site_key %>% filter(species==species_do), mapping=aes(yintercept=lat_dd), col="grey30") +
    # # Add sampling points
    # geom_point(samples %>% filter(species==species_do), mapping=aes(x=date, y=lat_dd), inherit.aes = F, size=0.8, pch=1, alpha=0.3) +
    # Plot season lines
    geom_line(season_key %>% filter(species==species_do), inherit.aes = F,
              mapping=aes(x=date, y=lat_plot, group=line_group, color=fishery_type), lwd=0.3) +
    # Labels
    labs(x="", y="") +
    scale_y_continuous(breaks=seq(32, 42, 2), lim=c(32,42), labels = paste0(seq(32, 42, 2), "°N")) +
    scale_x_date(lim=c(ymd("2014-01-01"), NA), date_breaks = "1 year", date_labels="%Y") +
    # Legend
    scale_fill_gradientn(name="Proportion\ncontaminated", 
                         colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    scale_color_manual(guide=F, values=c("black", "grey40")) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="none")
  g
  
  # Return
  return(g)
  
}

# Plot number of events
plot_nevents <- function(species){
  
  # Read data
  species_do <- species
  model_do <- model_key %>% filter(species==species_do) %>% pull(model) %>% gsub(".Rds", "", .)
  infile <- paste0(model_do, "_predictions_range_mask_hovmoller_imputed_events.Rdata")
  load(file=file.path(hovmollerdir, infile))
  
  # Build data
  nevents <- data_hov_imputed %>% 
    group_by(lat_dd) %>% 
    summarize(nevents=sum(pcontam_avg>=0.5))
  
  # Plot events
  g <- ggplot(nevents, aes(x=lat_dd, y=nevents)) +
    # Plot # of events
    geom_area(fill="grey60") +
    # Add sampling lines
    # geom_vline(data=site_key %>% filter(species==species_do), mapping=aes(xintercept=lat_dd), col="grey30") +
    # Flip vertical
    coord_flip() +
    # Labels
    labs(x="", y="") +
    scale_x_continuous(breaks=seq(32, 42, 2), lim=c(32,42), labels = paste0(seq(32, 42, 2), "°N")) +
    # Theme
    theme_bw() + base_theme
  g
  
}

# Plot data
################################################################################

# Sample data for fast plotting
# data_sample <- data %>% 
#   sample_frac(size=0.01)

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_blank(),
                    plot.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.text.y = element_text(angle = 90, hjust = 0.5))

# Plot maps
date_do <- "2016-04-01"
m1 <- plot_map(species="Dungeness crab", date=date_do)
m2 <- plot_map(species="Rock crab", date=date_do)
m3 <- plot_map(species="Spiny lobster", date=date_do)
m4 <- plot_map(species="Razor clam", date=date_do)

# Plot rasters
r1 <- plot_raster(species="Dungeness crab") #, frac=0.01)
r2 <- plot_raster(species="Rock crab") #, frac=0.01)
r3 <- plot_raster(species="Spiny lobster") #, frac=0.01)
r4 <- plot_raster(species="Razor clam") #, frac=0.01)

# Plot event histograms
e1 <- plot_nevents(species="Dungeness crab")
e2 <- plot_nevents(species="Rock crab")
e3 <- plot_nevents(species="Spiny lobster")
e4 <- plot_nevents(species="Razor clam")

# Merge plots
g <- grid.arrange(m1, r1, e1, 
                  m2, r2, e2, 
                  m3, r3, e3, 
                  m4, r4, e4, 
                  ncol=3, widths=c(0.25, 0.55, 0.20))
#g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_hovmoller_sampling_program_no_lines.png"), 
       width=6.5, height=7.5, units="in", dpi=600)



