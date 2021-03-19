
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
hovmollerdir <- "output/hovmoller"
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

# Build raster data
##################################

# Models
models <- c("dungeness_crab_model_rf_cda.Rds",
            "rock_crab_model_brt_cda.Rds",
            "spiny_lobster_model_rf_cda.Rds",
            "razor_clam_model_rf_cda.Rds")

# Loop through models
x <- models[1]
data_orig <- purrr::map_df(models, function(x) {
  
  # Model
  model_do <- gsub(".Rds", "", x)
  infile <- paste0(model_do, "_predictions_range_mask_hovmoller.Rdata")
  
  # Read model
  load(file=file.path(hovmollerdir, infile))
  
  # Format data
  spp <- strsplit(infile, split="_model")[[1]][1] %>% gsub("_", " ", .) %>% stringr::str_to_sentence()
  if(exists("dcrab_df")){data_df <- dcrab_df} # delete this later when you fixed predict code
  df <- data_df %>% 
    mutate(species=spp) %>% 
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
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    plot.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank())


# Plot raster
species <- "Dungeness crab"
plot_raster <- function(species){
  
  # Plot rasters
  spp_do <- species
  g <- ggplot(data %>% filter(species==spp_do), aes(x=date, y=lat_dd, fill=pcontam)) +
    # Plot raster
    geom_tile() +
    # Add sampling lines
    geom_hline(data=site_key %>% filter(species==spp_do), mapping=aes(yintercept=lat_dd), col="grey30") +
    # Add sampling points
    geom_point(samples %>% filter(species==spp_do), mapping=aes(x=date, y=lat_dd), inherit.aes = F, size=0.8, pch=1, alpha=0.3) +
    # Plot season lines
    geom_line(season_key %>% filter(species==spp_do), inherit.aes = F,
              mapping=aes(x=date, y=lat_plot, group=line_group, color=fishery_type), lwd=0.3) +
    # Labels
    labs(x="", y="Latitude (°N)", title=spp_do) +
    scale_y_continuous(breaks=seq(32, 42, 2), lim=c(31,43)) +
    scale_x_date(lim=c(ymd("2014-01-01"), NA)) +
    # Legend
    scale_fill_gradientn(name="Proportion\ncontaminated", 
                         colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    scale_color_manual(guide=F, values=c("black", "grey40")) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="left")
  g
  
  # Return
  return(g)
  
}


# Plot number of events
plot_nevents <- function(species){
  
  # Plot events
  spp_do <- species
  g <- ggplot(nevents  %>% filter(species==spp_do), aes(x=lat_dd, y=nevents)) +
    # Plot # of events
    geom_area(fill="grey60") +
    # Add sampling lines
    geom_vline(data=site_key %>% filter(species==spp_do), mapping=aes(xintercept=lat_dd), col="grey30") +
    # Flip vertical
    coord_flip() +
    # Labels
    labs(x="", y="# of events", title="") +
    scale_x_continuous(breaks=seq(32, 42, 2), lim=c(31,43)) +
    # Theme
    theme_bw() + base_theme +
    # Eliminate y-axis
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank())
  g
  
}

# Plot rasters
r1 <- plot_raster(species="Dungeness crab")
r2 <- plot_raster(species="Rock crab")
r3 <- plot_raster(species="Spiny lobster")
r4 <- plot_raster(species="Razor clam")


# Plot event histograms
e1 <- plot_nevents(species="Dungeness crab")
e2 <- plot_nevents(species="Rock crab")
e3 <- plot_nevents(species="Spiny lobster")
e4 <- plot_nevents(species="Razor clam")


# Merge plots
g <- gridExtra::grid.arrange(r1, e1, 
                             r2, e2, 
                             r3, e3, 
                             r4, e4, 
                             ncol=2, widths=c(0.8,0.2))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_hovmoller_diagrams.png"), 
       width=6.5, height=7.5, units="in", dpi=600)



