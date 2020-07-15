

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
plotdir <- "models/contamination/paper/figures/for_presentations"

# Read masked predictions
preds <- brick(file.path(outputdir, "dungeness_crab_model_rf_cda_predictions_range_mask.grd"))

# Read Hovmoller data for masked predictions
load(file.path(outputdir, "dungeness_crab_model_rf_cda_predictions_range_mask_hovmoller.Rdata"))
hov_data <- data_stats
rm(data_stats, data_df)
hov_data_sample <- sample_frac(hov_data, 0.2)

# Read sampling site info
landingsdir <- "data/cdfw/landings_public/processed"
ports <- read.csv(file.path(landingsdir, "dungeness_ports.csv"), as.is=T)
da_ports <- filter(ports, da_sampling!="") %>% 
  mutate(da_sampling=recode_factor(da_sampling, 
                                   "routine"="Routine",
                                   "ad-hoc"="Ad-hoc"))

# Read and format crab sampling data
dasurvdir1 <- "data/da_sampling/2019_request/raw"
da_sites <- rio::import(file.path(dasurvdir1, "2014_crab_da_sampling_sites.xlsx")) %>% 
  mutate(long_dd=long_dd*-1)

# Read sampling data
samples <- readRDS(file.path(datadir, "CDPH_crab_bivalve_domoic_acid_data.Rds")) %>% 
  # Important columns
  select(comm_name, date, lat_dd, da_ppm) %>%
  # Get rid of 1900 values
  filter(year(date)>=2014 & comm_name=="Dungeness crab")

# Build season key
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

# Season key
season_key <- bind_rows(dcrab_comm_n_key, dcrab_comm_c_key,
                        dcrab_rec_n_key, dcrab_rec_c_key) %>% 
  # Add latitudes to plot at
  mutate(lat_plot=34.2,
         lat_plot=ifelse(fishery_type=="Commercial", lat_plot+0.18, lat_plot),
         lat_plot=ifelse(region=="Central", lat_plot-0.09, lat_plot)) %>% 
  # Make new line group id (unique)
  mutate(line_group=paste(species, fishery_type, region, line_group), sep="-")


# Plot data
################################################################################

# A. Map
# B. Hovmoller
# C. Number of events

# Theme
########################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    plot.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank())

# Latitudes
lats  <- seq(34,42,2)
lat_labels <- paste0(lats, "°N")

# A. Map
########################################

# Raster
date <- "2016-04-01"
date_layer <- gsub("-", ".", date) %>% paste0("X", .)
pred_ras <- preds[[date_layer]]
pred_df <- pred_ras %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", "pcontam")) %>% 
  filter(!is.na(pcontam))

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
  # Add sampling areas
  geom_point(data=da_sites, aes(x=long_dd, y=lat_dd), size=0.5, shape=4) +
  geom_point(data=da_ports %>% filter(da_sampling=="Routine"), aes(x=long_dd, y=lat_dd), size=0.8, color="black") +
  geom_text(data=filter(da_ports, da_sampling=="Routine"), aes(x=long_dd, y=lat_dd, label=port),
            size=2, hjust=-0.13) +
  # Crop extent
  coord_sf(xlim = c(-125, -120), ylim = c(34, 42)) +
  # Labels
  labs(x=" ", y="") +
  # Legend
  scale_fill_gradientn(name="p(contaminated)", 
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu")), 
                       # colors=RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       lim=c(0, max(hov_data$pcontam_avg))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position=c(0.3,0.2),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y = element_blank())
g1


# B. Hovmoller diagram
########################################

# Plot Hovmoller diagram
g2 <- ggplot(hov_data, aes(x=date, y=lat_dd, fill=pcontam_avg)) +
  # Plot raster
  geom_tile() +
  # Plot sample lines
  geom_hline(data=da_sites, mapping=aes(yintercept=lat_dd), color="black", lwd=0.25) +
  # Plot sample points
  geom_point(samples, mapping=aes(x=date, y=lat_dd), inherit.aes = F, size=0.8, pch=1, alpha=0.3) +
  # Plot season lines
  geom_line(season_key, inherit.aes = F,
            mapping=aes(x=date, y=lat_plot, group=line_group, color=fishery_type), lwd=0.3) +
  # Labels
  labs(x=" ", y="") +
  # Y-extent and labels
  scale_x_date(breaks=seq(ymd("2014-01-01"), ymd("2020-01-01"), by="year"), labels=2014:2020) +
  scale_y_continuous(breaks=lats, labels=lat_labels, lim=c(34,42)) +
  # Legends
  scale_color_manual(guide=F, values=c("black", "grey40")) +
  scale_fill_gradientn(name="p(contaminated)", 
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu")), 
                       # colors=RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       lim=c(0, max(hov_data$pcontam_avg))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y = element_blank())
g2


# C. Number of events
########################################

# Number of contamination events at each latitude
nevents <- hov_data %>%
  group_by(lat_dd) %>% 
  summarize(nevents=sum(pcontam_avg > 0.5)) %>% 
  ungroup()

# Plot number of events
g3 <- ggplot(nevents, aes(x=lat_dd, y=nevents)) +
  # Plot # of events
  geom_area(fill="grey60") +
  # Plot sample lines
  geom_vline(data=da_sites, mapping=aes(xintercept=lat_dd), color="black", lwd=0.25) +
  # Flip vertical
  coord_flip() +
  # Labels
  labs(x="", y="# of events") +
  # Y-extent and labels
  scale_x_continuous(breaks=lats, labels=lat_labels, lim=c(34,42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y = element_blank())
g3


# Merge and export
########################################

# Merge
g <- grid.arrange(g1, g2, g3, ncol=3, widths=c(0.25, 0.55, 0.20))
g

# Export 
ggsave(g, filename=file.path(plotdir, "figure_dcrab_contam_pred.png"), 
       width=10.5, height=4.5, units="in", dpi=600)



