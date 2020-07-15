
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
outputdir <- "models/contamination/paper/output"
plotdir <- "models/contamination/paper/figures"

# Read data
dcrab <- brick(file.path(outputdir, "dungeness_crab_model_rf_cda_predictions_100fathoms.grd"))

landingsdir <- "data/cdfw/landings_public/processed"
dasurvdir1 <- "data/da_sampling/2019_request/raw"

# Read and format Dcrab ports
ports <- read.csv(file.path(landingsdir, "dungeness_ports.csv"), as.is=T)
da_ports <- filter(ports, da_sampling!="") %>% 
  mutate(da_sampling=recode_factor(da_sampling, 
                                   "routine"="Routine",
                                   "ad-hoc"="Ad-hoc"))

# Read and format crab sampling data
da_sites <- rio::import(file.path(dasurvdir1, "2014_crab_da_sampling_sites.xlsx")) %>% 
  mutate(long_dd=long_dd*-1)


# Build data
################################################################################

# Build data?
build_data <- F

# Build data or read it in
if(build_data){
  
  # Convert to dataframe
  dcrab_df <- as.data.frame(dcrab, xy=T) %>% 
    # Eliminate empty cells
    na.omit() %>% 
    # Cather wide-to-long
    gather(key="date", value="pcontam", 3:ncol(.)) %>% 
    # Rename columns
    rename(long_dd=x, lat_dd=y) %>% 
    # Format date
    mutate(date = gsub("X", "", date) %>%  ymd()) %>% 
    # Arrange columns
    select(date, everything())
  
  # Calculate date for Hovmöller diagram
  dcrab_stats <- dcrab_df %>% 
    group_by(date, lat_dd) %>% 
    summarize(n=n(),
              pcontam_avg=median(pcontam))
  
  # Export data
  save(dcrab_df, dcrab_stats, 
       file=file.path(outputdir, "dungeness_crab_model_rf_cda_predictions_100fathoms_hovmuller.Rdata"))
  
  # Read data   
}else{
  
  # Load data
  load(file.path(outputdir, "dungeness_crab_model_rf_cda_predictions_100fathoms_hovmuller.Rdata"))
  
}

# Number of contamination events at each latitude
nevents_lat <- dcrab_stats %>%
  group_by(lat_dd) %>% 
  summarize(nevents=sum(pcontam_avg>0.5)) %>% 
  ungroup()

# Sample data for fast plotting
dcrab_stats_sample <- dcrab_stats %>% 
  sample_frac(size=0.2)


# Plot data
################################################################################
 
# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=10),
                    plot.title=element_text(size=12),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=10),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank())

# Plotting params
ref_line_lwd <- 0.2
ref_line_color <- "grey30"

# Plot Hovmöller diagram
lats <- seq(32, 42, 2)
g1 <- ggplot(dcrab_stats, aes(x=date, y=lat_dd, fill=pcontam_avg)) +
  geom_tile() +
  # Add sampling site reference points
  geom_hline(yintercept=da_sites$lat_dd, linetype="solid", 
             color=ref_line_color, lwd=ref_line_lwd) +
  # Labels
  labs(x="Day", y="Latitude") +
  scale_y_continuous(breaks=lats, labels=paste0(lats, "°N")) +
  # Legend
  # scale_fill_gradientn(name="Proportion\ncontaminated", 
  #                      colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  scale_fill_gradientn(name="Proportion\ncontaminated", 
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  # Custom plot 1 theme
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  # Temporarily get rid of legend
  theme(legend.position="none")
g1

# Plot histogram of number contamination events
g2 <- ggplot(nevents_lat, aes(x=lat_dd, y=nevents)) +
  # Plot number of events
  geom_area(fill="grey60") +
  # Plot sampling sites
  geom_vline(xintercept=da_sites$lat_dd, linetype="solid", 
             color=ref_line_color, lwd=ref_line_lwd) +
  # Flip vertical
  coord_flip() +
  # Labels
  labs(x="", y="# of events") +
  # Theme
  theme_bw() +
  base_theme +
  # Custom plot 2 theme
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.8,0.2))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig7_hovmuller_diagram_dcrab.png"), 
       width=6.5, height=2.5, units="in", dpi=600)





