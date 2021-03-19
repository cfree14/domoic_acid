
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "outreach/cordell_bank_nms"
datadir <- "data/closures/data"

# Read data
closures <- readRDS(file.path(datadir, "CDFW_2015_2020_fishery_closures.Rds"))
advisories <- readRDS(file.path(datadir, "CDPH_2014_2020_health_advisories.Rds"))


# Format data
################################################################################

# Format advisories
################################

# Add factor
advisories <- advisories %>% 
  # Reduce to Dungeness crab
  filter(comm_name=="Dungeness crab") %>% 
  # Reduce to lat 
  filter(lat_dd>=37 & lat_dd <=39) %>% 
  # Add factors
  mutate(advisory=recode_factor(advisory, 
                                "Out-of-season"="Out-of-Season",
                                "None"="No advisory",
                                "Partial"="Viscera advisory",
                                "Full"="Meat advisory"))

# Sample
advisories_sample <- sample_frac(advisories, size=0.1)

# Format closures
################################

# Add factor
closures <- closures %>% 
  # Reduce to Dungeness crab
  filter(comm_name=="Dungeness crab" & fishery=="Commercial") %>% 
  # Reduce to lat 
  filter(lat_dd>=37 & lat_dd <=39) 


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=5),
                  legend.title=element_text(size=7),
                  strip.text = element_text(size=8),
                  plot.title=element_text(size=8), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.y = element_text(angle = 90, hjust = 0.5))
  
# Plot closure grid
g <- ggplot(advisories, aes(x=date, y=lat_dd, fill=advisory)) +
  # Plot raster
  geom_raster() +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  # Labels
  labs(x="", y="Latitude (°N)", title="Dungeness crab health advisories") +
  # Legends
  scale_fill_manual(name="Advisory status", values=c("white", "grey70", "coral", "darkred")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="right")
g

# Export
ggsave(g, filename=file.path(plotdir, "CBNMS_dcrab_health_advisories.png"), 
       width=6.5, height=3, units="in", dpi=600)



# Plot closure grid
g <- ggplot(closures, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  # Labels
  labs(x="", y="Latitude (°N)", title="Dungeness crab commercial fishery closures") +
  # Legends
  scale_fill_manual(name="Season status", values=c("grey70", "white", "salmon", "darkred", "navy")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="right")
g

# Export
ggsave(g, filename=file.path(plotdir, "CBNMS_dcrab_fishery_closures.png"), 
       width=6.5, height=3, units="in", dpi=600)



