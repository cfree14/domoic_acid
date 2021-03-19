
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "data/closures/figures"
datadir <- "data/closures/data"

# Read data
closures <- readRDS(file.path(datadir, "CDFW_2015_2020_fishery_closures.Rds"))
advisories <- readRDS(file.path(datadir, "CDPH_2014_2020_health_advisories.Rds"))

# Read county lines
counties <- readxl::read_excel(file.path(datadir, "CA_county_lines.xlsx"))


# Plot all advisories
################################################################################

# Study species
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", "Mussels", "Clams", "Scallops", "Northern anchovy", "Pacific sardine")

# Add factor
advisories <- advisories %>% 
  mutate(comm_name=factor(comm_name, levels=spp_do), 
         advisory=recode_factor(advisory, 
                                "Out-of-season"="Out-of-Season",
                                "None"="No advisory",
                                "Partial"="Viscera advisory",
                                "Full"="Meat advisory"))

# Sample
advisories_sample <- sample_frac(advisories, size=0.1)

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  legend.position = "bottom",
                  legend.title = element_blank())
  
# Plot closure grid
g <- ggplot(advisories, aes(x=date, y=lat_dd, fill=advisory)) +
  # Facet by species
  facet_wrap(~comm_name, ncol=2, scales="free_x") +
  # Plot raster
  geom_raster() +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=33:42) +
  # Labels
  labs(x="", y="Latitude (°N)", title="CDPH public healthy advisories") +
  # Legends
  scale_fill_manual(name="", values=c("grey80", "grey50", "coral", "darkred")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "CDPH_health_advisories_all_species.png"), 
       width=8.5, height=11.5, units="in", dpi=600)


# Loop through species and plot individually
for(i in 1:length(spp_do)){
  
  # Filter data
  spp <- spp_do[i]
  sdata <- advisories %>% 
    filter(comm_name==spp) 
  
  # Plot closure grid
  g <- ggplot(sdata, aes(x=date, y=lat_dd, fill=advisory)) +
    # Plot raster
    geom_raster() +
    # Axis
    scale_x_date(date_breaks="1 year", date_labels = "%Y") +
    scale_y_continuous(breaks=33:42) +
    # Labels
    labs(x="", y="Latitude (°N)", title=paste("CDPH public healthy advisories:", spp)) +
    # Legends
    scale_fill_manual(name="", values=c("grey80", "grey50", "coral", "darkred")) +
    # Theme
    theme_bw() + my_theme
  g
  
  # Export
  outfig <- paste0("CDPH_health_advisories_all_", tolower(spp) %>% gsub(" ", "_", .), ".png")
  ggsave(g, filename=file.path(plotdir,   outfig ), 
         width=6.5, height=4, units="in", dpi=600)
  
}

# Plot all closures
################################################################################

# Sample
closures_sample <- sample_frac(closures, size=0.1)

# Plot closure grid
g <- ggplot(closures, aes(x=date, y=lat_dd, fill=status)) +
  # Facet
  facet_grid(fishery ~ comm_name) +
  # Plot raster
  geom_raster() +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=33:42) +
  # Labels
  labs(x="", y="Latitude (°N)", title=paste("CDFW fishery closures")) +
  # Legends
  scale_fill_manual(name="", values=c("grey50", "grey80", "pink", "darkred", "navy")) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.text=element_text(size=6))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "CDFW_fishery_closures_all.png"), 
       width=6.5, height=5.5, units="in", dpi=600)









