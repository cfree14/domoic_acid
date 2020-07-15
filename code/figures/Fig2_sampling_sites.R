

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)
library(lubridate)

# Directories
datadir <- "models/contamination/paper/data"
tabledir <- "models/contamination/paper/tables"
plotdir <- "models/contamination/paper/figures"

# Study species
study_species <- c("Dungeness crab",
                   "Rock crab",
                   "Spiny lobster",
                   "Razor clam",
                   "Sea mussel",
                   "Bay mussel",
                   "Pacific oyster")

# Read range data
ranges <- readRDS(file.path(datadir, "species_ranges.Rds")) %>% 
  rename(comm_name=species) %>% 
  mutate(comm_name=factor(comm_name, levels=study_species))

# Read sample data
data_orig <- readRDS(file.path(datadir, "CDPH_crab_bivalve_domoic_acid_data.Rds"))

# DA ppm limit
da_ppm_limit <- 200
  
# Format data
data <- data_orig %>% 
  filter(comm_name %in% study_species) %>% 
  mutate(da_ppm_cap=pmin(da_ppm, da_ppm_limit),
         comm_name = factor(comm_name, levels=study_species),
         type=recode_factor(type, "wild"="Wild", "cultured"="Cultured", "sentinel"="Sentinel"))


# Build site key
################################################################################

# Build site key
biv_sites <- data %>% 
  # Identify unique sites
  filter(type %in% c("Cultured", "Sentinel") | comm_name=="Razor clam") %>% 
  group_by(comm_name, port, area, lat_dd, long_dd) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Add general site location
  mutate(site=ifelse(grepl("Humboldt Bay", area), "Humboldt Bay", NA),
         site=ifelse(grepl("Tomales Bay", area), "Tomales Bay", site),
         site=ifelse(grepl("Morro Bay", area), "Morro Bay", site),
         site=ifelse(grepl("Agua Hedionda Lagoon", area), "Agua Hedionda Lagoon", site),
         site=ifelse(grepl("Santa Barbara Ch.", area), "Santa Barbara", site),
         site=ifelse(grepl("Long Beach, Fish Harbor", area), "Long Beach", site),
         site=ifelse(grepl("Crescent City", area), "Crescent City", site),
         site=ifelse(grepl("Clam Beach, McKinleyville", area), "Humboldt beaches", site),
         site=ifelse(grepl("Humboldt, Clam Beach", area), "Humboldt beaches", site),
         site=ifelse(grepl("Humboldt, Moonstone Beach", area), "Humboldt beaches", site),
         site=ifelse(grepl("Little River State Beach", area), "Humboldt beaches", site),
         site=ifelse(grepl("Bodega Harbor, Doran Beach", area), "Doran Beach", site),
         site=ifelse(grepl("Trinidad Head", area), "Humboldt Bay", site),
         site=ifelse(grepl("Drakes", area), "Drakes Bay", site)) %>%  
  # Fill in missing and recode
  mutate(site=ifelse(is.na(site), area, site),
         site=recode(site, 
                     "San Luis Obispo, Cal Poly Pier"="San Luis Obispo", 
                     "Santa Barbara, Stearns Wharf"="Santa Barbara", 
                     "Santa Cruz Pier"="Santa Cruz", 
                     "Bodega Harbor, USCG Dock"="Bodega Harbor")) %>% 
  # Summarize
  group_by(comm_name, site) %>% 
  summarize(lat_dd=mean(lat_dd),
            long_dd=mean(long_dd))

# Export bivalve sites
write.csv(biv_sites, file=file.path(datadir, "bivalve_sampling_sites.csv"), row.names=F)

# Sample size key
n_key <- data %>% 
  group_by(comm_name) %>% 
  summarize(n=n(),
            n_label=paste0("n=", n))

# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  plot.title=element_blank(),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.position="bottom", 
                  legend.title=element_text(size=8),
                  legend.text=element_text(size=8))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot ranges
  geom_sf(data=ranges, fill="blue", col=NA) +
  # Plot CA/Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Plot samples
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, color=type), size=1) +
  facet_wrap(~comm_name, ncol=4) +
  # Label sites
  geom_text(data=biv_sites, mapping=aes(x=long_dd, y=lat_dd, label=site), hjust=-0.1, size=1.7) +
  # Add sample size
  geom_text(data=n_key, mapping=aes(label=n_label), x=-116, y=41.5, hjust=1, size=3) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Legend
  scale_color_discrete(name="Sample source") +
  # Labels
  labs(x="", y="") +
  theme_bw() + my_theme +
  theme(legend.position = c(0.85, 0.35))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_sample_maps.png"), 
       width=6.5, height=5, units="in", dpi=600)



