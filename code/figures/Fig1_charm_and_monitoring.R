
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(cowplot)

# Plotting directory
plotdir <- "models/contamination/paper/figures"

# Data directories
landingsdir <- "data/cdfw/landings_public/processed"
charmdir <- "data/charm/processed"
pierdir <- "data/pier_sampling/processed"
gisdir <- "data/cdfw/gis_data/processed"
dasurvdir <- "data/da_sampling/2019_request/processed"
dasurvdir1 <- "data/da_sampling/2019_request/raw"

# Date to plot
date <- "2015-07-01"
date1 <- gsub("-", ".", paste0("X", date))


# Read data
################################################################################

# Read C-HARM data
pn_brick <- brick(file.path(charmdir, "CHARM_PN_20140305_to_present.grd"))
dap_brick <- brick(file.path(charmdir, "CHARM_DAP_20140305_to_present.grd"))
dac_brick <- brick(file.path(charmdir, "CHARM_DAC_20140305_to_present.grd"))

# Extract one date from C-HARM data
pn_df <- pn_brick[[date1]] %>% 
  raster::as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", "prob"))
dap_df <- dap_brick[[date1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", "prob"))
dac_df <- dac_brick[[date1]] %>% 
  as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", "prob"))

# Read and format pier data
piers <- read.csv(file.path(pierdir, "pier_sampling_locations.csv"), as.is=T) %>% 
  mutate(type=recode_factor(type,
                            "continuous"="Quantitative",
                            "percent composition"="Qualitative"))
q_piers <- piers %>% 
  filter(type=="Quantitative") %>% 
  mutate(label_name=plyr::revalue(long_name, c("La Jolla, Scripp's Pier"="Scripps Pier",
                                               "Fisherman's Wharf, Monterey"="Monterey Wharf")))

# Read and format ports data
ports <- read.csv(file.path(landingsdir, "dungeness_ports.csv"), as.is=T)
da_ports <- filter(ports, da_sampling!="") %>% 
  mutate(da_sampling=recode_factor(da_sampling, 
                                   "routine"="Routine",
                                   "ad-hoc"="Ad-hoc"))

# Read and format crab sampling data
da_crabs <- read.csv(file.path(dasurvdir, "CDPH_crab_viscera_da_data.csv"), as.is=T)
da_sites <- import(file.path(dasurvdir1, "2014_crab_da_sampling_sites.xlsx")) %>% 
  mutate(long_dd=long_dd*-1)

# Read bivalve sampling data
da_bivs <- read.csv(file.path("models/contamination/paper/data/bivalve_sampling_sites.csv"), as.is=T) %>% 
  mutate(comm_name=recode(comm_name, 
                          "Pacific oyster"="PO",
                          "Sea mussel"="SM",
                          "Bay mussel"="BM",
                          "Razor clam"="RC"))

# Plot data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  plot.title=element_text(size=8),
                  legend.text=element_text(size=5),
                  legend.title=element_text(size=7),
                  legend.position = "bottom",
                  legend.spacing.y = unit(0.05, "cm"),
                  legend.box = "vertical",
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# A. Pseudo-nitzschia
#############################

# Plot Pseudo-nitzschia
g1 <- ggplot() +
  # Plot p(Pseudo-nitzschia) raster
  geom_raster(pn_df, mapping=aes(x=long_dd, y=lat_dd, fill=prob)) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Add piers and legend
  geom_point(data=arrange(piers, desc(type)), aes(x=long_dd, y=lat_dd, col=type), size=0.8) +
  # ggrepel::geom_text_repel(data=q_piers, aes(x=long_dd, y=lat_dd, label=label_name),
  #                          size=1.6, direction = "y", hjust = 0, nudge_x=0.10, segment.size=0.2) +
  geom_text(data=q_piers, aes(x=long_dd, y=lat_dd, label=label_name),
            size=1.6, hjust=c(-0.13, -0.13, -0.13, 1.13, -0.13,-0.13, -0.13, -0.13)) +
  scale_color_manual(name="Monitoring type", values=c("black", "grey40")) +
  # Raster legend
  scale_fill_gradientn(#name="p(PN ≥10^4 cells/ml)",
                       name=expression("p(PN ≥10"^"4"*" cells/ml)"),
                       colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                       limits=c(0,1),
                       na.value=NA) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, ticks.colour = "black", frame.colour = "black"),
         color = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="", title="A. Pseudo-nitzschia (PN) risk\nand monitoring program") +
  theme_bw() + my_theme
g1


# B. Particulate domoic acid
#############################

# Plot particulate domoic acid
g2 <- ggplot() +
  # Plot p(pDA) raster
  geom_raster(dap_df, mapping=aes(x=long_dd, y=lat_dd, fill=prob)) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Add sampling areas and legend
  geom_point(data=da_sites, aes(x=long_dd, y=lat_dd), size=0.5, shape=4) +
  geom_point(data=da_ports, aes(x=long_dd, y=lat_dd, col=da_sampling), size=0.8) +
  scale_color_manual(name="Monitoring type", values=c("black", "grey40")) +
  # ggrepel::geom_text_repel(data=filter(da_ports, da_sampling=="routine"), aes(x=long_dd, y=lat_dd, label=port),
  #                          size=1.6, direction = "y", hjust = 0, nudge_x=0.10, segment.size=0.2) +
  geom_text(data=filter(da_ports, da_sampling=="Routine"), aes(x=long_dd, y=lat_dd, label=port),
            size=1.6, hjust=-0.13) +
  # Raster legend
  scale_fill_gradientn(name="p(pDA ≥500 ng/l)",
                       colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                       limits=c(0,1),
                       na.value=NA) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, ticks.colour = "black", frame.colour = "black"),
         color = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="", title="B. Particulate domoic acid (pDA) risk\nand the crab sampling program") +
  theme_bw() + my_theme
g2

# C. Cellular domoic acid
#############################

# Plot cellular domoic acid
g3 <- ggplot() +
  # Plot p(cDA) raster and legend
  geom_raster(dac_df, mapping=aes(x=long_dd, y=lat_dd, fill=prob)) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Add sampling areas and legend
  geom_point(data=da_bivs, aes(x=long_dd, y=lat_dd, color=comm_name), size=0.8) +
  scale_color_discrete(name="Species") +
  # Raster legend
  scale_fill_gradientn(name="p(cDA ≥10 pg/cell)",
                       colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                       limits=c(0,1),
                       na.value=NA) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, ticks.colour = "black", frame.colour = "black"),
         color = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="", title="C. Cellular domoic acid (cDA) risk\nand the bivalve monitoring program") +
  theme_bw() + my_theme
g3


# Merge plots and export
################################################################################

# Merge
g <- plot_grid(g1, g2, g3, ncol=3)

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_charm_monitoring_maps.png"), width=6.5, height=4.5, units="in", dpi=600)



