
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

# Internal directories
plotdir <- "figures"
datadir <- "data/charm/processed"

# External directories
gisdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/cdfw/gis_data/raw/StateWaterJurisdiction/"
bathydir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/bathymetry/processed"

# 100 fathoms averages
################################################################################

# Build data
build <- F
if(build==TRUE){
  
  # Read data
  pn_brick <- raster::brick(file.path(datadir, "CHARM_PN_20140305_to_present_imputed.grd"))
  dap_brick <- raster::brick(file.path(datadir, "CHARM_DAP_20140305_to_present_imputed.grd"))
  dac_brick <- raster::brick(file.path(datadir, "CHARM_DAC_20140305_to_present_imputed.grd"))
  
  # Read 100 fathoms polygon
  bathy100 <- readRDS(file.path(bathydir, "CA_100_fathoms_polgyon_crude.Rds"))
  
  # Calculate means inside 100 fathoms (fishing grounds)
  pn_avg_ca <- raster::extract(x=pn_brick, y=bathy100, method="simple", fun=mean, na.rm=T)
  dap_avg_ca <- raster::extract(x=dap_brick, y=bathy100, method="simple", fun=mean, na.rm=T)
  dac_avg_ca <- raster::extract(x=dac_brick, y=bathy100, method="simple", fun=mean, na.rm=T)
  
  # Calculate standard deviations inside 100 fathoms (fishing grounds)
  pn_sd_ca <- raster::extract(x=pn_brick, y=bathy100, method="simple", fun=sd, na.rm=T)
  dap_sd_ca <- raster::extract(x=dap_brick, y=bathy100, method="simple", fun=sd, na.rm=T)
  dac_sd_ca <- raster::extract(x=dac_brick, y=bathy100, method="simple", fun=sd, na.rm=T)
  
  # FMerge means / sds
  pn_ca_df <- tibble(variable="Psuedo-nitzschia",
                     date=colnames(pn_avg_ca) %>%  gsub("X", "", .) %>% ymd(),
                     risk_avg=pn_avg_ca %>% as.numeric(),
                     risk_sd=pn_sd_ca %>% as.numeric())
  dap_ca_df <- tibble(variable="Particulate domoic acid",
                      date=colnames(dap_avg_ca) %>%  gsub("X", "", .) %>% ymd(),
                      risk_avg=dap_avg_ca %>% as.numeric(),
                      risk_sd=dap_sd_ca %>% as.numeric())
  dac_ca_df <- tibble(variable="Cellular domoic acid",
                      date=colnames(dac_avg_ca) %>%  gsub("X", "", .) %>% ymd(),
                      risk_avg=dac_avg_ca %>% as.numeric(),
                      risk_sd=dac_sd_ca %>% as.numeric())
  
  # Merge variables
  data <- bind_rows(pn_ca_df, 
                    dap_ca_df,
                    dac_ca_df) %>% 
    mutate(variable=factor(variable, levels=c("Psuedo-nitzschia", "Particulate domoic acid", "Cellular domoic acid")))
  
  # Export data
  
  
}



# Plot data
################################################################################
  
# Build Dungeness season df
open_dates <- c("2014-01-01", paste0(2014:2019, "-11-01")) %>% ymd()
close_dates <- c("2014-07-15", paste0(2015:2020, "-07-15")) %>% ymd()
dcrab_season <- tibble(open=open_dates,
                       close=close_dates)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  axis.title.x = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.position = "bottom")
  
# Plot data
g <- ggplot(data, aes(x=date, y=risk_avg)) +
  # Add seasons
  geom_rect(data=dcrab_season, inherit.aes=F,
            mapping=aes(xmin=open, xmax=close), ymin=0, ymax=1, fill="grey90") +
  # Add lines
  facet_wrap(~variable, ncol=1) +
  geom_ribbon(aes(ymin=risk_avg-risk_sd, ymax=risk_avg+risk_sd), fill="grey60", alpha=0.9) +
  geom_line(lwd=0.2) +
  # Axes
  ylim(0,1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # Labels
  labs(x="", y="Mean daily risk") +
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS2_charm_average_fishing_grounds.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

