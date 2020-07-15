
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "models/contamination/paper/figures"
datadir <- "data/charm/processed"
gisdir <- "data/cdfw/gis_data/raw/StateWaterJurisdiction/"
bathydir <- "data/bathymetry/processed"

# Read data
pn_brick <- raster::brick(file.path(datadir, "CHARM_PN_20140305_to_present_imputed.grd"))
dap_brick <- raster::brick(file.path(datadir, "CHARM_DAP_20140305_to_present_imputed.grd"))
dac_brick <- raster::brick(file.path(datadir, "CHARM_DAC_20140305_to_present_imputed.grd"))

# Read 100 fathoms polygon
bathy100 <- readRDS(file.path(bathydir, "CA_100_fathoms_polgyon_crude.Rds"))


# 100 fathoms averages
################################################################################

# Calculate means inside 100 fathoms (fishing grounds)
pn_avg_ca <- raster::extract(x=pn_brick, y=bathy100, method="simple", fun=mean, na.rm=T)
dap_avg_ca <- raster::extract(x=dap_brick, y=bathy100, method="simple", fun=mean, na.rm=T)
dac_avg_ca <- raster::extract(x=dac_brick, y=bathy100, method="simple", fun=mean, na.rm=T)


# Format data
pn_avg_ca_df <- tibble(variable="Psuedo-nitzschia",
                       date=colnames(pn_avg_ca) %>%  gsub("X", "", .) %>% ymd(),
                       risk=pn_avg_ca %>% as.numeric())
dap_avg_ca_df <- tibble(variable="Particulate domoic acid",
                       date=colnames(dap_avg_ca) %>%  gsub("X", "", .) %>% ymd(),
                       risk=dap_avg_ca %>% as.numeric())
dac_avg_ca_df <- tibble(variable="Cellular domoic acid",
                       date=colnames(dac_avg_ca) %>%  gsub("X", "", .) %>% ymd(),
                       risk=dac_avg_ca %>% as.numeric())

# Merge data
data <- bind_rows(pn_avg_ca_df, 
                  dap_avg_ca_df,
                  dac_avg_ca_df)

# Build data
################################################################################

# Function
calc_avg_by_layer <- function(rbrick){
  vals <- sapply(1:nlayers(rbrick), function(x) mean(getValues(rbrick[[x]]), na.rm=T))
  return(vals)
}

# Build data frame
build_df <- function(rbrick, var, server){
  vals <- calc_avg_by_layer(rbrick)
  dates <- names(rbrick) %>% gsub("X", "", .) %>% ymd()
  df <- tibble(server=server, variable=var, date=dates, risk=vals)
  return(df)
}

# Mean values by date
pn <- build_df(pn_brick, var="PN", server="THREDDS")
dap <- build_df(dap_brick, var="pDA", server="THREDDS")
dac <- build_df(dac_brick, var="cDA", server="THREDDS")

# Merge data
data <- bind_rows(pn, dap, dac) %>% 
  mutate(variable=recode_factor(variable,
                                "PN"="Pseudo-nitzschia",
                                "pDA"="Particulate domoic acid",
                                "cDA"="Cellular domoic acid")) 


# Plot data
################################################################################
  
# Build Dungeness season df
open_dates <- c("2014-01-01", paste0(2014:2019, "-11-01")) %>% ymd()
close_dates <- c("2014-07-01", paste0(2015:2020, "-07-01")) %>% ymd()
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
g <- ggplot(data, aes(x=date, y=risk)) +
  # Add seasons
  geom_rect(data=dcrab_season, inherit.aes=F,
            mapping=aes(xmin=open, xmax=close), ymin=0, ymax=1, fill="grey60", alpha=0.3) +
  # Add lines
  facet_wrap(~variable, ncol=1) +
  geom_line(lwd=0.3) + 
  # Axes
  ylim(0,1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # Labels
  labs(x="", y="Risk") +
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS1_charm_coverage.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

