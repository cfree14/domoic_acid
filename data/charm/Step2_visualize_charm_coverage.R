
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
plotdir <- "data/charm/figures"
datadir <- "data/charm/processed"
gisdir <- "data/cdfw/gis_data/raw/StateWaterJurisdiction/"

# Read THREDDS data (3/5/2014-5/7/2019)
pn_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_PN_20140305_to_20190507.grd"))
dap_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAP_20140305_to_20190507.grd"))
dac_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAC_20140305_to_20190507.grd"))

# Read ERDDAP data (5/8/2019-present)
pn_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_PN_20180619_to_present.grd"))
dap_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAP_20180619_to_present.grd"))
dac_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAC_20180619_to_present.grd"))


# Examination #1
################################################################################

# Build data
##########################

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
pn1 <- build_df(pn_brick14, var="PN", server="THREDDS")
dap1 <- build_df(dap_brick14, var="pDA", server="THREDDS")
dac1 <- build_df(dac_brick14, var="cDA", server="THREDDS")
pn2 <- build_df(pn_brick19, var="PN", server="ERDDAP")
dap2 <- build_df(dap_brick19, var="pDA", server="ERDDAP")
dac2 <- build_df(dac_brick19, var="cDA", server="ERDDAP")


# Format data
##########################

# Merge data
data_merge <- bind_rows(pn1, dap1, dac1,
                  pn2, dap2, dac2) 

# Build key to fill in missing values
date_key <- expand.grid(date=seq(min(data_merge$date), max(data_merge$date), "1 day"),
                        server=c("THREDDS", "ERDDAP"),
                        variable=c("PN", "pDA", "cDA"))

# Build data
data <- date_key %>%
  full_join(data_merge) %>% 
  mutate(variable=recode_factor(variable,
                                "PN"="Pseudo-nitzschia",
                                "pDA"="Particulate domoic acid",
                                "cDA"="Cellular domoic acid")) 


# Plot data
##########################

# Build Dungeness season df
open_dates <- paste0(2014:2019, "-11-01") %>% ymd()
close_dates <- paste0(2015:2020, "-07-01") %>% ymd()
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
g <- ggplot(data, aes(x=date, y=risk, color=server)) +
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
  scale_color_discrete(name="Server") +
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_charm_data_server_comparison.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

# Export data
write.csv(data, file=file.path(datadir, "CHARM_pn_pda_pca_means_by_server.csv"), row.names=F)

