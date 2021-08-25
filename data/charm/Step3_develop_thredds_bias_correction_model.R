
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(mgcv)
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)
library(betareg)

# Directories
plotdir <- "data/charm/figures"
datadir <- "data/charm/processed"
gisdir <- "data/cdfw/gis_data/raw/StateWaterJurisdiction/"

# Read THREDDS data (3/5/2014-5/7/2019)
pn_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_PN_20140305_to_20190507.grd"))
dap_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAP_20140305_to_20190507.grd"))
dac_brick14 <- raster::brick(file.path(datadir, "CHARM_THREDDS_DAC_20140305_to_20190507.grd"))

# Read ERDDAP data (6/19/2018-present)
pn_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_PN_20180619_to_present.grd"))
dap_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAP_20180619_to_present.grd"))
dac_brick19 <- raster::brick(file.path(datadir, "CHARM_ERDDAP_DAC_20180619_to_present.grd"))


# Merge dates of overlap for comparison 
################################################################################

# THREDDS data (3/5/2014-5/7/2019)
# ERDDAP data (6/19/2018-present)
# Overlap: 6/19/2018 - 5/7/2019

# Dates
date1 <- "2018-06-19" %>% ymd()
date2 <- "2019-05-07" %>% ymd()
dates <- seq(date1, date2, by="1 day")
dates_c <- dates %>% as.character(.) %>% gsub("-", ".", .) %>% paste0("X", .)

# Prepare THREDDS data for merge
######################################

# Subset to days of interest
pda_thredds <- dap_brick14[[dates_c]]

# Convert to dataframe
pda_thredds_df <- as.data.frame(pda_thredds, xy=T)

# Format data frame
pda_thredds_df1 <- pda_thredds_df %>% 
  # Rename
  rename(long_dd=x, lat_dd=y) %>% 
  # Gather
  gather(key="date", value="pda_thredds", 3:ncol(.)) %>% 
  # Format date
  mutate(date=date %>% gsub("X", "", .) %>% ymd())

# Prepare ERDDAP data for merge
######################################

# Subset to days of interest
pda_erddap <- dap_brick19[[dates_c]]

# Convert to dataframe
pda_erddap_df <- as.data.frame(pda_erddap, xy=T)

# Format data frame
pda_erddap_df1 <- pda_erddap_df %>% 
  # Rename
  rename(long_dd=x, lat_dd=y) %>% 
  # Gather
  gather(key="date", value="pda_erddap", 3:ncol(.)) %>% 
  # Format date
  mutate(date=date %>% gsub("X", "", .) %>% ymd())

# Merge THREDDS and ERDDAP data
######################################

# Merge data
pda <- pda_thredds_df1 %>% 
  left_join(pda_erddap_df1, by=c("date", "lat_dd", "long_dd"))

# Subsample for plotting
pda_plot <- pda %>% 
  sample_n(size=10^5)


# Fit model
################################################################################


# Model formulation
# ERDDAP ~ THREDDS
# So that you can scale old THREDDS to match new and growing ERDDAP

# Fit beta regression
bfit <- betareg::betareg(pda_erddap ~ pda_thredds, data=pda_plot)
# summary(bfit)

# Fit GAM
gamfit <- mgcv::gam(pda_erddap ~ s(pda_thredds), data=pda_plot, family="betar")
#gam.check(gamfit)

# Predictions
x <- seq(0,1,0.01)
beta_pred <- predict(bfit, newdata=tibble(pda_thredds=pred_x), type = "response")
gam_pred <- mgcv::predict.gam(object=gamfit, newdata=tibble(pda_thredds=pred_x), type = "response")

# Build predictions dataframe for plotting
pred_df <- tibble(pda_thredds=x,
                  pda_erddap_gam=gam_pred,
                  pda_erddap_beta=beta_pred)


# Plot bias and bias correction
################################################################################

# Plot data
g1 <- ggplot(pda_plot, aes(x=pda_thredds, y=pda_erddap, fill=lat_dd)) + 
  geom_point(alpha=0.05, color="grey30", pch=21, stroke=0.1) + 
  # Labels
  labs(x="pDA risk\n(THREDDS server)", y="pDA risk\n(ERDDAP server)") +
  # Limits
  lims(x=c(0,1), y=c(0,1)) +
  # Legend
  scale_fill_gradientn(name="Latitude (°N)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Reference line
  geom_abline(slope=1) +
  # Plot GAM fit
  geom_line(data=pred_df, mapping=aes(x=pda_thredds, y=pda_erddap_gam), inherit.aes = F) +
  # Theme
  theme_bw()
g1

# Plot data
g2 <- ggplot(pda_plot, aes(x=pda_thredds, y=pda_erddap, fill=long_dd)) + 
  geom_point(alpha=0.05, color="grey30", pch=21, stroke=0.1) + 
  # Labels
  labs(x="pDA risk\n(THREDDS server)", y="pDA risk\n(ERDDAP server)") +
  # Limits
  lims(x=c(0,1), y=c(0,1)) +
  # Legend
  scale_fill_gradientn(name="Longitude (°W)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Reference line
  geom_abline(slope=1) +
  # Theme
  theme_bw()
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=2)
g


# Export useful stuff
################################################################################


# Export
saveRDS(pda, file=file.path(datadir, "20180619_20190507_thredds_errdap_overlap_dataset.Rds"))
save(gamfit, pda_plot, file=file.path(datadir, "thredds2erddap_data_and_model.Rdata"))





