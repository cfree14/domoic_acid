
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "data/charm/figures"
datadir <- "data/charm/processed"

# Read data
data_orig <- readRDS(file.path(datadir, "CHARM_20140305_to_present_imputed_hovmoller_data.Rds"))


# Format data
################################################################################

# Impute missing values
lats_in <- sort(unique(data_orig$lat_dd)) %>% round(.,2)
lats_should <- seq(min(lats_in), max(lats_in), 0.03) %>% round(., 2)
lats_missing <- setdiff(lats_should, lats_in)

# Endpoints for linear interpolation
data_imputed <- data_orig %>% 
  # Ungroup
  ungroup() %>% 
  # Round latitudes
  mutate(lat_dd=round(lat_dd, 2)) %>% 
  # Remove number of cells
  select(-n) %>% 
  # Reduce to data for latitudes on either side of missing latitudes
  filter(lat_dd >= 36.129 & lat_dd <= 36.221) %>%
  # Spread to ease interpolation
  spread(key="lat_dd", value="risk_avg") %>% 
  # Add new columns
  mutate("36.16"=NA, 
         "36.19"=NA) %>% 
  # Arrange columns
  select(variable, date, "36.13", "36.16", "36.19", "36.22") %>% 
  rename("lat1"="36.13", "lat2"="36.16", "lat3"="36.19", "lat4"="36.22") %>% 
  # Perform interpolation
  mutate(lat2 = lat1 + (lat4-lat1)/3,
         lat3 = lat4 - (lat4-lat1)/3) %>% 
  # Reduce to interpolated data to be appended to data frame
  select(variable, date, lat2, lat3) %>% 
  gather(key="lat_dd", value="risk_avg", 3:ncol(.)) %>% 
  mutate(n=NA,
         lat_dd=recode(lat_dd, "lat2"="36.16", "lat3"="36.19") %>% as.numeric()) %>% 
  # Arrange to match data
  select(variable, date, lat_dd, n, risk_avg)


# Build data
data <- data_orig %>% 
  # Round latitude
  mutate(lat_dd=round(lat_dd,2)) %>% 
  # Append imputed data
  bind_rows(., data_imputed) %>% 
  # Arrange
  arrange(variable, date, lat_dd) %>% 
  # Recode variable name
  mutate(variable=recode_factor(variable,
                                "PN"="Pseudo-nitzchia", 
                                "DAP"="Particulate domoic acid (pDA)",
                                "DAC"="Cellular domoic acid (cDA)"))

# Export
saveRDS(data, file=file.path(datadir, "CHARM_20140305_to_present_imputed_hovmoller_data_for_plotting.Rds"))

# Sample data
data_sample <- data %>% 
  sample_frac(size=0.15)


# Plot data
################################################################################

# Plot raster
g <- ggplot(data, aes(x=date, y=lat_dd, fill=risk_avg)) +
  # Plot raster
  facet_wrap(~variable, ncol=1) +
  geom_tile() +
  # Labels
  labs(x="Day", y="Latitude (°N)") +
  scale_y_continuous(breaks=seq(32,42,2)) +
  # Legend
  scale_fill_gradientn(name="Risk", limits=c(0,1),
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
#g

# Export plot
ggsave(g, filename=file.path(plotdir, "charm_fishing_grounds_hovmoller.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




# Experiment with contours
################################################################################

library(fasterize)


# Get cDA 2015-2016 data
sdata <- data %>% 
  filter(variable=="Cellular domoic acid (cDA)" & date>="2015-01-01" & date <="2016-01-01")


# Plot
g <- ggplot(sdata, aes(x=date, y=lat_dd, z=risk_avg, fill=risk_avg)) +
  geom_tile() +
  geom_contour(breaks=0.5, color="black", lwd=0.4) +
  # Labels
  labs(x="Day", y="Latitude (°N)") +
  scale_y_continuous(breaks=seq(32,42,2)) +
  # Legend
  scale_fill_gradientn(name="Risk", limits=c(0,1),
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g

# Characterize events

# Convert to raster
event_thresh <- 0.5

# Format dataframe for conversion
sdata_df <- sdata %>% 
  ungroup() %>% 
  # Reduce to important columns
  select(lat_dd, date, risk_avg) %>% 
  # Format columns
  mutate(event=ifelse(risk_avg>event_thresh, T, NA),
         day=difftime(date, min(date), units="days") %>% as.numeric(), 
         lat=lat_dd - min(lat_dd)) %>% 
  # Reduce to important columns
  select(lat, day, event)

# Plot check
g <- ggplot(sdata_df, aes(x=day, y=lat, z=event, fill=event)) +
  geom_raster()
g

# Convert to raster (first stage)
sdata_ras1 <- sdata_df %>% 
  rasterFromXYZ()

# Convert to polygon (SF)
sdata_sf <- sdata_ras1 %>% 
  # Convert to polygons (SP)
  rasterToPolygons() %>% 
  # Convert to SF
  sf::st_as_sf() %>% 
  # Dissolve into single polygon
  summarize() %>% 
  # Break into seperate polygons
  sf::st_cast("POLYGON") %>%
  sf::st_cast("MULTIPOLYGON") %>%
  # Add event number
  mutate(eventid=1:n())

# Convert SF back to rasters
sdata_ras2 <- fasterize(sf=sdata_sf, raster=sdata_ras1, field="eventid")

# Convert to dataframe
sdata_df2 <- sdata_ras2 %>% 
  as.data.frame(xy=T) %>% 
  rename(day=y, lat=x, eventid=layer) %>% 
  filter(!is.na(eventid))
  
# Plot events
g <- ggplot(sdata_df2, aes(x=day, y=lat, fill=as.factor(eventid))) +
  geom_tile() +
  theme_bw() +
  theme(legend.position = "none")
g

# Characterize events
events_stats <- sdata_df2 %>% 
  group_by(eventid) %>% 
  summarize(length_days=n_distinct(day),
            height_lat=max(lat) - min(lat),
            ncells=n()) %>% 
  ungroup() %>% 
  gather(key="metric", value="value", 2:ncol(.))

# Plot event characteristics
g <- ggplot(events_stats, aes(x=value)) +
  facet_wrap(~metric, ncol=3, scales="free") +
  geom_histogram() + theme_bw()
g

