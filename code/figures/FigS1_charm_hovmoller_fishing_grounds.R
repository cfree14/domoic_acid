
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "figures"
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

# Sample data
data_sample <- data %>% 
  sample_frac(size=0.15)


# Plot data
################################################################################

range(data$date)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  axis.title.x = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  legend.position="right")

# Plot raster
g <- ggplot(data, aes(x=date, y=lat_dd, fill=risk_avg)) +
  # Plot raster
  facet_wrap(~variable, ncol=1) +
  geom_tile() +
  # Labels
  labs(x="", y="Latitude (°N)") +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(breaks=seq(32,42,2)) +
  # Legend
  scale_fill_gradientn(name="Mean daily risk", limits=c(0,1),
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
#g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS1_charm_hovmoller_fishing_grounds.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



