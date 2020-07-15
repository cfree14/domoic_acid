
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)
library(plyr)

# Directories
inputdir <- "data/pier_sampling/raw"
outputdir <- "data/pier_sampling/processed"
plotdir <- "data/pier_sampling/figures"

# Read data
# All pier data downloaded from here:
# http://www.sccoos.org/data/habs/history.php?location=Santa%20Cruz%20Wharf
data_orig <- read.delim(file.path(inputdir, "Harmful Algal Blooms_2008-2019.csv"),
                        sep=",", skip=7, header=T, check.names=F, na.strings="NaN", as.is=T)
colnames(data_orig) <- iconv(colnames(data_orig), to = "ASCII", sub = "")
str(data_orig)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  rename(lat_dd="latitude",
         long_dd="longitude",
         depth_m="depth (m)",
         akashiwo_cellsL="Akashiwo sanguinea (cells/L)",
         alexandrium_cellsL="Alexandrium spp. (cells/L)",
         ammonia_uM="Ammonia (uM)",
         chl_mgm3="Chlorophyll (mg/m3)",
         chl1_mgm3="Chlorophyll 1 (mg/m3)",
         chl2_mgm3="Chlorophyll 2 (mg/m3)",
         da_vol_filtered_mL="DA Volume Filtered (mL)",
         dinophysis_cellsL="Dinophysis spp. (cells/L)",
         da_ngml="Domoic Acid (ng/mL)",
         lingulodinium_cellsL="Lingulodinium polyedrum (cells/L)",
         nitrate_uM="Nitrate (uM)",
         nitrite_uM="Nitrite (uM)",
         diatoms_cellsL="Other Diatoms (cells/L)",
         dinoflags_cellsL="Other Dinoflagellates (cells/L)",
         phaeophytin_mgm3="Phaeophytin (mg/m3)",
         phaeophytin1_mgm3="Phaeophytin 1 (mg/m3)",
         phaeophytin2_mgm3="Phaeophytin 2 (mg/m3)",
         phosphate_uM="Phosphate (uM)",
         prorocentrum_cellsL="Prorocentrum spp. (cells/L)",
         pn_delicat_cellsL="Pseudo-nitzschia delicatissima group (cells/L)",
         pn_seriata_cellsL="Pseudo-nitzschia seriata group (cells/L)",
         silicate_uM="Silicate (uM)",
         vol4counting_settled_mL="Volume Settled for counting (mL)",
         temp_c="Water Temperature (C)",
         vol4counting_mL="Volume for counting (mL)",
         diatoms1_cellsL="Other diatoms (cells/L)",
         dinoflags1_cellsL="Other dinoflagellates (cells/L)") %>% 
  # Add date/time columns
  mutate(date=ymd(paste(year, month, day, sep="-")),
         datetime=ymd_hms(paste(date, time))) %>% 
  # Rearrange columns
  select(date, datetime, everything()) %>% 
  arrange(location, datetime)

# Perform checks
anyDuplicated(colnames(data))
str(data)

# Export data
saveRDS(data, file.path(outputdir, "2008_2019_pier_sampling_data.Rds"))


# Piers
piers <- data %>%
  select(location, long_dd, lat_dd) %>% 
  unique()

# Time intervals
intervals <- difftime(data$datetime[2:nrow(data)], data$datetime[1:(nrow(data)-1)], units="hours")
hist(as.numeric(intervals), breaks=seq(0,200,0.25))
abline(v=1)

# Quick plot
################################################################################


g <- ggplot(data, aes(x=datetime, y=pn_delicat_cellsL/1e6, col=location)) +
  geom_line() +
  labs(x="", y="Pseudo-nitzschia delicatissima\n(millions of cells per liter)") +
  # geom_hline(yintercept = 10000) +
  scale_color_discrete(name="Location") +
  theme_bw()
g


g <- ggplot(data, aes(x=datetime, y=pn_seriata_cellsL/1e6, col=location)) +
  geom_line() +
  labs(x="", y="Pseudo-nitzschia seriata\n(millions of cells per liter)") +
  # geom_hline(yintercept = 10000) +
  scale_color_discrete(name="Location") +
  theme_bw()
g



