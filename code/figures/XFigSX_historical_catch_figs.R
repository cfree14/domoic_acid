

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/landings_confidential/processed"
plotdir <- "models/contamination/paper/figures"

# Read landings data
load(file.path(datadir, "CDFW_dungeness_landings_data.Rdata"))


# Build data
################################################################################


pcatch_south_mb <- data %>% 
  # California only
  filter(block_state=="California" & season!="out-of-season" & !is.na(block_lat_dd)) %>% 
  # Mark catch north/south of Monterey Bay
  mutate(mb=ifelse(block_lat_dd>=36.5, "North", "South")) %>% 
  # Summarize catch
  group_by(season, mb) %>% 
  summarize(catch_mt=sum(landings_mt)) %>% 
  ungroup() %>% 
  # Calculate propotion
  group_by(season) %>% 
  mutate(catch_prop=catch_mt/sum(catch_mt)) %>% 
  ungroup() %>% 
  # Filter to south
  filter(mb=="South")

#
mean_past <- pcatch_south_mb %>% 
  pull(catch_prop) %>% 
  mean()
mean_recent <- pcatch_south_mb %>% 
  arrange(desc(season)) %>% 
  slice(1:5) %>% 
  pull(catch_prop) %>% 
  mean()

# 
g <- ggplot(pcatch_south_mb, aes(x=season, y=catch_prop*100)) +
  geom_bar(stat="identity") +
  # Means
  geom_hline(yintercept=mean_past*100, linetype="dashed") +
  geom_hline(yintercept=mean_recent*100, linetype="dashed") +
  # Labels
  labs(x="", y="Percent of landings") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))
g


# Prop by port

pcatch_south_mb_port <- data %>% 
  # California only
  filter(block_state=="California" & season!="out-of-season" & !is.na(block_lat_dd)) %>% 
  # Mark catch north/south of Monterey Bay
  mutate(mb=ifelse(block_lat_dd>=36.5, "North", "South")) %>% 
  # Summarize by port
  group_by(season, port_name, mb) %>% 
  summarize(catch_mt=sum(landings_mt),
            nvessels=n_distinct(vessel_id)) %>% 
  group_by(season, port_name) %>% 
  mutate(catch_prop=catch_mt/sum(catch_mt))


g <- ggplot(pcatch_south_mb_port, aes(x=season, y=catch_prop, fill=mb)) +
  geom_bar(stat="identity") +
  facet_wrap(~port_name, ncol=6) +
  theme_bw()
g





            