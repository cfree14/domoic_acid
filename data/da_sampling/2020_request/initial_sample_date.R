

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/da_sampling/2020_request/processed"

# Read data
data_orig <- readRDS(file.path(datadir, "CDPH_crab_viscera_da_data.rds"))


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to Dcrab
  filter(comm_name=="Dungeness crab") %>% 
  # Add julian day
  mutate(julian_day=yday(date)) %>% 
  # Reduce to pre-season sampling dates
  filter(julian_day>240 & julian_day<320) %>% 
  # Identify pre-season surveys
  group_by(year, region, port, area, date, julian_day) %>% 
  summarize(ncrabs=n()) %>% 
  ungroup() %>% 
  filter(ncrabs>=6) %>% 
  # Identify first survey at each site
  group_by(year, region, port, area) %>% 
  summarize(day_of_first_survey=min(julian_day))
  



hist(data$day_of_first_survey)
abline(v=yday(ymd("2015-10-01")))

     