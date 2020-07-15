
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(tidyverse)
library(lubridate)

# Directories
inputdir <- "data/da_sampling/raw/reports"
outputdir <- "data/da_sampling/processed"
plotdir <- "data/da_sampling/figures"

# Read data
data_orig <- import(file.path(inputdir, "2015_2019_viscera_da_sampling.xlsx"))
razor_orig <- read.csv(file.path(inputdir, "2017_2019_razor_clam_da_data.csv"), as.is=T)


# Format data
################################################################################
  
# Format data
data <- data_orig %>% 
  # Format mean and range values
  # Pull out min and max values from range
  # Non-detectable = NA
  mutate(mean_ppm1=gsub(" ppm", "", mean_ppm), 
         mean_ppm1=as.numeric(ifelse(mean_ppm1=="Non-detectable", NA, mean_ppm1)),
         range_ppm1=gsub(" ", "", gsub("ppm", "", gsub("<", "", range_ppm))),
         range_ppm1=ifelse(range_ppm1%in%c("Non-detectable", "non-detectable"), NA, range_ppm1), 
         min_ppm=as.numeric(sapply(range_ppm1, function(x) strsplit(x, "-")[[1]][1])),
         max_ppm=as.numeric(sapply(range_ppm1, function(x) strsplit(x, "-")[[1]][2])),
         date=ymd(date),
         year=year(date)) %>% 
  # Arrange columns
  select(species, port, area, season, year, date, n_samples, range_ppm1, min_ppm, max_ppm, mean_ppm1) %>% 
  arrange(species, port, area, date) 

# Razor clam data
razor <- razor_orig %>% 
  mutate(date=mdy(date),
         year=year(date),
         da_ppm=as.numeric(ifelse(da_ppm=="<2.5", 2.5, da_ppm)))
  

  
# Plot data
################################################################################

# Plot data
g <- ggplot(data, aes(x=year, weight=n_samples, fill=species)) +
  geom_bar() +
  labs(x="", y="Number of samples") +
  scale_fill_discrete(name="Species")
g


raz <- ggplot(razor, aes(x=year)) +
  geom_bar() +
  labs(x="", y="Number of samples")
raz


stats <- data %>% 
  group_by(year, species) %>% 
  summarise(n_samples=sum(n_samples))

