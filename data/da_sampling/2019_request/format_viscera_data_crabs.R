
# Things to do:
# 1. Block 208 doesn't have a block centroid
# Add region

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(rio)
library(readxl)
library(tidyverse)
library(lubridate)

# Directories
inputdir <- "data/da_sampling/raw"
outputdir <- "data/da_sampling/processed"
plotdir <- "data/da_sampling/figures"
blockdir <- "data/cdfw/gis_data/processed"

# Read data
crab1517_orig <- read_excel(file.path(inputdir, "Free Request 061019 2015-2019 DA revised.xlsx"), sheet=1)
crab1718_orig <- read_excel(file.path(inputdir, "Free Request 061019 2015-2019 DA revised.xlsx"), sheet=2)
crab1819_orig <- read_excel(file.path(inputdir, "Free Request 061019 2015-2019 DA revised.xlsx"), sheet=3)
lobster16_orig <- read_excel(file.path(inputdir, "Free Request 061019 2015-2019 DA revised.xlsx"), sheet=4)
lobster1718_orig <- read_excel(file.path(inputdir, "Free Request 061019 2015-2019 DA revised.xlsx"), sheet=5)
lobster1819_orig <- read_excel(file.path(inputdir, "Free Request 061019 2015-2019 DA revised.xlsx"), sheet=6)

# Read block centroids
blocks <- read.csv(file.path(blockdir, "CA_commercial_fishing_blocks.csv"))

# Read sampling site coords
sites <- read_excel(file.path(inputdir, "2014_crab_da_sampling_sites.xlsx"))


# Format individual files
################################################################################

crab1517 <- crab1517_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         block_id="block #",
         depth_fthm="depth (fathoms)",
         da_ppm="result (ppm)",
         comm_name="species")
colnames(crab1517)

crab1718 <- crab1718_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         block_id="block #",
         comm_name="species - viscera",
         depth_fthm="depth (fathoms)",
         da_ppm="result (ppm)",
         lat_long="lat/long coordinates")
colnames(crab1718)

crab1819 <- crab1819_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         date="date of catch",
         comm_name="species - viscera",
         block_id="block #",
         lat_long="lat/long coordinates",
         depth_fthm="depth (fathoms)",
         da_ppm="result (ppm)") %>% 
  dplyr::select(sampleid:da_ppm)
colnames(crab1819)

lobster16 <- lobster16_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         block_id="block #",
         depth_fthm="depth (fathoms)",
         da_ppm="result (ppm)",
         comm_name=species) %>% 
  dplyr::select(sampleid:da_ppm)
colnames(lobster16)  

lobster1718 <- lobster1718_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         block_id="block #",
         lat_long="lat/long coordinates",
         depth_fthm="depth (fathoms)",
         da_ppm="result (ppm)",
         comm_name=species) 
colnames(lobster1718)  

lobster1819 <- lobster1819_orig %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(sampleid="is#",
         date="date of catch",
         comm_name="species - viscera",
         block_id="block #",
         lat_long="lat/long coordinates",
         depth_fthm="depth (fathoms)",
         da_ppm="result (ppm)") 
colnames(lobster1819) 


# Merge and format files
################################################################################

# Regions
n_ports <- c("Crescent City", "Trinidad", "Eureka", "Fort Bragg")
c_ports <- c("Bodega Bay", "Half Moon Bay/SF", "Half Moon Bay", 
             "Santa Cruz", "Monterey", "Monterey Bay", "Morro Bay", "San Luis Obispo", 
             "Santa Barbara", "Ventura", "LA/San Pedro", "Catalina", "San Diego")

# Merge
data_merged <- bind_rows(crab1517, crab1718, crab1819, 
                         lobster16, lobster1718, lobster1819)

# Look for duplicated IDs
dup_ids <- data_merged$sampleid[duplicated(data_merged$sampleid)]
dup_data <- data_merged %>% 
  filter(sampleid %in% dup_ids) %>% 
  arrange(sampleid)

# Remove duplicated entries
data_merged_nodups <- data_merged %>% unique()
anyDuplicated(data_merged_nodups$sampleid)

# Format
data <- data_merged_nodups %>% 
  # Format date
  mutate(#date=ifelse(date==as.POSIXct("1900-01-11"), as.POSIXct("2016-01-27"), date),
         date=ymd(date),
         year=year(date),
         day=yday(date)) %>% 
  # Remove incorrect dates
  filter(year > 2000) %>% 
  # Format ports
  mutate(port=recode(port, "Ft. Bragg"="Fort Bragg",
                           "Trindad"="Trinidad",
                           "Cresecent City"="Crescent City")) %>% # Monterey == Monterey Bay?
  # Add region
  mutate(region=ifelse(port%in%n_ports, "Northern", "Central")) %>% 
  # Format depth
  # 1 fathom = 6 feet = 1.8 meters
  # Convert everything to fathoms then meters
  mutate(depth_fthm=plyr::revalue(depth_fthm, c("Unk"=NA,
                                       "unknown"=NA,
                                       "< 25"=25 / 6,
                                       "< 30"=30 / 6,
                                       "> 25"=25 / 6,
                                       "> 30"=30 / 6,
                                       "10 ft"=10 / 6, 
                                       "10-20 feet"=15 / 6, 
                                       "110 feet"=110 / 6,
                                      "140 feet"=140 / 6,
                                      "15 feet"=15 / 6,
                                      "22-35 feet"=28.5 / 6,
                                      "30 feet"=30 / 6,
                                      "30 to 40 feet"=35 / 6,
                                      "31 to 40 feet"= 35.5 / 6,
                                      "32 to 40 feet"=36 / 6,
                                      "33 to 40 feet"=36.5 / 6,
                                      "34 to 40 feet"=38 / 6,
                                      "35 feet"=35 / 6,
                                      "40 feet"=40 / 6,
                                      "45 feet"=45 / 6,
                                      "60 feet"=60 / 6)),
         depth_fthm=as.numeric(depth_fthm),
         depth_ft=depth_fthm*6,
         depth_m=depth_ft * 0.3048) %>%
  # Format common name and sex
  mutate(comm_name=freeR::sentcase(comm_name),
         sex=ifelse(comm_name=="Lobster (f)", "female", 
                    ifelse(comm_name=="Lobster (m)", "male", "unknown")),
         comm_name=recode(comm_name, "Lobster"="Spiny lobster",
                                 "Lobster (f)"="Spiny lobster", 
                                 "Lobster (m)"="Spiny lobster")) %>% 
  # Add species name
  mutate(species=recode(comm_name,
                        "Dungeness crab"="Metacarcinus magister",
                        "Rock crab"="Cancer spp.", # Cancer productus, Cancer anthonyi, Cancer antennarius
                        "Sheep crab"="Loxorhynchus grandis",
                        "Spider crab"="unknown", # this might be Sheep crab as per Duy Truong June 27, 2019 email
                        "Spiny lobster"="Panulirus interruptus")) %>%  # California spiny lobster
  # Add season
  # Northern: Dec 1 (335) to Jul 15 (196)
  # Central: Nov 15 (319) to Jun 30 (181)
  mutate(season_type=ifelse(comm_name!="Dungeness crab", "not relevant", 
                       ifelse(region=="Northern" & (day>=335 | day <=196), "in-season",
                              ifelse(region=="Central" & (day>=319 | day <=181), "in-season", "out-of-season"))),
         season=ifelse(season_type=="in-season" & day>=319, paste(year, year+1, sep="-"),
                       ifelse(season_type=="in-season" & day<319, paste(year-1, year, sep="-"), season_type))) %>% 
  # Format DA ppm
  mutate(da_ppm_prefix=ifelse(grepl("<|ND|nd", da_ppm), "<", ""), 
         da_ppm=as.numeric(ifelse(grepl("<|ND|nd", da_ppm), "2.5", da_ppm))) %>%
  # Format lat/long
  rename(latlong_orig=lat_long) %>% 
  mutate(latlong_orig=str_trim(latlong_orig)) %>% 
  # Rearrange columns
  left_join(dplyr::select(blocks, block_id, block_long_dd, block_lat_dd), by="block_id") %>% 
  dplyr::select(sampleid, year, date, day, season,
         region, port, area, block_id, block_long_dd, block_lat_dd,
         latlong_orig, depth_m, depth_fthm,
         comm_name, species, sex, da_ppm_prefix, da_ppm)
  
# Any duplicated IDs?
anyDuplicated(data$sampleid)

# Inspect data (plus a few remaining problems)
freeR::complete(data)
table(data$sex)
table(data$season)
table(data$port)
table(data$area)

# Dungeness stats
stats <- data %>% 
  filter(species=="Dungeness crab") %>% 
  group_by(year, region, port, area, season) %>% 
  summarize(n=n())


# Export temporary data file to deal with some of the lat/long encoding problems
write.csv(data, file=file.path(inputdir, "crab_da_data_temp.csv"), fileEncoding="UTF-8", row.names=F)


# Read data for lat/long formatting
################################################################################

# Read in temporary file
data1 <- read.csv(file.path(inputdir, "crab_da_data_temp.csv"), as.is=T, fileEncoding = "UTF-8", strip.white = T)

# Format unique lat/longs
latlong <- data1 %>% 
  dplyr::select(latlong_orig) %>% 
  unique() %>% 
  mutate(latlong=trimws(latlong_orig),
         # Erase NA info
         latlong=ifelse(latlong %in% c("not avail / SB Operation", "Doran Beach", "not available"), NA, latlong),
         # Remove degree/minute symbols
         latlong=gsub("’|°|'", "", latlong), 
         # Erase N/S designators
         latlong=gsub("N|N |W|W ", "", latlong),
         # Convert to comma seperated values
         latlong=gsub(" -", ", ", latlong),
         latlong=gsub(" / ", ", ", latlong),
         latlong=gsub(",,", ",", latlong), 
         latlong=gsub(" , ", ",", latlong),
         latlong=gsub(" -", ", ", latlong),
         # Revalue a few troublesome values
         latlong=sapply(latlong, function(x) trimws(x)),
         latlong=plyr::revalue(latlong, c("34.052324,-119.568205" = "34.052324, 119.568205",
                                          "34 00 945, 119 21 340" = "34 00.945, 119 21.340",
                                          "38 41 123 27" = "38 41, 123 27",
                                          "34 00.533120 02.569" = "34 00.533, 120 02.569",
                                          "40 39.03 124 24.01 or 40 38.95 124 24.05" = "40 39.03, 124 24.01",
                                          "40 39.06 124 25.82 or 40 39.00 124 25.84" = "40 39.06, 124 25.82",
                                          "39 00.42, 123 44.772 and \n 39 01.25, 123 46.779" = "39 00.42, 123 44.772",
                                          "1) 34 02.56, 119 31.93. 2) 34 02.69, 119 32.54. 3) 34 01.77, 119 36.52" = "34 02.56, 119 31.93",
                                          "33 59.598 119 33.772 or 33 59.624 119 34.298" = "33 59.598, 119 33.772",
                                          "34 02.26, 119 31.50 or 34 02.45, 119 31.73" = "34 02.26, 119 31.50",
                                          "41 16.00 -124 09.00" = "41 16.00, 124 09.00",
                                          "41 33.00 -124 11.00" = "41 33.00, 124 11.00",
                                          "41 46.00 124 15.00" = "41 46.00, 124 15.00",
                                          "40 39.8 124 23.5"  = "40 39.8, 124 23.5",
                                          "40 39.3 124 23.7"  = "40 39.3, 124 23.7",
                                          "41.46.62 124.19.13"  = "41.46.62, 124.19.13",
                                          "41 38 124  10" = "41 38, 124 10",
                                          "41 45 124  19" = "41 45, 124 19",
                                          "41 45.700  124 19.400" = "41 45.700, 124 19.400",
                                          "41 45.000  124 18.705" = "41 45.000, 124 18.705",
                                          "41 10.47  124 11.32" = "41 10.47, 124 11.32",
                                          "41 10.11  124 13.06" = "41 10.11, 124 13.06",
                                          "33 59.383 119 32.134" = "33 59.383, 119 32.134",
                                          "34 03.2499 119 32.80" = "34 03.2499, 119 32.80",
                                          "34 02.60 119 32.24" = "34 02.60, 119 32.24",
                                          "33 59.310 119 32.427" = "33 59.310, 119 32.427",
                                          "34 02.69 119 32.45" = "34 02.69, 119 32.45"))) %>% 
  # Add length of names
  mutate(nchar=nchar(latlong)) %>% 
  mutate(lat=sapply(latlong, function(x) unlist(strsplit(x, ","))[1]),
         lat=trimws(lat),
         long=sapply(latlong, function(x) unlist(strsplit(x, ","))[2]),
         long=trimws(long),
         lat_dd=as.numeric(substr(lat, 1, 2)) + as.numeric(substr(lat, 4, nchar(lat)))/60,
         long_dd=as.numeric(substr(long, 1, 3)) + as.numeric(substr(long, 5, nchar(lat)))/60) %>% 
  # Fix ones already in decimal degrees
  mutate(lat_dd=ifelse(lat_dd>180, lat, lat_dd),
         long_dd=ifelse(long_dd>180, long, long_dd))

# Add formatted coordinates to original data
data2 <- data1 %>%  
  left_join(dplyr::select(latlong, latlong_orig, lat_dd, long_dd), by="latlong_orig")

# Some of the lat/longs didn't get properly formatted
# Look up the sample ids of samples missing coordinates but with coord info
sdata <- data2 %>%
  filter(!is.na(latlong_orig) & is.na(lat_dd))

# Ids of samples with coord info that needs to get put in manually
prob_ids1 <- c("168100317M", "168100317N", "168100317O", "168100317P", "168100317Q", "168100317R")
prob_ids2 <- c("168100317S", "168100317T", "168100317U", "168100317V", "168100317W", "168100317X")

# Add corrected lat/longs
data2$lat_dd[data2$sampleid %in% prob_ids1] <- 41+16/60
data2$long_dd[data2$sampleid %in% prob_ids1] <- (124+9/60)
data2$lat_dd[data2$sampleid %in% prob_ids2] <- 41+33/60 
data2$long_dd[data2$sampleid %in% prob_ids2] <- (124+11/60)  

# Multiply longitudes by -1
data3 <- data2 %>% 
  mutate(lat_dd=as.numeric(lat_dd),
         long_dd=as.numeric(long_dd), 
         long_dd = -1 * long_dd) %>% 
  dplyr::select(sampleid:latlong_orig, long_dd, lat_dd, everything())

# Export final data
write.csv(data3, file=file.path(outputdir, "CDPH_crab_viscera_da_data.csv"), row.names=F)


