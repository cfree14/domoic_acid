
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tabulizer)
library(tidyverse)

# Directories
inputdir <- "data/da_sampling/raw/reports"
outputdir <- "data/da_sampling/processed"
plotdir <- "data/da_sampling/figures"

# Vignette: https://cran.r-project.org/web/packages/tabulizer/vignettes/tabulizer.html
# Installation issues: https://github.com/ropensci/tabulizer/issues/105
# 1) Update Java from Java panel in System Preferences
# 2) Restart computer
# 3) If necessary, run the following code in the terminal: sudo R CMD javareconf


# Function
################################################################################

# Function to extract DA table from PDF
extract_table <- function(file, headers){

  # Extract tables from PDF
  data_orig <- extract_tables(file, method="decide")
  
  # Which pages contain tables?
  pages_do <- which(sapply(data_orig, ncol)==length(headers))
  
  # Merge pages with tables
  data_merged <- purrr::map_df(pages_do, function(x) {
    df <- data_orig[[x]] %>% 
      as.data.frame() %>% 
      setNames(headers) %>% 
      mutate(page=x) %>% 
      select(page, everything())
  })
  
  # Format merged data
  data <- data_merged %>% 
    # Remove header columns
    filter(port!="PORT" & !grepl("SUMMARY", port)) %>% 
    # Remove empty rows
    filter(n_samples!="")
  
  # If date column does not contain a date, shift columns to the right
  if(any(!grepl("/", data$date))){
    rows_to_shift <- which(!grepl("/", data$date))
    data1 <- data
    data1[rows_to_shift,3:ncol(data1)] <- data1[rows_to_shift, 2:(ncol(data1)-1)]
    data1$port[rows_to_shift] <- ""
    data <- data1
  }
  
  # Fill in missing ports
  for(i in 1:nrow(data)){
    port <- data$port[i]
    prev_ports <- unique(data$port[1:i])
    prev_ports <- prev_ports[prev_ports!=""]
    last_port <- prev_ports[length(prev_ports)]
    if(port==""){
      data$port[i] <- last_port
    }
  }
  
  # Return
  return(data)
  
}


# 2015 data
################################################################################

# Read data
crabs15 <- extract_table(file=file.path(inputdir, "CrabDA15-16.pdf"),
                         headers=c("port", "dates", "n_samples", "range_ppm", "mean_ppm", "perc_over"))

# Format data
crabs15new <- crabs15 %>%
  # Extract area species from date
  mutate(area_species=gsub("[\\(\\)]", "", regmatches(dates, gregexpr("\\(.*?\\)", dates))), 
         area_species=plyr::revalue(area_species, c("character0"=""))) %>% 
  # Remove ()s from date
  mutate(dates=trimws(gsub("\\(.*?\\)", "", dates))) %>% 
  # Add species column
  mutate(species="Dungeness crab") %>% 
  mutate(species=ifelse(grepl("Rock Crab", area_species), "Rock crab", species),
         species=ifelse(grepl("Sheep Crab", area_species), "Sheep crab", species),
         species=ifelse(grepl("Spider Crab", area_species), "Spider crab", species),
         species=ifelse(grepl("Red Rock Crab", area_species), "Red rock crab", species),
         species=ifelse(grepl("Yellow Rock Crab", area_species), "Yellow rock crab", species)) %>% 
  # Create area column
  mutate(area=trimws(gsub("Rock Crab-|Sheep Crab|Spider Crab|Red Rock Crab", "", area_species))) %>% 
  mutate(area=ifelse(area=="", port, area)) 
  # Arrange
  select(-area_species) %>% 
  select(port, area, dates, species, everything())


table(crabs15new$port)



# 2016 data
# 1) fill missing ports, 2) shift some right
crabs16 <- extract_table(file=file.path(inputdir, "CrabDA16-17.pdf"),
                         headers=c("port", "area", "dates", "species", "n_samples", "range_ppm", "mean_ppm", "perc_over"))

# 2017 data
# fill missing ports
crabs17 <- extract_table(file=file.path(inputdir, "CrabDA17-18.pdf"),
                         headers=c("port", "area", "dates", "species", "n_samples", "range_ppm", "mean_ppm", "perc_over"))

# 2018 data
crabs18 <- extract_table(file=file.path(inputdir, "CrabDA18-19.pdf"),
                         headers=c("port", "area", "dates", "species", "n_samples", "range_ppm", "mean_ppm", "perc_over"))
