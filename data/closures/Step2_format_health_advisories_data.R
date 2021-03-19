
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "figures"
datadir <- "data/closures/data"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "CA fishery closures spreadsheet.xlsx"), 
                                sheet="CDPH health advisories", na="NA")


# Clean data
################################################################################

# Formatting steps
# 1. Basic formatting
# 2. Split rows with multiple species into 1 row per species
# 3. Format species

# Column names
colnames(data_orig)
cols <- c("year", "num", "date", "species_parts", "fishery", "action", "reason", "where",  "lat_s", "lat_n", "link", "notes")

# Step 1. Basic formatting
data1 <- data_orig %>% 
  # Rename columns
  setNames(cols) %>% 
  # Format date
  mutate(date=ymd(date))

# Inspect data
str(data1)
range(data1$date)
range(data1$lat_s, na.rm=T)
range(data1$lat_n, na.rm=T)
sum(data1$lat_n <= data1$lat_s, na.rm=T)
table(data1$reason)
table(data1$action)
table(data1$fishery)


# Step 2. One row per species
data2 <- purrr::map_df(1:nrow(data1), function(x) {
  
  # Get row
  row <- data1 %>% slice(x)
  
  # Identify species represented in row
  spp_in_row_string <- row %>% pull(species_parts)
  spp_in_row_list <- strsplit(spp_in_row_string, split=", ")
  spp_in_row_cvec <- unlist(spp_in_row_list)
  
  # Duplicate row, if necessary
  nspp <- length(spp_in_row_cvec)
  if(nspp>1){
    row_out <- row %>% 
      slice(rep(1:n(), each=nspp)) %>% 
      mutate(species_parts=spp_in_row_cvec)
  }else{
    row_out <- row
  }
  
})


# Step 3. Format species
data3 <- data2 %>% 
  # Break apart species and parts
  mutate(comm_name=gsub(" \\(.*", "", species_parts),
         parts=gsub(".*\\((.*)\\).*", "\\1", species_parts)) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name, 
                          "Anchovies"="Northern anchovy",
                          "Sardines"="Pacific sardine",
                          "Lobster"="Spiny lobster", 
                          "Lobsters"="Spiny lobster",
                          "Rock crabs"="Rock crab",
                          "Dungeness crabs"="Dungeness crab")) %>% 
  # Format parts
  mutate(parts=recode(parts, "viscera/roe"="viscera")) %>% 
  # Build advisory
  mutate(advisory_abbrev=paste(action, parts, sep="-"),
         advisory_long=recode(advisory_abbrev, 
                         "close-viscera"="Consumption of viscera not advised", 
                         "close-whole"="No consumption advised",
                         "open-meat"="Consumption of viscera not advised",
                         "open-viscera"="No advisory", 
                         "open-whole"="No advisory"),
         advisory_short=recode(advisory_abbrev, 
                              "close-viscera"="Partial", 
                              "close-whole"="Full",
                              "open-meat"="Partial",
                              "open-viscera"="None", 
                              "open-whole"="None")) %>% 
  # Arrange columns
  select(comm_name, parts, species_parts, year, date, everything()) %>% 
  arrange(comm_name, date)

# Inspect
table(data3$comm_name)
table(data3$parts)
table(data3$advisory_abbrev)
table(data3$advisory_long)
table(data3$advisory_short)


# Step 4. Simple data set
data4 <- data3 %>% 
  # Relevant data
  filter(!is.na(lat_s) & reason %in% c("Domoic acid", "End-of-season")) %>% 
  # Relevant columns
  select(num, date, comm_name, parts, fishery, action, advisory_short, lat_s, lat_n, where) %>% 
  # Arrange
  arrange(comm_name, date)

# Format for export
events <- data3 %>% 
  # Relevant columns
  select(num, year, date, comm_name, parts, fishery, reason, action, lat_s, lat_n, where, link, notes, everything()) %>% 
  select(-c(advisory_short, advisory_abbrev, advisory_long, species_parts)) %>% 
  # Arrange
  arrange(comm_name, date)


# Function to build data
################################################################################

# Dungeness commercial season key
# North max: First Saturday of November - July 30
# Central max: First Saturday of November - June 30
years <- 2013:2019
seasons <- paste(years, years+1-2000, sep="-")
open <- freeR::first_wday_in_month(day="Saturday", month="November", years=years)
close_n <-  paste0(years+1, "-07-30") %>% ymd()
close_c <-  paste0(years+1, "-06-30") %>% ymd()
nyears <- length(years)
dcrab_season_key <- tibble(comm_name="Dungeness crab", 
                           region=c(rep("Northern", nyears), rep("Central", nyears)),
                           season=rep(seasons,2),
                           open=rep(open, 2),
                           close=c(close_n, close_c))

# Function to build grid
# data <- data4; species <- "Dungeness crab"; season_key <- dcrab_season_key
build_closure_grid <- function(data, species, season_key=NULL){
  
  # Subset data
  sdata <- data %>% 
    filter(comm_name==species)
  
  # Build empty grid
  date1 <- ymd("2014-01-01")
  date2 <- ymd("2020-07-31")
  dates <- seq(date1, date2, by="1 day")
  lat1 <- 32.5
  lat2 <- 42
  lats <- seq(32.5, 42, 0.01)
  closure_grid <- expand.grid(date=dates, lat_dd=lats) %>% 
    as.data.frame() %>% 
    mutate(advisory="None",
           comm_name=species) %>% 
    select(comm_name, date, lat_dd, advisory) %>% 
    arrange(date, lat_dd)
  
  # Loop through announcements
  for(i in 1:nrow(sdata)){
    
    # Get announcement
    date1 <- sdata %>% slice(i) %>% pull(date)
    lat_s <- sdata %>% slice(i) %>% pull(lat_s)
    lat_n <- sdata %>% slice(i) %>% pull(lat_n)
    advisory_new <- sdata %>% slice(i) %>% pull(advisory_short)
    
    # Apply announcement
    closure_grid <- closure_grid %>% 
      mutate(advisory=ifelse(lat_dd>=lat_s & lat_dd <= lat_n & date>=date1, advisory_new, advisory))
    
  }
  
  # Apply out-of-season label
  if(!is.null(season_key)){
    
    # Subset fishery
    n_key <- season_key %>% 
      filter(region=="Northern")
    closed_dates_n <- purrr::map_df(2:nrow(n_key), function(x) {
      date1 <- n_key$close[x-1]+1
      date2 <- n_key$open[x]-1
      dates <- tibble(date=seq(date1, date2, by="day"))
    })
    c_key <- season_key %>% 
      filter(region=="Central")
    closed_dates_c <- purrr::map_df(2:nrow(c_key), function(x) {
      date1 <- c_key$close[x-1]+1
      date2 <- c_key$open[x]-1
      dates <- tibble(date=seq(date1, date2, by="day"))
    })
    
    # Apply out-of-season closures
    closure_grid <- closure_grid %>% 
      # Northern closures
      mutate(advisory=ifelse(lat_dd >= 38.766488 & date %in% closed_dates_n$date, "Out-of-season", advisory)) %>% 
      # Central closures
      mutate(advisory=ifelse(lat_dd < 38.766488 & date %in% closed_dates_c$date, "Out-of-season", advisory))
    
  }
  
  # Format closure gird
  closure_grid1 <- closure_grid %>% 
    mutate(advisory=factor(advisory, levels=c("Out-of-season", "None", "Partial", "Full")))
  
  # Plot closure grid
  g <- ggplot(closure_grid1, aes(x=date, y=lat_dd, fill=advisory)) +
    # Plot raster
    geom_raster() +
    # Plot events
    geom_segment(data=sdata, mapping=aes(x=date, y=lat_s, xend=date, yend=lat_n, 
                                         linetype=action, color=parts), inherit.aes = F, lwd=1) +
    # Axis
    scale_x_date(date_breaks="1 year", date_labels = "%Y") +
    # Labels
    labs(x="", y="Latitude (°N)", title=species) +
    # Legends
    scale_color_manual(values=c("green", "blue", "black")) +
    scale_fill_manual(name="Public health advisory", values=c("grey80", "grey50", "coral", "red"), drop=F) +
    # Theme
    theme_bw()
  print(g)
  
  # Return
  return(closure_grid1)
  
}


# Build data individually
################################################################################

# Apply
closures_mussels <- build_closure_grid(data=data4, species="Mussels")
closures_clams <- build_closure_grid(data=data4, species="Clams")
closures_scallops <- build_closure_grid(data=data4, species="Scallops")
closures_anchovies <- build_closure_grid(data=data4, species="Northern anchovy")
closures_sardines <- build_closure_grid(data=data4, species="Pacific sardine")
closures_lobsters <- build_closure_grid(data=data4, species="Spiny lobster")
closures_dcrabs <- build_closure_grid(data=data4, species="Dungeness crab", season_key =  dcrab_season_key)
closures_rcrabs <- build_closure_grid(data=data4, species="Rock crab")

# Merge
advisories <- bind_rows(closures_mussels,
                        closures_clams,
                        closures_scallops,
                        closures_anchovies,
                        closures_sardines,
                        closures_lobsters,
                        closures_dcrabs,
                        closures_rcrabs)

# Export data
saveRDS(advisories, file=file.path(datadir, "CDPH_2014_2020_health_advisories.Rds"))
write.csv(events, file=file.path(datadir, "CDPH_2014_2020_health_advisory_announcements.csv"))




