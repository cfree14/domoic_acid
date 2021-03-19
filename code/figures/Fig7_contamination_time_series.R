
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(raster)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)

# Directories
preddir <- "output/model_preds"
plotdir <- "figures"

# Study species
study_species <- c("Dungeness crab", "Rock crab", 
                  "Spiny lobster", "Razor clam")


# Build data
################################################################################

# Calculate mean
calc_avg_by_layer <- function(rbrick){
  vals <- sapply(1:nlayers(rbrick), function(x) mean(getValues(rbrick[[x]]), na.rm=T))
  return(vals)
}

# Calculate SD
calc_sd_by_layer <- function(rbrick){
  vals <- sapply(1:nlayers(rbrick), function(x) sd(getValues(rbrick[[x]]), na.rm=T))
  return(vals)
}

# If build
build <- T
if(build){

  # Models
  models <- c("dungeness_crab_model_rf_cda.Rds",
              "rock_crab_model_brt_cda.Rds",
              "spiny_lobster_model_rf_cda.Rds",
              "razor_clam_model_rf_cda.Rds")
  
  # Loop through models
  x <- models[2]
  data_orig <- purrr::map_df(models, function(x) {
    
    # Model
    model_do <- gsub(".Rds", "", x)
    infile <- paste0(model_do, "_predictions_range_mask.grd")
    
    # Read model
    sbrick <- brick(file.path(preddir, infile))
  
    # Calculate averages
    pcontam_avgs <- calc_avg_by_layer(sbrick)
    pcontam_sds <- calc_sd_by_layer(sbrick)
    
    # Build data frame
    spp_do <- gsub("_model_rf_cda|_model_brt_cda", "", model_do) %>% gsub("_", " ", .) %>% stringr::str_to_sentence()
    dates <- names(sbrick) %>% gsub("X", "", .) %>% ymd()
    df <- tibble(species=spp_do,
                 date=dates,
                 pcontam_avg=pcontam_avgs,
                 pcontam_sd=pcontam_sds)
  
  })
  
  
  # Format data
  data <- data_orig %>% 
    mutate(species=factor(species, study_species))
  
  # Export
  saveRDS(data, file=file.path(preddir, "species_range_pcontam_averages.Rds"))
  
}else{
  
  # Read built data
  data <- readRDS(file.path(preddir, "species_range_pcontam_averages.Rds"))
  
}

# Build season lines
################################################################################

# Function to build season key
# species <- "Dungeness crab"; fishery_type <- "Commercial"; region <- "Northern"; open_date <- "12-01"; close_date <- "07-15"
build_season_key <- function(species, fishery_type, region, open_date, close_date){
  dates_open <- paste(2013:2019, open_date, sep="-") %>% ymd()
  dates_close <- paste0(2014:2020, close_date, sep="-")  %>% ymd()
  season_key <- tibble(species=species,
                       fishery_type=fishery_type,
                       region=region, 
                       open_date=dates_open, 
                       close_date=dates_close) %>% 
    mutate(line_group=1:n()) %>% 
    select(species:region, line_group, everything()) %>% 
    gather(key="endpoint", value="date", 5:ncol(.)) %>% 
    arrange(species, fishery_type, region, line_group)
  return(season_key)
}

# Dungeness crab season keys
dcrab_comm_n_key <- build_season_key(species="Dungeness crab", fishery_type="Commercial", 
                                     region="Northern", open_date="12-01", close_date="07-15")
dcrab_comm_c_key <- build_season_key(species="Dungeness crab", fishery_type="Commercial", 
                                     region="Central", open_date="11-15", close_date="06-30")
dcrab_rec_n_key <- build_season_key(species="Dungeness crab", fishery_type="Recreational", 
                                    region="Northern", open_date="11-01", close_date="07-30")
dcrab_rec_c_key <- build_season_key(species="Dungeness crab", fishery_type="Recreational", 
                                    region="Central", open_date="11-01", close_date="06-30")

# Lobster season keys
lobster_comm_key <- build_season_key(species="Spiny lobster", fishery_type="Commercial", 
                                     region="All", open_date="10-01", close_date="03-15")
lobster_rec_key <- build_season_key(species="Spiny lobster", fishery_type="Recreational", 
                                    region="All", open_date="10-01", close_date="03-15")

# Season key
season_key <- bind_rows(dcrab_comm_n_key, dcrab_comm_c_key,
                        dcrab_rec_n_key, dcrab_rec_c_key,
                        lobster_comm_key, lobster_rec_key) %>% 
  # Add latitudes to plot at
  mutate(lat_plot=31.5,
         lat_plot=ifelse(fishery_type=="Commercial", lat_plot+0.3, lat_plot),
         lat_plot=ifelse(region=="Central", lat_plot-0.15, lat_plot)) %>% 
  # Make new line group id (unique)
  mutate(line_group=paste(species, fishery_type, region, line_group), sep="-") %>% 
  mutate(species=factor(species, levels=study_species))


# Plot data
################################################################################

# Format data for plotting
data <- data %>% 
  mutate(species=factor(species, levels=study_species))

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  axis.title.x = element_blank(),
                  plot.title=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot contamination time series
g <- ggplot(data, aes(x=date, y=pcontam_avg)) +
  facet_wrap(~species, ncol=1) +
  # Plot errors
  geom_ribbon(mapping=aes(x=date, ymin=pcontam_avg-pcontam_sd, ymax=pcontam_avg+pcontam_sd), fill="grey80") +
  # Plot lines
  geom_line(lwd=0.2) +
  # Plot seasons
  geom_line(data=season_key, mapping=aes(x=date, y = 0, group=line_group)) +
  # Axis
  scale_y_continuous(lim=c(0,1)) +
  scale_x_date(breaks=seq(ymd("2014-01-01"), ymd("2020-01-02"), by="1 year"), labels=2014:2020) +
  # Labels
  labs(x="", y="Mean daily contamination risk\ninside fishing grounds") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig7_contamination_time_series.png"), 
       width=4.5, height=4.5, units="in", dpi=600)





