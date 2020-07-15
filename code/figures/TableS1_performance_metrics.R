

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "models/contamination/paper/output"
plotdir <- "models/contamination/paper/figures"
tabledir <- "models/contamination/paper/tables"


# Build data
################################################################################

# Species do
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", 
            "Razor clam", "Sea mussel", "Bay mussel")

# Loop through species
x <- spp_do[1]
data <- purrr::map_df(spp_do, function(x) {
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", x)), "_evaluation.Rds")
  sdata <- readRDS(file.path(datadir, infile))
  stats <- sdata$pmetrics %>% 
    mutate(species=x) %>% 
    dplyr::select(species, everything())
  
})

# Format data
data_out <- data %>% 
  # Order species
  mutate(species=factor(species, levels=spp_do)) %>% 
  # Add model / predictor columns
  mutate(model_type=gsub(" \\(cDA)| \\(pDA)", "", model),
         pred_type=ifelse(grepl("cDA", model), "cDA", "pDA")) %>% 
  # Arrange columns
  dplyr::select(species, model_type, pred_type, auc, kappa, accuracy) %>% 
  # Format stats
  mutate(auc=round(auc, digits=2), 
         kappa=round(kappa, digits=2), 
         accuracy=round(accuracy, digits=2)) %>% 
  # Arrange
  arrange(species, desc(auc))


# Export table
################################################################################

# Export
write.csv(data_out, file=file.path(tabledir, "TableS1_model_performance_metrics.csv"), row.names=F)





