
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(tidymodels)
library(randomForest)

# Directories
datadir <- "models/contamination/paper/output"
plotdir <- "models/contamination/paper/figures"
tabledir <- "models/contamination/paper/tables"

# Read model
model <- readRDS(file=file.path(datadir, "dungeness_crab_model_rf_pda.Rds"))

# Read data
################################################################################

# Variable importance
var_imp_df <- caret::varImp(model$finalModel) %>% 
  mutate(day=row.names(.) %>% gsub("pda|cda", "", .) %>% as.numeric(.)) %>% 
  rename(rel_imp=Overall) %>% 
  select(day, rel_imp)

# Plot data
g <- ggplot(var_imp_df, aes(x=day, y=rel_imp)) +
  geom_bar(stat="identity") +
  labs(y="Relative importance", x="Day") +
  coord_flip() +
  theme_bw()
g

