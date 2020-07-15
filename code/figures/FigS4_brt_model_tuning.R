
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


# Build data
################################################################################

# BRT model files
model_files <- list.files(datadir, pattern="brt")

# Loop through model files
x <- model_files[[1]]
data <- purrr::map_df(model_files, function(x) {
  
  # Read model file
  brt_fit <- readRDS(file.path(datadir, x))
  
  # Extract tune data
  brt_tune <- brt_fit$results %>% 
    mutate(model_file=x)
  
})

# Format data
data1 <- data %>% 
  janitor::clean_names(case="snake") %>% 
  mutate(variable=ifelse(grepl("pda", model_file), "pDA", "cDA"),
         species=model_file %>% gsub("_model_brt_cda.Rds|_model_brt_pda.Rds", "", .) %>% 
           gsub("_", " ", .) %>% str_to_sentence(.)) %>% 
  dplyr::select(species, variable, everything()) %>% 
  dplyr::select(-model_file)

# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  plot.title=element_blank(),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.position = "bottom")

# Plot data
g <- ggplot(data1 %>% filter(variable=="pDA"), aes(x=n_trees, y=kappa, 
                                                   color=as.factor(interaction_depth), 
                                                   group=as.factor(interaction_depth))) +
  # Facet by species
  facet_grid(species ~ shrinkage) +
  # Lines and points
  geom_line() +
  geom_point() +
  # Legend
  scale_color_discrete(name="Interaction depth") +
  # Labels
  labs(x="Number of trees", y="Cohen's kappa") +
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_brt_model_tuning_pda.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




