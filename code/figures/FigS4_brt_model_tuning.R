
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(tidymodels)
library(randomForest)

# Directories
datadir <- "output/candidate_models"
plotdir <- "figures"
tabledir <- "tables"


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
           gsub("_", " ", .) %>% str_to_sentence(.),
         species=factor(species, levels=c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam", "Sea mussel", "Bay mussel"))) %>% 
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

# Kappa values
kappas <- c(0.2, 0.4, 0.7)
kappa_labels <- data.frame(kappa=c(0.2,0.2,0.4,0.7), label=c("poor", "fair", "good", "excellent"), vjust=c(1.3,-0.3,-0.3,-0.3))

# Plot data
g <- ggplot(data1, aes(x=n_trees, y=kappa, color=as.factor(interaction_depth), group=as.factor(interaction_depth))) +
  # Facet by species
  facet_grid(species ~ shrinkage) +
  # Plot points/lines/errors
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=kappa-kappa_sd, ymax=kappa+kappa_sd), width=0, alpha=0.2) +
  # Add kappa reference points
  geom_hline(yintercept=kappas, col="grey60", linetype="dotted", lwd=0.3) +
  geom_text(data=kappa_labels, mapping=aes(y=kappa, label=label, vjust=vjust), x=10000, hjust=1, inherit.aes = F, col="grey60", size=2.2) +
  # Legend
  scale_color_discrete(name="Interaction depth") +
  # Labels
  labs(x="Number of trees", y="Cohen's kappa") +
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.7), lim=c(-0.1, 0.8)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS4_brt_model_tuning.png"), 
       width=6.5, height=7.0, units="in", dpi=600)




