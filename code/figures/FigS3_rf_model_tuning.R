
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

# RF model files
rf_model_files <- list.files(datadir, pattern="rf")

# Loop through model files
x <- rf_model_files[[1]]
data <- purrr::map_df(rf_model_files, function(x) {
  
  # Read model file
  rf_fit <- readRDS(file.path(datadir, x))
  
  # Extract tune data
  rf_tune <- rf_fit$results %>% 
    mutate(model_file=x)
  
})

# Format data
data1 <- data %>% 
  janitor::clean_names(case="snake") %>% 
  mutate(variable=ifelse(grepl("pda", model_file), "pDA", "cDA"),
         species=model_file %>% gsub("_model_rf_cda.Rds|_model_rf_pda.Rds", "", .) %>% 
           gsub("_", " ", .) %>% str_to_sentence(.),
         species=factor(species, levels=c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam"))) %>% 
  select(species, variable, everything()) %>% 
  select(-model_file)

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
g <- ggplot(data1, aes(x=mtry, y=kappa, color=variable)) +
  # Facet by species
  facet_wrap(~ species, ncol=3) +
  # Plot points/lines/errors
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=kappa-kappa_sd, ymax=kappa+kappa_sd), width=0, alpha=0.2) +
  # Add kappa reference points
  geom_hline(yintercept=kappas, col="grey60", linetype="dotted", lwd=0.3) +
  geom_text(data=kappa_labels, mapping=aes(y=kappa, label=label, vjust=vjust), x=20, hjust=1, inherit.aes = F, col="grey60", size=2.2) +
  # Legend
  scale_color_discrete(name="Predictors") +
  # Labels
  labs(x="Number of variables", y="Cohen's kappa") +
  scale_x_continuous(breaks=seq(10,20, 2)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_rf_model_tuning.png"), 
       width=6.5, height=5, units="in", dpi=600)




