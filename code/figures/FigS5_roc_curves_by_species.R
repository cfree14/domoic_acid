
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "output/model_tests"
plotdir <- "figures"

# Build data
################################################################################

# Species do
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam", "Sea mussel", "Bay mussel")

# Build ROC curve data
######################################

# Loop through species
x <- spp_do[1]
data <- purrr::map_df(spp_do, function(x) {
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", x)), "_evaluation.Rds")
  sdata <- readRDS(file.path(datadir, infile))
  
  # ROC curve
  roc_curve <- sdata$roc_curves %>% 
    mutate(species=x) %>% 
    dplyr::select(species, everything())
  
})

# Format data
data1 <- data %>% 
  mutate(species=factor(species, levels=spp_do)) %>% 
  mutate(model_type=gsub(" \\(cDA)| \\(pDA)", "", model),
         pred_type=ifelse(grepl("cDA", model), "cDA", "pDA"))


# Build test data sample size/performance data
######################################

# Sample size stats
x <- spp_do[1]
stats <- purrr::map_df(spp_do, function(x) {
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", x)), "_evaluation.Rds")
  sdata <- readRDS(file.path(datadir, infile))
  
  # Sample size
  n_test <- sdata$preds %>% 
    mutate(correct=true==estimate) %>% 
    group_by(model, true) %>% 
    summarize(n=n(),
              ncorrect=sum(correct),
              pcorrect=ncorrect/n) %>% 
    mutate(true=recode(true, 
                       "0"="below",
                       "1"="above")) %>% 
    mutate(species=x)
  
})

# Format stats # MAJOR HACK HERE - PREPARE STATS FOR BEST MODEL
stats_label <- stats %>% 
  # Classify best models
  mutate(model_type=gsub(" \\(cDA)| \\(pDA)", "", model),
         best=ifelse((species=="Rock crab" & model_type=="Boosted regression trees") | (species!="Rock crab" & model_type=="Random forest"), T, F)) %>% 
  filter(best) %>% 
  # Format species
  mutate(species=factor(species, levels=spp_do)) %>% 
  # Build label
  mutate(label=paste0(n, " ", true, " (", round(pcorrect*100, 1), "% correct)")) %>% 
  # Arrange
  arrange(species, desc(true)) %>% 
  # Collapse label
  group_by(species, model_type) %>% 
  summarize(label_combo=paste(label, collapse="\n"))

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

# Plot ROC curves
g <- ggplot(data1, aes(x=fpr, y=tpr, color=model_type)) +
  # Facet by species
  facet_wrap(~species, ncol=3) +
  # Plot ROC curves
  geom_line() +
  # Plot reference line
  geom_abline(slope=1, intercept=0, col="black", linetype="dotted") +
  # Labels
  labs(x="False positive rate (1-specificity)", y="True positive rate (sensitivity)") +
  # Sample size
  geom_text(data=stats_label, inherit.aes=F, mapping=aes(x=1, y=0.05, label=label_combo, color=model_type), size=2,  hjust=1, show.legend = FALSE) +
  # Legend
  scale_color_discrete(name="Model") +
#guides(color = guide_legend(title.position="top", title.hjust = 0)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS5_roc_curves_by_species.png"), 
       width=6.5, height=5, units="in", dpi=600)





