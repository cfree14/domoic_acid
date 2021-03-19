
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "output/model_tests"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- read.csv(file.path(tabledir, "TableS1_model_performance_metrics.csv"), as.is=T)

# Build data
################################################################################

# Species do
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", 
            "Razor clam", "Sea mussel", "Bay mussel")

# Format data
data <- data_orig %>% 
  # Factor species and model type
  mutate(species=factor(species, levels=spp_do),
         model_type=recode(model_type, "Random forest"="Random forests"),
         model_type=factor(model_type, c("Logistic regression", "Boosted regression trees", "Random forests"))) %>% 
  # Add labels
  mutate(kappa_catg=cut(kappa %>% round(., 2), breaks=c(-Inf, 0.2, 0.4, 0.7, 1), labels=c("Poor", "Fair", "Good", "Excellent"), right = F),
         auc_catg=cut(auc %>% round(., 2), breaks=c(-Inf, 0.7, 0.8, 0.9, 1), labels=c("Poor", "Acceptable", "Excellent", "Outstanding"), right = F),
         catg_label=paste(kappa_catg, auc_catg, sep=", ")) %>% 
  # Identify best model
  mutate(orig_dist=sqrt(kappa^2+auc^2)) %>% 
  group_by(species) %>% 
  mutate(best_model=orig_dist==max(orig_dist))
  

# Specify best
best_models <- data %>% 
  filter(best_model)
  


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
  filter(model=="Random forest (cDA)") %>% 
  mutate(species=factor(species, levels=spp_do)) %>% 
  mutate(label=paste0(n, " ", true, " (", round(pcorrect*100, 1), "% correct)")) %>% 
  arrange(species, desc(true)) %>% 
  group_by(species) %>% 
  summarize(label_combo=paste(label, collapse="\n"))


# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  plot.title=element_blank(),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.position = "bottom")

# Plot performance metrics scatter plot
g <- ggplot(data, aes(x=kappa, y=auc, size=accuracy, color=model_type)) +
  # Facet by species
  facet_wrap(~species, ncol=3) +
  # Plot reference lines
  geom_vline(xintercept = c(0.2, 0.4, 0.7), linetype="dotted", color="grey80", lwd=0.2) + # Cohen's kappa
  geom_hline(yintercept = c(0.5, 0.7, 0.8, 0.9), linetype="dotted", color="grey80", lwd=0.2) + # AUC
  # Limits
  scale_y_continuous(breaks=c(0.5, 0.7, 0.8, 0.9, 1), 
                     labels=c("0.5\nPoor", "0.7\nAcceptable", "0.8\nExcellent", "0.9\nOutstanding", "1.0\nPerfect"), lim=c(NA, 1.0)) +
  scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.7, 1), lim=c(NA, 1),
                     labels=c("0.0\nPoor", "0.2\nFair", "0.4\nGood", "0.7\nExcellent", "1.0\nPerfect")) +
  # Plot points
  geom_point() +
  # Label best models
  geom_text(data=best_models, mapping=aes(x=kappa+0.05, y=auc, color=model_type, label=catg_label), inherit.aes=F, hjust=0, show.legend=F, size=2) +
  # Labels
  labs(x="Cohen's kappa", y="Area under the curve (AUC)") +
  # Legends
  scale_color_discrete(name="Model") +
  scale_size_continuous(name="Accuracy", range = c(0.5, 3), breaks=seq(0.75, 0.95, 0.1)) +
  guides(size = guide_legend(title.position="top", title.hjust = 0),
         color = guide_legend(title.position="top", title.hjust = 0)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g


# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_model_performance.png"), 
       width=6.5, height=5, units="in", dpi=600)





