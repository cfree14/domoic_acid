
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

# Build performance metrics data
######################################

# Loop through species
x <- spp_do[1]
data <- purrr::map_df(spp_do, function(x) {
  
  # Read data
  infile <- paste0(tolower(gsub(" ", "_", x)), "_evaluation.Rds")
  sdata <- readRDS(file.path(datadir, infile))
  
  # Performance metrics
  pmetrics <- sdata$pmetrics %>% 
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
g <- ggplot(data1, aes(x=kappa, y=auc, size=accuracy, color=model_type)) +
  # Facet by species
  facet_wrap(~species, ncol=3) +
  # Plot reference lines
  geom_vline(xintercept = c(0.2, 0.4, 0.7), linetype="dotted", color="grey80", lwd=0.3) + # Cohen's kappa
  geom_hline(yintercept = c(0.5, 0.7, 0.8, 0.9), linetype="dotted", color="grey80", lwd=0.3) + # AUC
  # Limits
  scale_y_continuous(breaks=c(0.5, 0.7, 0.8, 0.9, 1), 
                     labels=c("0.5\nCoin flip", "0.7\nAcceptable", "0.8\nExcellent", "0.9\nOutstanding", "1.0\nPerfect"), lim=c(0.5, 1.0)) +
  scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.7, 1), lim=c(NA, 1),
                     labels=c("0.0\nPoor", "0.2\nFair", "0.4\nGood", "0.7\nExcellent", "1.0\nPerfect")) +
  # Plot points
  geom_point() +
  # Labels
  labs(x="Cohen's kappa", y="Area under the curve (AUC)") +
  # Legends
  scale_color_discrete(name="Model") +
  scale_size_continuous(name="Accuracy") +
  guides(size = guide_legend(title.position="top", title.hjust = 0),
         color = guide_legend(title.position="top", title.hjust = 0)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g


# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_model_performance.png"), 
       width=6.5, height=5, units="in", dpi=600)





