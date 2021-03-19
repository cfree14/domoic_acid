
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "input"
evaldir <- "output/model_tests"
plotdir <- "figures"

# Build data
################################################################################

# Species do
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam", "Sea mussel", "Bay mussel")

# Sample size stats
x <- spp_do[1]
data <- purrr::map_df(spp_do, function(x) {
  
  # Read test data
  infile <- paste0(tolower(gsub(" ", "_", x)), "_data_split.Rds")
  sdata <- readRDS(file.path(datadir, infile))
  
  # Read predictions
  infile <- paste0(tolower(gsub(" ", "_", x)), "_evaluation.Rds")
  spreds <- readRDS(file.path(evaldir, infile))
  
  # Build data
  sdata1 <- sdata$data_test %>% 
    select(comm_name, sampleid, da_ppm, over) %>% 
    mutate(tempid=1:n())
  
  # Format predictions
  spreds1 <- spreds$preds %>% 
    # Add temporary id
    group_by(model) %>% 
    mutate(tempid=1:n()) %>% 
    # Add sample info
    left_join(sdata1, by="tempid")
  
  # Confirm that you did this correctly
  #sum(spreds1$over != spreds1$true)
  
})


# Plot data
################################################################################

# Best model key
best_model_key <- tibble(comm_name=spp_do, 
                         model="Random forest (cDA)",
                         best=T) %>% 
  mutate(model=ifelse(comm_name=="Rock crab", "Boosted regression trees (cDA)", model))

# Data to plot
data_plot <- data %>% 
  left_join(best_model_key) %>% 
  filter(best) %>% 
  mutate(da_ppm_cap=pmin(200, da_ppm),
         comm_name=factor(comm_name, levels=spp_do))

# Calculate percentages in each quadrant
quad_percs <- data_plot %>% 
  # Classify predictions/observations and create categories
  mutate(da_thresh_ppm=ifelse(comm_name=="Dungeness crab", 30, 20), 
         pred_type=ifelse(prob_over>=0.5, "over", "under"),
         obs_type=ifelse(da_ppm_cap>=da_thresh_ppm, "over", "under"),
         type=paste(pred_type, obs_type, sep="-")) %>% 
  # Calculate number in each category
  group_by(comm_name, type) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Calculate percentage in each category
  group_by(comm_name) %>% 
  mutate(perc=n/sum(n)) %>% 
  mutate(perc_label=paste0(format(perc*100, digits=1, nsmall=1), "%")) %>% 
  # Format categories
  mutate(type=recode(type, 
                     "over-over"="True positive",
                     "over-under"="False positive",
                     "under-over"="False negative",
                     "under-under"="True negative"),
         xpos=ifelse(type %in% c("True negative", "False positive"), 0, 200),
         ypos=ifelse(type %in% c("True negative", "False negative"), 0, 1),
         hjust=ifelse(type %in% c("True negative", "False positive"), 0, 1))
  

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
g <- ggplot(data_plot, aes(y=prob_over, x=da_ppm_cap)) +
  # Plot points
  facet_wrap(~comm_name, ncol=3) +
  geom_point(col="grey75") +
  # Plot reference lines
  geom_hline(yintercept=0.5) +
  geom_vline(xintercept=30) +
  # Plot percent labels
  geom_text(data=quad_percs, mapping=aes(x=xpos, y=ypos, label=perc_label, hjust=hjust), color="black", size=2.2, fontface="bold") +
  # Labels
  labs(x="Observed\ndomoic acid contamination (ppm)", y="Predicted\np(above action threshold)") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS6_confusion_matrix_scatterplots.png"), 
       width=6.5, height=5, units="in", dpi=600)


