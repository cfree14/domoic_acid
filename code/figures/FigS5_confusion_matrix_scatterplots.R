
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "models/contamination/paper/output"
plotdir <- "models/contamination/paper/figures"

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
  spreds <- readRDS(file.path(datadir, infile))
  
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

# Data to plot
data_plot <- data %>% 
  filter(model=="Random forest (pDA)") %>% 
  mutate(da_ppm_cap=pmin(200, da_ppm),
         comm_name=factor(comm_name, levels=spp_do))

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
  facet_wrap(~comm_name, ncol=3) +
  geom_point() +
  geom_hline(yintercept=0.5) +
  geom_vline(xintercept=30) +
  labs(x="Observed\ndomoic acid contamination (ppm)", y="Predicted\np(above action threshold)") +
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS5_confusion_matrix_scatterplots.png"), 
       width=6.5, height=5, units="in", dpi=600)


