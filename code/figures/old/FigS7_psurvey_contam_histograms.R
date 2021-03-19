

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/da_sampling/2020_request/processed/"
tabledir <- "models/contamination/paper/tables"
plotdir <- "models/contamination/paper/figures"

# Read data
data <- readRDS(file.path(datadir, "CDPH_crab_viscera_da_data.Rds"))


# Build data
################################################################################

# Label survey
stats <- data %>% 
  # Build and sort by survey id
  mutate(surveyid=paste(date, area, sep="-")) %>% 
  select(surveyid, everything()) %>% 
  arrange(surveyid) %>% 
  # Add domoic acid threshold
  mutate(action_ppm=ifelse(comm_name=="Dungeness crab", 30, 20)) %>% 
  # Compute survey stats
  group_by(comm_name, year, surveyid) %>% 
  summarize(n=n(),
            n_over=sum(da_ppm >= action_ppm),
            p_over=n_over/n) %>% 
  # Remove species
  filter(comm_name %in% c("Dungeness crab", "Rock crab", "Spiny lobster") & n>=5)



# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Build stats
spp_key <- stats %>% 
  group_by(comm_name) %>% 
  summarize(n=n(),
            ymax= max(hist(p_over, breaks=seq(0, 1, 0.1), plot=F)$count, right=T) ) %>% 
  mutate(n_label=paste0(n, " surveys")) %>% 
  mutate(closure_trigger=ifelse(comm_name=='Dungeness crab', 0.5, NA))

# Plot results for all species
g <- ggplot(stats, aes(x=p_over)) +
  facet_wrap(~comm_name, scale="free_y") +
  geom_histogram(breaks=seq(0,1,0.1)) +
  # Add closure trigger
  geom_vline(spp_key, mapping=aes(xintercept=closure_trigger), linetype="dotted") +
  # Add sample size
  geom_text(spp_key, mapping=aes(x=1, y=ymax, label=n_label), hjust=1, size=2.5) +
  # Labels
  labs(x="Proportion of samples\nabove the domoic acid action level", y="Number of surveys") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS7_psurvey_contam_histograms.png"), 
       width=6.5, height=2.5, units="in", dpi=600)






  
