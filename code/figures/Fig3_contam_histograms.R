


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)
library(lubridate)

# Directories
datadir <- "models/contamination/paper/data"
tabledir <- "models/contamination/paper/tables"
plotdir <- "models/contamination/paper/figures"

# Study species
study_species <- c("Dungeness crab",
                   "Rock crab",
                   "Spiny lobster",
                   "Razor clam",
                   "Sea mussel",
                   "Bay mussel",
                   "Pacific oyster")

# Read sample data
data_orig <- readRDS(file.path(datadir, "CDPH_crab_bivalve_domoic_acid_data.Rds"))

# DA ppm limit
da_ppm_limit <- 200

# Format data
data <- data_orig %>% 
  filter(comm_name %in% study_species) %>% 
  mutate(da_ppm_cap=pmin(da_ppm, da_ppm_limit),
         comm_name = factor(comm_name, levels=study_species),
         type=recode_factor(type, "wild"="Wild", "cultured"="Cultured", "sentinel"="Sentinel"))



# Plot histograms
################################################################################

# Action levels
action_levels <- tibble(comm_name=levels(data$comm_name), level_ppm=20) %>% 
  mutate(level_ppm=ifelse(comm_name=="Dungeness crab", 30, level_ppm),
         comm_name=factor(comm_name, levels=levels(data$comm_name)))

# Species key
spp_key <- data %>% 
  left_join(action_levels, by="comm_name") %>% 
  group_by(comm_name, level_ppm) %>% 
  summarize(n=n(),
            n_over=sum(da_ppm>=level_ppm),
            max_ppm=max(da_ppm),
            ymax= max(hist(da_ppm_cap, breaks=seq(0, da_ppm_limit, 5), plot=F)$count, right=F) ) %>% 
  mutate(p_over=n_over/n*100,
         p_over_label=paste0(format(p_over, digits=2, nsmall=1), "%"),
         n_label=paste0("n=", n, "\n", max_ppm, " ppm maximum", "\n", p_over_label, " contaminated"))

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_blank(),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot histograms
g <- ggplot(data, aes(x=da_ppm_cap)) +
  geom_histogram(breaks=seq(0,da_ppm_limit,5), closed="left") +
  facet_wrap(~comm_name, ncol=4, scales="free_y") +
  # Action level
  geom_vline(data=spp_key, mapping=aes(xintercept=level_ppm), lwd=0.3) +
  # Sample size
  geom_text(data=spp_key, aes(label=n_label, y=ymax), x=200, hjust=1, vjust=1, size=2) +
  # Labels
  scale_x_continuous(breaks=seq(0,200,50), labels=c(seq(0,150,50), ">200")) +
  labs(x="Domoic acid concentration (ppm)", y="Number of individuals") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_da_histograms.png"), 
       width=6.5, height=3.5, units="in", dpi=600)




