
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(raster)
library(tidyverse)
library(RColorBrewer)
library(gganimate)
library(gifski)

# Directories
datadir <- "data/processed"
plotdir <- "data/figures"

# Read PN probability
data <- readRDS(file.path(datadir, "charm_pn_nowcast_dataframe.Rds"))



# Build data
results <- data %>% 
  group_by(date) %>% 
  summarize(prop=sum(prob>0.7 & !is.na(prob))/sum(!is.na(prob)))

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(results, aes(x=date, y=prop)) +
  geom_line() +
  geom_point() +
  labs(x="", y="High-risk of Pseudo-nitzschia\nProportional coverage") +
  theme_bw() + my_theme
g
ggsave(g, filename=file.path(plotdir, "FigX_charm_pn_high_risk_over_time.png"),
       width=6.5, height=3.5, units="in", dpi=600)



# Format for plotting
################################################################################
# 
# dates <- sort(unique(data$date))
# breaks <- seq(0,1,0.05)
# 
# 
# results <- data %>% 
#   filter(!is.na(prob)) %>% 
#   mutate(prob_catg=cut(prob, breaks=seq(0,1,0.05))) %>% 
#   group_by(date, prob_catg) %>% 
#   summarize(n=n()) %>% 
#   mutate(prop=n/sum(n),
#          prob_catg_val=breaks[2:length(breaks)][prob_catg]) %>% 
#   ungroup()
# 
# 
# g <- ggplot(results, aes(x=date, y=prop, 
#                          group=prob_catg_val, fill=prob_catg_val)) +
#   geom_area() +
#   scale_fill_gradientn(name="p(Pseudo-nitzschia)", 
#                        colors=brewer.pal(9, "YlOrRd")) +
#   labs(x="", y="Coverage") +
#   theme_bw()
# g
# 
# 
# 
