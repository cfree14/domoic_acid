
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
datadir <- "data/charm/raw"
plotdir <- "data/charm/figures"

# Read data
pn_brick <- raster::brick(file.path(datadir, "PN_nowcast_2014present.nc"))
dap_brick <- raster::brick(file.path(datadir, "DAP_nowcast_2014present.nc"))
dac_brick <- raster::brick(file.path(datadir, "DAC_nowcast_2014present.nc"))


# Build and plot coverage
################################################################################

# Dates with data
pn_dates <- data.frame(data="Pseudo nitzschia", date=as.Date(names(pn_brick), format="X%Y.%m.%d"), stringsAsFactors = F)
dap_dates <-  data.frame(data="Particulate domoic acid", date=as.Date(names(dap_brick), format="X%Y.%m.%d"), stringsAsFactors = F)
dac_dates <-  data.frame(data="Cellular domoic acid", date=as.Date(names(dac_brick), format="X%Y.%m.%d"), stringsAsFactors = F)
data <- rbind(pn_dates, dap_dates, dac_dates)

#
dates <- seq(min(data$date), max(data$date), by="day")


pn_missing <- dates[!dates%in%pn_dates$date]
dap_missing <- dates[!dates%in%dap_dates$date]


# Data frame
date_ref <- data.frame(date=seq(min(data$date), max(data$date), by="day")) %>% 
  left_join(pn_dates, by="date") %>% 
  left_join(dap_dates, by="date") %>% 
  left_join(dac_dates, by="date") %>% 
  setNames(c("date", "pn_yn", "dap_yn", "dac_yn")) %>% 
  mutate(pn_yn=ifelse(is.na(pn_yn), "No", "Yes"),
         dap_yn=ifelse(is.na(pn_yn), "No", "Yes"),
         dac_yn=ifelse(is.na(pn_yn), "No", "Yes")) %>% 
  gather(key="dataset", value="YN", 2:4)


g <- ggplot(date_ref, aes(x=date, y=dataset, col=YN)) +
  geom_line()
g

g <- ggplot(data, aes(x=date, y=data, col=data)) +
  geom_point(size=0.5)
g


pn_stats <- raster::cellStats(pn_brick, "mean", na.rm=T)

sum(is.na(pn_stats))





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


