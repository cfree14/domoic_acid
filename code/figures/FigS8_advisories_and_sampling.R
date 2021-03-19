
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "figures"
datadir <- "data/closures/data"
inputdir <- "input"

# Read data
closures <- readRDS(file.path(datadir, "CDFW_2015_2020_fishery_closures.Rds"))
advisories <- readRDS(file.path(datadir, "CDPH_2014_2020_health_advisories.Rds"))

# Read county lines
counties <- readxl::read_excel(file.path(datadir, "CA_county_lines.xlsx"))

# Read monitoring data
data_orig <- readRDS(file.path(inputdir, "CDPH_crab_bivalve_domoic_acid_data_use.Rds"))


# Format data
################################################################################

# Study species
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", "Clams", "Mussels")
spp_do1 <- c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam", "Mussels")

# Format advisories
################################

# Add factor
advisories <- advisories %>% 
  # Reduce to species of interest
  filter(comm_name %in% spp_do) %>% 
  # TEMPORARY: Reclass clams as razor clams
  mutate(comm_name=recode(comm_name, "Clams"="Razor clam")) %>% 
  # Add factors
  mutate(comm_name=factor(comm_name, levels=spp_do1), 
         advisory=recode_factor(advisory, 
                                "Out-of-season"="Out-of-Season",
                                "None"="No advisory",
                                "Partial"="Viscera advisory",
                                "Full"="Meat advisory"))

# Sample
advisories_sample <- sample_frac(advisories, size=0.1)

# Format monitoring
################################

# Build surveys
surv_results <- data_orig %>% 
  # Eliminate surveys with unknown date
  filter(year>2000) %>% 
  # Add survey id
  mutate(surveyid=paste(comm_name, area, sample_date, sep="-")) %>% 
  # Calculate survey statistics
  group_by(comm_name, region, port, area, sample_date, surveyid) %>% 
  summarize(n=n(),
            nover=sum(over),
            pover=nover/n, 
            status=ifelse(pover==0, "Clean", 
                          ifelse(pover<0.5, "<50% contaminated", "≥50% contaminated")),
            status=factor(status, levels=c("Clean", "<50% contaminated", "≥50% contaminated")),
            anyover=ifelse(nover>0, 0, 1) %>% as.factor(),
            lat_dd=mean(lat_dd_use),
            across(.cols=cda0:cda30, .fns=mean)) %>% 
  ungroup() %>% 
  # Reduce to complete surveys
  filter(n>=5) %>% 
  # Factor common name
  mutate(comm_name=factor(comm_name, levels=c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam")))

# Format for merge
surv_results_use <- surv_results %>% 
  mutate(result_type="Survey", 
         result=recode(status, "Clean"="None", "<50% contaminated"="Partial", "≥50% contaminated"="Risky")) %>% 
  select(comm_name, sample_date, lat_dd, result_type, result) %>% 
  rename(date=sample_date)

# Identify individual results
indiv_results <- data_orig %>% 
  # Eliminate surveys with unknown date
  filter(year>2000) %>% 
  # Add survey id
  mutate(surveyid=paste(comm_name, area, sample_date, sep="-")) %>% 
  # Reduce to survey ids not described above
  filter(!surveyid %in% surv_results$surveyid) %>% 
  # Reduce to mussels of interest
  filter(type!="cultured") %>% 
  mutate(comm_name=ifelse(comm_name %in% c("Sea mussel", "Bay mussel"), "Mussels", comm_name)) %>% 
  # Add new columns
  mutate(result_type="Individual", 
         da_ppm_thresh=ifelse(comm_name=="Dungeness crab", 30, 20),
         result=ifelse(da_ppm==2.5, "None", 
                       ifelse(da_ppm<da_ppm_thresh, "Partial", "Risky"))) %>% 
  # Reduce to relevant columns
  select(comm_name, sample_date, lat_dd, result_type, result) %>% 
  rename(date=sample_date)
  
# Merge
monitoring_results <- bind_rows(surv_results_use, indiv_results) %>% 
  mutate(comm_name=factor(comm_name, levels=c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam", "Mussels")),
         result_type=factor(result_type, levels=c("Survey", "Individual")), 
         result=factor(result, levels=c("None", "Partial", "Risky")))

# Inspect
freeR::complete(monitoring_results)
table(monitoring_results$comm_name)

# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=5),
                  legend.title=element_text(size=7),
                  strip.text = element_text(size=8),
                  plot.title=element_blank(), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))
  
# Plot closure grid
g <- ggplot(advisories, aes(x=date, y=lat_dd, fill=advisory)) +
  # Facet by species
  facet_wrap(~comm_name, ncol=2, scales="free_x") +
  # Plot raster
  geom_raster() +
  # Plot survey results
  geom_point(data=monitoring_results, mapping=aes(x=date, y=lat_dd, color=result, shape=result_type), inherit.aes=F, size=0.5) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=33:42) +
  # Labels
  labs(x="", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Advisory status", values=c("grey90", "grey70", "coral", "darkred")) +
  scale_shape_manual(name="Result type", values=c(16, 4)) +
  scale_color_manual(name="Contamination", values=c("white", "grey40", "black")) +
  guides(fill = guide_legend(order = 1),
         shape = guide_legend(order = 2),
         color = guide_legend(order = 3)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.75, 0.2),
        legend.direction = "vertical", 
        legend.box = "horizontal")
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS8_advisories_vs_sampling.png"), 
       width=6.5, height=6.5, units="in", dpi=600)







