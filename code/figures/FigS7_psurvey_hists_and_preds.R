

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)
library(lubridate)

# Packages
library(tidyverse)
library(lubridate)

# Directories
inputdir <- "input"
cpdhdir <- "data/da_sampling/processed"
charmdir <- "data/charm/processed"
codedir <- "code/functions"
modeldir <- "output/candidate_models"
plotdir <- "figures"

# Source helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Study species
spp_do <- c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam")


# Build survey data
################################################################################

# Read individual data
data_orig <- readRDS(file.path(inputdir, "CDPH_crab_bivalve_domoic_acid_data_use.Rds"))

# Build survey data
data <- data_orig %>% 
  # Eliminate surveys with unknown date
  filter(year>2000) %>% 
  # Add survey id
  mutate(surveyid=paste(comm_name, area, sample_date, sep="-")) %>% 
  # Calculate survey statistics
  group_by(comm_name, region, port, area, sample_date, surveyid) %>% 
  summarize(n=n(),
            nover=sum(over),
            pover=nover/n, 
            anyover=ifelse(nover>0, 0, 1) %>% as.factor(),
            lat_dd=mean(lat_dd_use),
            across(.cols=cda0:cda30, .fns=mean)) %>% 
  ungroup() %>% 
  # Reduce to complete surveys
  filter(n>=5) %>% 
  # Factor species
  mutate(comm_name=factor(comm_name, spp_do))

# Inspect
anyDuplicated(data$surveyid)
hist(data$n)
hist(data$pover)
freeR::complete(data)

# Use best model to predict p(individual) at survey locations
################################################################################

# Species with survey data
spp_do <- sort(unique(data$comm_name))
best_model_key <- tibble(comm_name=spp_do, 
                         model=c("dungeness_crab_model_rf_cda.Rds", 
                                 "razor_clam_model_rf_cda.Rds",
                                 "rock_crab_model_brt_cda.Rds",
                                 "spiny_lobster_model_rf_cda.Rds"))

# Loop through species
x <- spp_do[4]
data_wpreds <- purrr::map_df(spp_do, function(x) {
  
  # Subset species
  spp <- x
  sdata <- data %>% 
    filter(comm_name==spp)
  
  # Read model
  model_do <- best_model_key %>% filter(comm_name==x) %>% pull(model)
  model_obj_do <-readRDS(file.path(modeldir, model_do))
  
  # Make predictions
  preds <- predict(model_obj_do, newdata=sdata, type="prob")
  
  # Add predictions to observations
  sdata_wpreds <- sdata %>% 
    mutate(pover_pred=preds[,2],
           resid=pover - pover_pred)
  
})


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  plot.title=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot histograms
################################

# Build stats
spp_key <- data %>% 
  group_by(comm_name) %>% 
  summarize(n=n(),
            ymax= max(hist(pover, breaks=seq(0, 1, 0.1), plot=F)$count, right=T) ) %>% 
  mutate(n_label=paste0(n, " surveys")) %>% 
  mutate(closure_trigger=ifelse(comm_name=='Dungeness crab', 0.5, NA))

# Plot results for all species
g1 <- ggplot(data, aes(x=pover)) +
  facet_wrap(~comm_name, scale="free_y", nrow=1) +
  geom_histogram(breaks=seq(0,1,0.1)) +
  # Add closure trigger
  geom_vline(spp_key, mapping=aes(xintercept=closure_trigger), linetype="dotted") +
  # Add sample size
  geom_text(spp_key, mapping=aes(x=1, y=ymax, label=n_label), hjust=1, size=2.5) +
  # Labels
  labs(x="Proportion of individuals contaminated\nabove the domoic acid action level", y="  \nNumber of surveys") +
  # Theme
  theme_bw() + my_theme
g1

# Plot predictions
################################

# Format data for plotting
data_wpreds <- data_wpreds %>% 
  mutate(comm_name=factor(comm_name, c("Dungeness crab", "Rock crab", "Spiny lobster", "Razor clam")))

# Build stats
stats <- data_wpreds %>% 
  group_by(comm_name) %>% 
  summarize(nsurveys=n(),
            r=cor(x=pover, y=pover_pred),
            label=paste0("r=", format(r, nsmall=2, digits=2)))


# Plot data
g2 <- ggplot(data_wpreds, aes(x=pover, y=pover_pred)) +
  # Plot ribbon
  geom_smooth(method="lm", col="black", fill="grey80") +
  # Plot points
  facet_wrap(~comm_name, ncol=4) +
  geom_point() + 
  # Plot reference line
  geom_abline(slope=1, linetype="dotted") +
  # Plot text
  geom_text(data=stats, mapping=aes(x=0, y=1, label=label), hjust=0, size=2.5) +
  # Limits
  lims(x=c(0,1), y=c(0,1)) +
  # Labels
  labs( x="Observed\nP(of individuals contaminated)", 
        y="Predicted\np(an individual is contaminated)") +
  # Theme
  theme_bw() + my_theme
g2


# Merge and export
################################

# Merge
g <- grid.arrange(g1, g2, nrow=2)

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS7_psurvey_hists_and_preds.png"), 
       width=6.5, height=4.75, units="in", dpi=600)






  
