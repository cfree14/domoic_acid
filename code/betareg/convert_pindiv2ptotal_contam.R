

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(caret)
library(raster)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(betareg)

# Directories
datadir <- "models/contamination/paper/data"
cpdhdir <- "data/da_sampling/processed"
charmdir <- "data/charm/processed"
codedir <- "models/contamination/paper/code/functions"
outputdir <- "models/contamination/paper/output"

# Source helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))



# Build survey data
################################################################################

# Read individual data
data_orig <- readRDS(file.path(outputdir, "dungeness_crab_data.Rds"))

# Build survey data
data <- data_orig %>% 
  # Eliminate surveys with unknown date
  filter(year>2000) %>% 
  # Add survey id
  mutate(surveyid=paste(area, sample_date, sep="-")) %>% 
  # Calculate survey statistics
  group_by(region, port, area, sample_date, surveyid) %>% 
  summarize(n=n(),
            nover=sum(over),
            pover=nover/n, 
            anyover=ifelse(nover>0, 0, 1) %>% as.factor(),
            lat_dd=mean(lat_dd_use),
            across(.cols=cda0:cda30, .fns=mean)) %>% 
  ungroup() %>% 
  # Reduce to complete surveys
  filter(n>=6)
  
# Inspect
anyDuplicated(data$surveyid)
hist(data$n)
hist(data$pover)
freeR::complete(data)


# Use best model to predict p(individual) at survey locations
################################################################################

# Read model
model_do <- "dungeness_crab_model_rf_cda.Rds"
model_obj_do <-readRDS(file.path(outputdir, model_do))

# Make predictions
preds <- predict(model_obj_do, newdata=data, type="prob")

# Add predictions to observations
data_wpreds <- data %>% 
  mutate(pover_pred=preds[,2],
         resid=pover - pover_pred)

# Plot predictions against observations
lmfit <- lm(pover ~ pover_pred, data_wpreds)
plot(pover ~ pover_pred, data_wpreds, bty="n", xlim=c(0,1), ylim=c(0,1))
abline(a=0, b=1)
abline(lmfit, col="red")


g <- ggplot(data_wpreds, aes(x=pover_pred, y=pover)) +
  geom_point() +
  geom_smooth(method="gam") +
  labs(y="p(contaminated)\n(observed)", x="p(contaminated)\n(predicted)") +
  geom_text(x=0, y=1, hjust=0, label="Prediction too low", size=5) +
  geom_text(x=1, y=0, hjust=1, label="Prediction too high", size=5) +
  geom_abline(slope=1) +
  theme_bw()
g


g <- ggplot(data_wpreds, aes(x=pover_pred, y=resid)) +
  geom_point() +
  geom_smooth(method="gam") +
  # labs(y="p(contaminated)\n(observed / bias corrected)", x="p(contaminated)\n(predicted)") +
  geom_hline(yintercept=0) +
  theme_bw()
g

