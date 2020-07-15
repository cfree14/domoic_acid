

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

# Outline steps
# 1. Build data (link CDPH and C-HARM data)
# 2. Divide into training/testing data
# 3. Impute missing values
# 4. Train models
# 5. Inspect model performance

# Setup
################################################################################

# Steps
# 1. Read data
# 2. Build data
# 3. Split data
# 4. Fit models
# 5. Evaluate models

# 1. Read data
###################################

data_orig <- readRDS(file.path(outputdir, "dungeness_crab_data.Rds"))


# 2. Build data
###################################

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


# 3. Split data
###################################

set.seed(14)
ptrain <- 0.8
train_ids <- sample(x=data$surveyid, size=nrow(data)*ptrain)
data <- data %>% 
  mutate(dataset=ifelse(surveyid %in% train_ids, "training", "testing")) %>% 
  select(region:nover, dataset, everything())
data_test <- data %>% filter(dataset=="testing")
data_train <- data %>% filter(dataset=="training")


# 4. Fit models
###################################

# GLM model
glmfit <- glm(pover ~ lat_dd + cda0 + cda1 + cda2 + cda3 + cda4 + cda5 + cda6 + cda7 + cda8 + cda9 + cda10 + cda11 + cda12 + cda13 + cda14 + cda15 +
                cda16 + cda17 + cda18 + cda19 + cda20 + cda21 + cda22 + cda23 + cda24 + cda25 + cda26 + cda27 + cda28 + cda29 + cda30, 
              data = data_train, family="quasibinomial")
plot(glmfit)

# Random forest model
fitGrid <- expand.grid(mtry=seq(10, 20, 5))
fitControl <- caret::trainControl(method="repeatedcv", number=10, repeats=10)
rf_fit <- caret::train(pover ~ lat_dd + cda0 + cda1 + cda2 + cda3 + cda4 + cda5 + cda6 + cda7 + cda8 + cda9 + cda10 + cda11 + cda12 + cda13 + cda14 + cda15 +
                         cda16 + cda17 + cda18 + cda19 + cda20 + cda21 + cda22 + cda23 + cda24 + cda25 + cda26 + cda27 + cda28 + cda29 + cda30,
                       data=data_train,
                       method="rf", 
                       distribution="quasibinomial", 
                       metric="RMSE",
                       tuneGrid=fitGrid, trControl=fitControl, na.action=na.pass, verbose=F)


# GLM hurdle model
glm_hurdle1 <- glm(anyover ~ lat_dd + cda0 + cda1 + cda2 + cda3 + cda4 + cda5 + cda6 + cda7 + cda8 + cda9 + cda10 + cda11 + cda12 + cda13 + cda14 + cda15 +
                           cda16 + cda17 + cda18 + cda19 + cda20 + cda21 + cda22 + cda23 + cda24 + cda25 + cda26 + cda27 + cda28 + cda29 + cda30, 
                         data = data_train, family="binomial")
plot(glm_hurdle1)

glm_hurdle2 <- glm(pover ~ lat_dd + cda0 + cda1 + cda2 + cda3 + cda4 + cda5 + cda6 + cda7 + cda8 + cda9 + cda10 + cda11 + cda12 + cda13 + cda14 + cda15 +
                 cda16 + cda17 + cda18 + cda19 + cda20 + cda21 + cda22 + cda23 + cda24 + cda25 + cda26 + cda27 + cda28 + cda29 + cda30, 
               data = data_train, subset=nover>0, family="quasibinomial")
plot(glm_hurdle2)

# RF hurdle model
rf_hurdle1 <- caret::train(anyover ~ lat_dd + cda0 + cda1 + cda2 + cda3 + cda4 + cda5 + cda6 + cda7 + cda8 + cda9 + cda10 + cda11 + cda12 + cda13 + cda14 + cda15 +
                           cda16 + cda17 + cda18 + cda19 + cda20 + cda21 + cda22 + cda23 + cda24 + cda25 + cda26 + cda27 + cda28 + cda29 + cda30,
                           data=data_train,
                           method="rf", 
                           distribution="bernoulli", 
                           metric="Kappa",
                           tuneGrid=fitGrid, trControl=fitControl, na.action=na.pass, verbose=F)

rf_hurdle2 <- caret::train(pover ~ lat_dd + cda0 + cda1 + cda2 + cda3 + cda4 + cda5 + cda6 + cda7 + cda8 + cda9 + cda10 + cda11 + cda12 + cda13 + cda14 + cda15 +
                             cda16 + cda17 + cda18 + cda19 + cda20 + cda21 + cda22 + cda23 + cda24 + cda25 + cda26 + cda27 + cda28 + cda29 + cda30,
                           data=data_train %>% filter(nover>0),
                           method="rf", 
                           distribution="quasibinomial", 
                           metric="RMSE",
                           tuneGrid=fitGrid, trControl=fitControl, na.action=na.pass, verbose=F)


# 4. Compare model
###################################

# Make predictions
preds_glm <- predict(glmfit, newdata=data_test, type="response")
preds_rf <- predict(rf_fit, newdata=data_test)
preds_glm_hurdle1 <-  predict(glm_hurdle1, newdata=data_test, type="response")
preds_glm_hurdle2 <-  predict(glm_hurdle2, newdata=data_test, type="response")
preds_rf_hurdle1 <-  predict(rf_hurdle1, newdata=data_test, type="prob")
preds_rf_hurdle2 <-  predict(rf_hurdle2, newdata=data_test, type="raw")

# Add predictions to test data 
data_preds <- data_test %>% 
  mutate(pover_glm=preds_glm,
         pover_rf=preds_rf,
         glm_hurdle1=preds_glm_hurdle1,
         glm_hurdle2=preds_glm_hurdle2, 
         glm_hurdle=ifelse(glm_hurdle1<0.5, 0, glm_hurdle2),
         rf_hurdle1=preds_rf_hurdle1[,2],
         rf_hurdle2=preds_rf_hurdle2, 
         rf_hurdle=ifelse(rf_hurdle1==0, 0, rf_hurdle2))

# Plot 
plot(rf_hurdle1 ~ pover, data_preds)
abline(v=0.5)
abline(h=0.5)







