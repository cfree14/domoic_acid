
fit_rf <- function(data, variable){
  
  # Reduce data
  data1 <- data %>% 
    dplyr::select(over, contains(variable))
  
  # Define tuning parameter grid
  # mtry = Number of variables randomly sampled as candidate variables
  fitGrid <- expand.grid(mtry=seq(10, 20, 2))
  
  # Define tuning and training method
  fitControl <- caret::trainControl(method="repeatedcv", number=10, repeats=10)
  
  # Train RF model
  rf_fit <- caret::train(over ~ .,
                         data=data1,
                         method="rf", 
                         distribution="bernoulli", 
                         metric="Kappa",
                         tuneGrid=fitGrid, trControl=fitControl, na.action=na.pass, verbose=F)
  
  # Plot RF tuning results
  rf_fit$bestTune
  rf_tune <- rf_fit$results
  g <- ggplot(rf_tune, aes(x=mtry, y=Kappa)) +
    labs(x="Number of variables\nsampled at each split" , y="Cohen's kappa", main="Random forest model tune") +
    geom_line() +
    geom_point() +
    theme_bw()
  print(g)
  
  # Return fit
  return(rf_fit)
  
}

