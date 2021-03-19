
# Fit boosted regression tree model
fit_brt <- function(data, variable){
  
  # Reduce data
  data1 <- data %>% 
    dplyr::select(over, contains(variable))

  # Define tuning parameter grid
  # fitGrid <- expand.grid(interaction.depth=c(1, 3, 5, 7, 10),
  #                        n.trees=seq(1000, 15000, 500),
  #                        shrinkage=c(0.01, 0.005, 0.001), 
  #                        n.minobsinnode=10)
  
  # Define tuning parameter grid
  fitGrid <- expand.grid(interaction.depth=c(5, 7, 12),
                         n.trees=seq(5000, 10000, 1000),
                         shrinkage=c(0.01, 0.005),
                         n.minobsinnode=10)
  
  # Define tuning and training method
  fitControl <- trainControl(method="repeatedcv", number=10, repeats=3)
  
  # Train BRT model
  brt_fit <- train(over ~ .,
                   data=data1,
                   method="gbm", 
                   bag.fraction=0.5,
                   distribution="bernoulli", 
                   metric="Kappa",
                   tuneGrid=fitGrid, trControl=fitControl, na.action=na.pass, verbose=F)
  
  # Inspect tune
  brt_tune <- brt_fit$results
  g <- ggplot(brt_tune, aes(x=n.trees, y=Kappa, color=as.factor(interaction.depth))) +
    geom_line() +
    facet_wrap(~ shrinkage) +
    scale_color_discrete(name="Tree depth") +
    labs(x="Number of trees", y="Cohen's kappa") +
    theme_bw()
  print(g)
  
  # Return
  return(brt_fit)

}