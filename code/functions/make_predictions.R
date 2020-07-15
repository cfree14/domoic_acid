# Helper functions
################################################################################

# Make predictions
# model <- rf_fit; dataset <- data_test
make_predictions <- function(model, dataset){
  
  # Measure performance (Parsnip object)
  if(length(names(model))==4){
    preds_fac <- predict(model, new_data=dataset)
    preds_prob <- predict(model, new_data=dataset, type="prob")
    preds <- dataset %>%
      # True class
      dplyr::select(over) %>% 
      rename(true=over) %>% 
      # Add class prediction
      bind_cols(preds_fac) %>% 
      rename(estimate=.pred_class) %>% 
      mutate(estimate=as.factor(estimate)) %>% 
      # Add class probabilities
      bind_cols(preds_prob) %>% 
      rename(prob_under=.pred_0, prob_over=.pred_1)
  }
  
  # Measure performance (Caret object)
  if(length(names(model))>4){
    preds_fac <- predict(model, newdata=dataset)
    preds_prob <-predict(model, newdata=dataset, type="prob")
    preds <- dataset %>%
      # True class
      dplyr::select(over) %>% 
      rename(true=over) %>% 
      # Add class prediction
      mutate(estimate=preds_fac) %>% 
      mutate(estimate=as.factor(estimate)) %>% 
      # Add class probabilities
      bind_cols(preds_prob) %>% 
      rename(prob_under='0', prob_over='1')
  }
  
  # Return
  return(preds)
  
}

