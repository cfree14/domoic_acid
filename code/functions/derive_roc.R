# Function to format ROC data
derive_roc <- function(preds_df, model_name, plot=F){
  
  # Derive ROC
  roc1 <- pROC::roc(data=preds_df, response="true", predictor = "prob_under")
  if(plot){print(plot(roc1))}
  roc_auc <- pROC::auc(roc1)
  message(paste("AUC =", roc_auc))
  
  # Format ROC
  tpr <- roc1$sensitivities # true positive rate (sensitivity)
  fpr <- 1-roc1$specificities # false positive rate (1 - specificity)
  df <- tibble(model=model_name, tpr=tpr, fpr=fpr) %>%
    arrange(tpr)
  
  # Return ROC
  return(df)
  
}