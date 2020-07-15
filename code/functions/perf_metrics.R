
# Measure performance
perf_metrics <- function(preds){
  acc <- accuracy(data=preds, truth="true", estimate="estimate")
  kap <- kap(data=preds, truth="true", estimate="estimate")
  roc1 <- pROC::roc(data=preds, response="true", predictor = "prob_under")
  auc <- pROC::auc(roc1)
  df <- tibble(accuracy=acc$.estimate, 
               kappa=kap$.estimate, 
               auc=as.numeric(auc))
  return(df)
}








