
evaluate_models <- function(models, model_names, test_data){
  
  # Steps
  # 1. Make predictions
  # 2. Calculate performance metrics
  # 3. Derive ROC curves
  
  # Step 1. Make predictions
  preds <- purrr::map_df(1:length(models), function(x) {
    # Make predictions
    mpreds <- make_predictions(model=models[[x]], dataset=test_data) %>% 
      mutate(model=model_names[x]) %>% 
      dplyr::select(model, everything())
  })
  
  
  # Step 2. Calculate performance metrics
  pmetrics <- purrr::map_df(model_names, function(x){
    # Performance metrics
    mperf <- preds %>% 
      filter(model==x) %>% 
      perf_metrics(.) %>% 
      mutate(model=x) %>% 
      dplyr::select(model, everything())
  }) %>% 
    arrange(desc(auc))
  
  # Step 3. Derive ROC curve
  roc_curves <- purrr::map_df(model_names, function(x) {
    # ROC
    mroc <- preds %>% 
      filter(model==x) %>% 
      derive_roc(preds_df=., model_name=x, plot=F)
  })


  # Plot ROC curves
  #####################################

  # Setup theme
  my_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=9),
                    plot.title=element_text(size=11),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))

  # Plot data
  g <- ggplot(roc_curves, aes(x=fpr, y=tpr, color=model)) +
    geom_line() +
    geom_abline(slope=1, intercept=0, col="black", linetype="dotted") +
    labs(x="False positive rate (1-specificity)", y="True positive rate (sensitivity)") +
    scale_color_discrete(name="Method") +
    theme_bw() + my_theme
  print(g)
  
  # Retrun stuff
  #####################################
  
  output <- list(preds=preds, pmetrics=pmetrics, roc_curves=roc_curves)
  return(output)
  
}
