fit_glm <- function(data, variable){
  
  # Reduce data
  data1 <- data %>% 
    dplyr::select(over, contains(variable))

  # Fit model  
  glm_fit <- logistic_reg(mode = "classification") %>%
    set_engine("glm") %>%
    fit(over ~ ., 
        data=data1)
  
  # Return fit
  return(glm_fit)
  
}