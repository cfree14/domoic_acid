fit_glm <- function(data, variable, lat=F){
  
  # Reduce data
  data1 <- data %>% 
    dplyr::select(over, lat_dd, contains(variable))
  
  # Remove latitude if not including
  if(lat==F){
    data1 <- data1 %>% 
      dplyr::select(-lat_dd)
  }

  # Fit model  
  glm_fit <- logistic_reg(mode = "classification") %>%
    set_engine("glm") %>%
    fit(over ~ ., 
        data=data1)
  
  # Return fit
  return(glm_fit)
  
}