
# Function to build data for model training/testing
# data <- sdata; ptrain <- 0.8
split_data <- function(data, ptrain){
  
  
  # Step 1. Format data
  ###############################################
  
  # Format data
  data1 <- data %>% 
    # Remove samples without any C-HARM data
    filter(!is.na(pda0)) %>% 
    # Add over/under variable
    mutate(over=factor(over))
  
  message(paste(sum(is.na(data$pda0)), " without C-HARM data"))
  
  # Step 2. Divide into training/testing data
  ###############################################
  
  # Divide into training and test datasets
  set.seed(1)
  sampleids_over <- data1$sampleid[data1$over==1]
  sampleids_under <- data1$sampleid[data1$over==0]
  sampleids_over_test <- sample(sampleids_over, 0.2*length(sampleids_over))
  sampleids_under_test <- sample(sampleids_under, 0.2*length(sampleids_under))
  data1 <- data1 %>% 
    mutate(dataset=ifelse(sampleid %in% c(sampleids_over_test, sampleids_under_test), "test", "train"))
  
  # Inspect sample size in each dataset
  stats <- data1 %>% 
    group_by(dataset, over) %>% 
    summarize(n=n()) %>% 
    ungroup() %>% 
    group_by(dataset) %>% 
    mutate(p=n/sum(n))

  # Final test datasets
  data_train <- data1 %>% 
    filter(dataset=="train") %>% 
    select(over, pda0:cda30)
  data_test <- data1 %>% 
    filter(dataset=="test") %>% 
    select(over, pda0:cda30)
  
  # Step 3. Impute missing values
  ##########################################
  
  # Specify recipe
  # Impute missing values
  data_recipe <- data_train %>% 
    recipe(over ~ .) %>%
    step_knnimpute(all_predictors(), neighbors = 3)
  
  # Prep recipe
  data_recipe_prepped <- data_recipe %>% 
    prep()
  
  # Juice training data
  data_train <- data_recipe_prepped %>% 
    juice()
  
  # Bake testing data with same recipe
  data_test <- data_recipe_prepped %>% 
    bake(new_data=data_test)
  
  # Return
  data_list <- list(data_orig=data, data_train=data_train, data_test=data_test)
  return(data_list)

}


