
# Function to predict DA contamination for a given species, model, and date
# date <- ymd("2015-11-15")
predict_da_contam <- function(date, model, model_obj, plot){
  
  # Id predictor variables
  date <- ymd(date)
  day0 <- date
  day30 <- day0 - 30
  dates_needed <- seq(day30, day0, by="day")
  layers_needed <- dates_needed %>% gsub("-", ".", .) %>% paste0("X", .)
  
  # Build key
  var <- ifelse(grepl("cda", model_do), "cda", "pda")
  varnames_needed <- paste0(var, 0:30)
  key <- tibble(date=dates_needed, layer=layers_needed, var_name=varnames_needed)
  
  # Build dataset
  if(var=="cda"){
    # Build predictor dataset
    input <- dac_brick[[layers_needed]] %>% 
      # Convert to dataframe
      as.data.frame(xy=T) %>% 
      # Gather variables
      gather(key="layer", value="risk", 3:ncol(.)) %>% 
      # Remove NA variables
      filter(!is.na(risk)) %>% 
      # Add variable name
      left_join(key %>% select(layer, var_name), by="layer") %>% 
      # Spread variables
      select(-layer) %>% 
      spread(key="var_name", value="risk")
  }else{
    # Build predictor dataset
    input <- dap_brick[[layers_needed]] %>% 
      # Convert to dataframe
      as.data.frame(xy=T) %>% 
      # Gather variables
      gather(key="layer", value="risk", 3:ncol(.)) %>% 
      # Remove NA variables
      filter(!is.na(risk)) %>% 
      # Add variable name
      left_join(key %>% select(layer, var_name), by="layer") %>% 
      # Spread variables
      select(-layer) %>% 
      spread(key="var_name", value="risk")
  }

  # # Add missing predictors to list
  # pred_missing <- setdiff(varnames_needed, colnames(input))
  # if(length(pred_missing)>0){
  #   for(i in 1:length(pred_missing)){
  #     input[,pred_missing[i]] <- 0.1 # HUGE HACK RIGHT HERE
  #   }
  # }
  
  # Arrange
  input <- input %>% 
    select(x, y, varnames_needed)
  
  # Make predictions
  preds <- predict(object=model_obj, newdata=input, type="prob")
  
  # Add predictions to data
  pdata <- input %>% 
    na.omit() %>% 
    select(x, y) %>% 
    cbind(preds) %>% 
    rename(long_dd=x, lat_dd=y, pcontam="1") %>% 
    select(-c("0")) %>% 
    mutate(date=date) %>% 
    select(date, everything())
  
  # Plot predictions
  if(plot==T){
    g <- ggplot(pdata, aes(x=long_dd, y=lat_dd, fill=pcontam)) +
      geom_raster() +
      labs(x="", y="", title=date) +
      scale_fill_gradientn(name="p(contaminated)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      theme_bw()
    print(g)
  }
  
  # Return
  return(pdata)
  
}

