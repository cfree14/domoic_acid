
# Function to build data for model training/testing
# species="Dungeness crab"; lag=30; action_ppm=30
build_data <- function(species, predictor, lag, action_ppm){
  
  # Format CDPH toxin data
  comm_name_do <- species
  sdata <- data %>%
    # Filter to species of interest
    filter(comm_name==comm_name_do) %>% 
    rename(sample_date=date) %>% 
    # Samples above action threshold
    mutate(over=ifelse(da_ppm >= action_ppm, 1, 0))
  
  # Loop through samples and build time series of DA exposure
  x <- sdata$sampleid[1]
  df <- purrr::map_df(sdata$sampleid, function(x) {
    
    # Sample date
    sample_date <- sdata$sample_date[sdata$sampleid==x]
    first_date <- ymd(sample_date) - lag
    
    # Sample coordinates
    xy <- sdata %>% 
      filter(sampleid==x) %>% 
      select(long_dd_use, lat_dd_use) %>% 
      as.matrix()
    
    # Extract DAP for sample up to X days before sample date
    dap_vals <- raster::extract(x=dap_brick, y=xy, df = TRUE) %>% 
      gather(key="date", value="dap_risk", 2:ncol(.)) %>% 
      mutate(sampleid=x,
             date=ymd(gsub("X", "", date))) %>% 
      select(sampleid, date, dap_risk) %>% 
      # Reduce to days before sample date
      filter(date<=sample_date & date>=first_date)
    
    # Extract DAC for sample up to X days before sample date
    dac_vals <- raster::extract(x=dac_brick, y=xy, df = TRUE) %>% 
      gather(key="date", value="dac_risk", 2:ncol(.)) %>% 
      mutate(sampleid=x,
             date=ymd(gsub("X", "", date))) %>% 
      select(sampleid, date, dac_risk) %>% 
      # Reduce to days before sample date
      filter(date<=sample_date & date>=first_date)
    
    # Merge DAP and DAC values
    da_vals <- dap_vals %>% 
      left_join(dac_vals, by=c("sampleid", "date"))
    
  })
  
  # Inspect sample size to make sure everything < 31
  df_n <- df %>% 
    group_by(sampleid) %>% 
    summarize(n=n())
  if(any(df_n$n>(lag+1))){stop("Something is wrong.")}
  
  # Spread long-to-wide
  df_wide <- df %>% 
    # Record lag
    group_by(sampleid) %>% 
    mutate(day=n():1-1) %>% 
    select(-date) %>% 
    # Convert to long
    select(sampleid, day, everything()) %>% 
    gather(key="variable", value="risk", 3:4) %>% 
    mutate(variable=recode(variable, 
                           "dap_risk"="pda",
                           "dac_risk"="cda")) %>% 
    mutate(predictor=paste(variable, day, sep="")) %>% 
    select(-c(variable, day)) %>% 
    # Convert to wide
    spread(key="predictor", value="risk") %>% 
    # Arrange columns
    select(sampleid, paste0("pda", 0:30), paste0("cda", 0:30))
    
  # Build results
  results <- sdata %>% 
    left_join(df_wide, by="sampleid")

  # Return results
  return(results)
  
}