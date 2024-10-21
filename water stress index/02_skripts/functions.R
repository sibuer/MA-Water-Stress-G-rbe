library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(zoo)
# wsi number to text 

wsi_to_text <- function(x){
  if(x>=1){
    return("Water Availability Exceeded")
  }  else if(x>=0.8){
    return("Severe Stress")
  } else if (x<0.8&x>=0.4) {
    return("High Stress")
  } else if  (x<0.4 & x>=0.2){
    return("Moderate Stress")
  } else if (x<0.2 & x>0.1){
    return("Low Stress")
  } else {
    return("No Stress")
  }
}


###### new periods functions 

monthly_sums <- function(x,q_min=0.25,q_max=0.75,method="orange"){
  res <- list()
  for(i in 1:3){
    
    sub <- x[,grep(names(x),pattern =rcp_list[i])]          # only for one rcp scenario
    
    res_sub <- matrix(nrow = 12,ncol=4)                     # create result matrix
    
    for(k in 1:12){
      sub_sub <- sub[as.numeric(month(x$date))==k,]         # only 1 month 
      years <- as.numeric(lubridate::year(x$date[as.numeric(lubridate::month(x$date))==k])) # years vector to aggregate 
      sub_sub <- as.data.frame(apply(sub_sub,MARGIN = 2,
                                     function(y)aggregate(y,list(years),
                                                          function(y)sum(y,na.rm = T))[2])) # aggregate to monthly sums for each model and year (column) seperately


      
      if(method=="blue"){

        
        # Flatten all model values (i.e., stack them) to compute the statistics across all models
        all_model_values <- as.vector(as.matrix(sub_sub))
        
        # Calculate median and quantiles across all model values
        res_vec <- c(k, NA, NA, NA)  # Results vector
        res_vec[2] <- median(all_model_values, na.rm = TRUE)  # Median of all model values
        res_vec[3] <- quantile(all_model_values, q_min, na.rm = TRUE)  # 25th percentile
        res_vec[4] <- quantile(all_model_values, q_max, na.rm = TRUE)  # 75th percentile
        
        res_sub[k,] <- res_vec
        
      } else if(method=="orange"){
        res_vec <- c(k,rep(NA,3)) # results vector
        
        res_vec[2] <- apply(sub_sub, MARGIN = 2, function(y)median(y)) %>% as.vector() %>% median() # median of monthly sums for each model, then mean
        res_vec[3] <- apply(sub_sub, MARGIN = 2, function(y)quantile(y,q_min))  %>% as.vector() %>% median() # q25 of monthly sums for each model, then mean
        res_vec[4] <- apply(sub_sub, MARGIN = 2, function(y)quantile(y,q_max)) %>% as.vector() %>% median() # q75 of monthly sums for each model, then mean
        
        res_sub[k,] <- res_vec
      
      } else {
        print("choose method")
      }
      
    }
    name <- rcp_list[i]
    colnames(res_sub)<-c("month","median","q_min","q_max")                          # save to results list
    res_sub <- as_tibble(res_sub)
    res[[name]] <- res_sub
  }
  return(res)
}












yearly_sums <- function(x, periods, q_min = 0.25, q_max = 0.75) {
  res <- list()
  
  for (i in 1:3) {
    
    sub <- x[, grep(names(x), pattern = rcp_list[i])]  # Filter data for one RCP scenario
    
    res_sub <- matrix(nrow = nrow(periods), ncol = 4)  # Create result matrix for each period
    
    for (p in 1:nrow(periods)) {
      # Subset data for the current period based on start and end year
      period_data <- sub[lubridate::year(x$date) >= periods$start[p] & lubridate::year(x$date) <= periods$end[p], ]
      
      # Sum data for each year within the period for each model
      period_sum <- as.data.frame(apply(period_data, MARGIN = 2, function(y) sum(y, na.rm = TRUE)))
      
      length_p <- periods$end[p] - periods$start[p]
      
      # Flatten all model values (i.e., stack them) to compute the statistics across all models
      all_model_values <- as.vector(as.matrix(period_sum))
      
      # Calculate median and quantiles across all model values for the current period
      res_vec <- c(paste(periods$start[p], periods$end[p], sep = "-"), NA, NA, NA)  # Results vector
      res_vec[2] <- median(all_model_values, na.rm = TRUE)/length_p  # Median of all model values
      res_vec[3] <- quantile(all_model_values, q_min, na.rm = TRUE) /length_p # q_min (e.g., 25th percentile)
      res_vec[4] <- quantile(all_model_values, q_max, na.rm = TRUE)/length_p  # q_max (e.g., 75th percentile)
      
      res_sub[p, ] <- res_vec  # Store the results for the current period
    }
    
    name <- rcp_list[i]
    colnames(res_sub) <- c("period", "median", "q_min", "q_max")  # Set column names
    res_sub <- as_tibble(res_sub)
    
    # s
    
  
    res[[name]] <- res_sub  # Save to the results list
  }
  
  return(res)
}





# monthly sums for multiple single col-data frames

monthly_sums_simple <- function(x,periods){
  res <- list()
  for(i in 1:nrow(periods)){
    dat <- x %>% filter(x$year>=periods[i,1]&x$year<=periods[i,2])
    res_df <- tibble(month=c(1:12))
    for(k in 2:ncol(x)){
      res_df[[as.character(names(dat)[k])]] <- dat[,k] %>% mean()
    }
    res[[paste(periods[i,1],periods[i,2],sep = "_")]] <- res_df
  }
  return(res)
}




get_meteorological_season <- function(month) {
  if (any(!is.numeric(month)) || any(month < 1 | month > 12)) {
    stop("All elements in month must be numeric values between 1 and 12.")
  }
  
  seasons <- character(length(month))
  seasons[month >= 3 & month <= 5] <- "MAM"
  seasons[month >= 6 & month <= 8] <- "JJA"
  seasons[month >= 9 & month <= 11] <- "SON"
  seasons[month == 12 | month == 1 | month == 2] <- "DJF"
  
  return(seasons)
}



assign_period <- function(year, periods) {
  # Ensure the input year is numeric
  year <- as.numeric(year)
  
  # Loop through each period and find the matching period for the input year
  for (i in 1:nrow(periods)) {
    start_period <- periods[i, "start"]
    end_period <- periods[i, "end"]
    
    # If the year falls within the period, return the period name
    if (year >= start_period & year <= end_period) {
      return(paste0(start_period, "-", end_period))
    }
  }
  
  # If the year doesn't match any period, return NA or some error message
  return(NA)  # Or you could return "No matching period" or another default message
}





aggregate_yearly_means <- function(x){
  rcp_list_nice <- c("RCP 2.6","RCP 4.5","RCP 8.5")
  
  pl_df <- matrix(ncol=3,nrow = 0)
  for(i in 1:3){
    # subset period
    pl_sub <- x[,grep(rcp_list[i],colnames(x))]
    
    pl_sub <- aggregate(pl_sub[,2:ncol(pl_sub)],
                        by=list(format(ymd(x$date), "%Y")),
                        FUN=function(x)mean(x,na.rm=T))
    # Rename the grouping column to "month" for clarity
    names(pl_sub)[1] <- "year"
    # Step 2: pivot all columns to a longer format
    pl_sub <- pivot_longer(pl_sub,
                           cols = -year, # Exclude the "month" column
                           names_to = "variable",
                           values_to = "value")
    pl_sub <- pl_sub %>% select(-variable)
    # add rcp & period column
    pl_sub$rcp <- rcp_list_nice[i]
    # make sure colnames align
    colnames(pl_df) <- colnames(pl_sub)
    # add to end of result df
    pl_df <- rbind(pl_df,pl_sub)
  }
  
  pl_df$period <- sapply(pl_df$year, function(x)assign_period(x,periods = periods)) %>% as.vector()
  pl_df <- pl_df[!is.na(pl_df$period),]
  
  pl_df <- pl_df[order(pl_df$year), ]
  
  return(pl_df)
}


aggregate_year_month_means <- function(x){
  rcp_list_nice <- c("RCP 2.6","RCP 4.5","RCP 8.5")
  
  pl_df <- matrix(ncol=3,nrow = 0)
  for(i in 1:3){
    # subset period
    pl_sub <- x[,grep(rcp_list[i],colnames(x))]
    
    pl_sub <- aggregate(pl_sub[,2:ncol(pl_sub)],
                        by=list(format(ymd(x$date), "%Y_%m")),
                        FUN=function(x)mean(x,na.rm=T))
    # Rename the grouping column to "month" for clarity
    names(pl_sub)[1] <- "year"
    # Step 2: pivot all columns to a longer format
    pl_sub <- pivot_longer(pl_sub,
                           cols = -year, # Exclude the "month" column
                           names_to = "variable",
                           values_to = "value")
    pl_sub <- pl_sub %>% select(-variable)
    # add rcp & period column
    pl_sub$rcp <- rcp_list_nice[i]
    # make sure colnames align
    colnames(pl_df) <- colnames(pl_sub)
    # add to end of result df
    pl_df <- rbind(pl_df,pl_sub)
  }
  
  # add period
  pl_df$period <- sapply(as.numeric(substring(pl_df$year,1,4)), function(x)assign_period(x,periods = periods)) %>% as.vector()
  pl_df <- pl_df[!is.na(pl_df$period),]
  
  # add month
  pl_df$month <- as.numeric(substring(pl_df$year,6,7)) %>% as.vector()
  pl_df$month <- factor(pl_df$month, 
                     levels = 1:12, 
                     labels = month.abb)
  
  # add season
  pl_df$season <- sapply(as.numeric(substring(pl_df$year,6,7)), function(x)get_meteorological_season(x)) %>% as.vector()
  
  pl_df <- pl_df[order(pl_df$year), ]
  
  return(pl_df)
}

aggregate_yearmonthly_means(hurs_BER)

### variables that are always needed
periods <- data.frame(start=c(1990,2020,2045),end=c(2019,2049,2047))
rcp_list <- c("RCP26", "RCP45", "RCP85")
rcp_list_nice <-  c("RCP 2.6", "RCP 4.5", "RCP 8.5")



