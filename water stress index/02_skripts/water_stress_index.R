### 
WSI <- list()

### DEfine Quantiles 
q_min <- 0.15
q_max <- 0.85
method_aggregation <- "blue"

# create names table
# streamflow
for(i in 1:nrow(periods)){
  for(k in 1:3){
    dat <- tibble(month=factor(c(1:12)))
    name <- paste(periods[i,1],periods[i,2],rcp_list[k],sep="_")
    WSI[[name]] <- dat
  }
  rm(dat)
}

# streamflow
for(i in 1:nrow(periods)){
  dat <- streamflow_belp_mod_mm %>% 
    filter(year(streamflow_belp_mod_mm$date)>=periods[i,1]&
             year(streamflow_belp_mod_mm$date)<=periods[i,2])
  dat <- monthly_sums(dat,q_min = q_min, q_max = q_max, method = method_aggregation)
  
  for(k in 1:3){
    dat_2 <- dat[[k]][,c(2:4)]
    colnames(dat_2) <- c("sfm_median","sfm_q25","sfm_q75")
    nr <- (i - 1) * 3 + k
    print(paste(
      periods[i,1],
      periods[i,2],
      names(dat)[k],
      nr,
      sep = "; "
    ))
    WSI[[nr]] <- cbind(WSI[[nr]],dat_2)
  }
  rm(dat)
}

# ## Irrigation
for(i in 1:nrow(periods)){
  dat <- irrigation_need_CTM %>%
    filter(year(irrigation_need_CTM$date)>=periods[i,1]&
             year(irrigation_need_CTM$date)<=periods[i,2])
  dat <- monthly_sums(dat,q_min = q_min, q_max = q_max, method = method_aggregation)
  
  for(k in 1:3){
    dat_2 <- dat[[k]][,c(2:4)]
    #print(dat_2)
    colnames(dat_2) <- c("irr_median","irr_q25","irr_q75")
    nr <- (i - 1) * 3 + k
    print(paste(
      periods[i,1],
      periods[i,2],
      names(dat)[k],
      nr,
      sep = "; "
    ))
    WSI[[nr]] <- cbind(WSI[[nr]],dat_2)
  }
  rm(dat)
}



## one scenario data (drinking, livestock, industrial)
for(i in 1:3){
  dat <- monthly_sums_simple(water,periods = periods)[[i]][,c(2:4)]
  for(k in 1:3){
    nr <- (i - 1) * 3 + k
    print(nr)
    WSI[[nr]] <- cbind(WSI[[nr]],dat)
  }
}


### Ecology 
for(i in 1:9){
  WSI[[i]]$ecology <- as.vector(c(q347_monthly_mm[1,],q347_monthly_mm[2,],q347_monthly_mm[3,]))[i]
}

q347_monthly_mm


## homogenize sf values for first (theoretically observed) period

for(i in 1:3){
  WSI[[i]]$sfm_median <- (WSI[[1]]$sfm_median+WSI[[2]]$sfm_median+WSI[[3]]$sfm_median)/3
  WSI[[i]]$sfm_q25 <- (WSI[[1]]$sfm_q25+WSI[[2]]$sfm_q25+WSI[[3]]$sfm_q25)/3
  WSI[[i]]$sfm_q75 <- (WSI[[1]]$sfm_q75+WSI[[2]]$sfm_q75+WSI[[3]]$sfm_q75)/3
} 

for(i in 1:3){
  WSI[[i]]$irr_median <- (WSI[[1]]$irr_median+WSI[[2]]$irr_median+WSI[[3]]$irr_median)/3
  WSI[[i]]$irr_q25 <- (WSI[[1]]$irr_q25+WSI[[2]]$irr_q25+WSI[[3]]$irr_q25)/3
  WSI[[i]]$irr_q75 <- (WSI[[1]]$irr_q75+WSI[[2]]$irr_q75+WSI[[3]]$irr_q75)/3
} 






### Water Stress index 
for(i in 1:9){
  WSI[[i]]$need_wo_eco <- 
    WSI[[i]]$irr_median + WSI[[i]]$drinking + WSI[[i]]$industrial + 
    WSI[[i]]$livestock_monthly
  
  WSI[[i]]$need_eco <- 
    WSI[[i]]$irr_median + WSI[[i]]$drinking + WSI[[i]]$industrial + 
    WSI[[i]]$livestock_monthly + WSI[[i]]$ecology
  
  WSI[[i]]$need_eco_max <- 
    WSI[[i]]$irr_q75 + WSI[[i]]$drinking + WSI[[i]]$industrial + 
    WSI[[i]]$livestock_monthly + WSI[[i]]$ecology
  
  WSI[[i]]$need_eco_min <- 
    WSI[[i]]$irr_q25 + WSI[[i]]$drinking + WSI[[i]]$industrial + 
    WSI[[i]]$livestock_monthly + WSI[[i]]$ecology
  
  WSI[[i]]$need_wo_eco_max <- 
    WSI[[i]]$irr_q75 + WSI[[i]]$drinking + WSI[[i]]$industrial + 
    WSI[[i]]$livestock_monthly
  
  WSI[[i]]$need_wo_eco_min <- 
    WSI[[i]]$irr_q25 + WSI[[i]]$drinking + WSI[[i]]$industrial + 
    WSI[[i]]$livestock_monthly 
}

for(i in 1:9){
  WSI[[i]]$WSI_eco <- WSI[[i]]$need_eco / WSI[[i]]$sfm_median
  WSI[[i]]$WSI_wo_eco <- WSI[[i]]$need_wo_eco/WSI[[i]]$sfm_median
}



annual_wsi <- vector()
## Annual Water Stress Levels
for(i in 1:9){
  annual_wsi[[i]] <- mean(WSI[[i]]$WSI_eco)
  names(annual_wsi)[i] <- names(WSI)[i]
}
annual_wsi <- round(annual_wsi,2)


rm(dat,dat_2)
