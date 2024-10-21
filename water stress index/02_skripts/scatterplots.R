scatterplot_dataframe <- function(wsi = "total") {
  
  # Initialize an empty result dataframe
  df_res_total <- data.frame(month = vector(), irr = vector(), sf = vector(), rcp = vector(), per = vector(), season = vector())
  
  # Precompute WSI index adjustments to avoid recalculating within loops
  wsi_adjustment <- ifelse(wsi == "total", 1, ifelse(wsi == "without_eco", 2, 3))
  
  # Preprocess the date column once to avoid repeated conversions
  irrigation_dates <- irrigation_need_CTM$date
  
  # Loop through all RCPs
  for (rcp in seq_along(rcp_list)) {
    
    # Filter the streamflow data by RCP once
    sf_sub <- streamflow_belp_mod_mm[, grep(colnames(streamflow_belp_mod_mm), pattern = rcp_list[rcp])]
    colnames(sf_sub) <- gsub("\\.", "_", colnames(sf_sub))  # Rename columns with underscore
    
    # Loop through all periods
    for (period in seq_len(nrow(periods))) {
      
      # Filter the period once
      in_period <- year(irrigation_dates) >= periods[period, 1] & year(irrigation_dates) <= periods[period, 2]
      sf_sub_period <- sf_sub[in_period, ]
      date_vector_period <- irrigation_dates[in_period]
      
      # Initialize an empty temporary dataframe for each period
      df_res <- data.frame(month = vector(), irr = vector(), sf = vector())
      
      # Loop through the columns of the streamflow subset
      for (i in seq_len(ncol(sf_sub_period))) {
        ir_sub <- irrigation_need_CTM[, grep(colnames(sf_sub_period)[i], colnames(irrigation_need_CTM))]
        
        if (length(ir_sub) >= 1) {
          ir_sub_period <- ir_sub[in_period]
          
          # Aggregate both irrigation and streamflow in a single step
          agg <- aggregate(ir_sub_period,
                           list(format(ymd(date_vector_period), "%Y_%m")),
                           function(x) {
                             if (sum(is.na(x)) > 3) {
                               return(NA)
                             } else {
                               return(sum(x, na.rm = TRUE))
                             }
                           })
          
          agg <- agg[complete.cases(agg), ]  # Remove rows with NA values
          
          # Aggregate the streamflow data
          agg$sf <- aggregate(sf_sub_period[, i],
                              list(format(ymd(date_vector_period), "%Y_%m")),
                              function(x) sum(x, na.rm = TRUE))[, 2]
          
          # Rename the columns of the aggregated dataframe
          colnames(agg) <- c("month", "irr", "sf")
          
          # Append the result to the temporary dataframe
          df_res <- rbind(df_res, agg)
        }
      }
      
      # Order by month and compute the period for each year
      df_res <- df_res[order(df_res$month), ]
      df_res$per <- sapply(as.numeric(substring(df_res$month, 1, 4)), function(x) assign_period(x, periods = periods)) %>% as.vector()
      
      # Pre-calculate WSI demand based on the specified scenario
      for (i in 1:12) {
        if (wsi == "total") {
          df_res$dem[as.numeric(substring(df_res$month, 6, 7)) == i] <- 
            WSI[[(period - 1) * 3 + rcp]]$industrial[i] +
            WSI[[(period - 1) * 3 + rcp]]$drinking[i] +
            WSI[[(period - 1) * 3 + rcp]]$livestock_monthly[i] +
            WSI[[(period - 1) * 3 + rcp]]$ecology[i]
        } else if (wsi == "without_eco") {
          df_res$dem[as.numeric(substring(df_res$month, 6, 7)) == i] <- 
            WSI[[(period - 1) * 3 + rcp]]$industrial[i] +
            WSI[[(period - 1) * 3 + rcp]]$drinking[i] +
            WSI[[(period - 1) * 3 + rcp]]$livestock_monthly[i]
        } else if (wsi == "surface") {
          df_res$dem[as.numeric(substring(df_res$month, 6, 7)) == i] <- 
            WSI[[(period - 1) * 3 + rcp]]$ecology[i]
        }
      }
      
      # Add irrigation demand to the total demand
      df_res$dem <- df_res$dem + df_res$irr
      df_res$irr <- NULL  # Remove the irrigation column
      
      # Calculate the Water Scarcity Index (WSI)
      df_res$wsi <- df_res$dem / df_res$sf
      
      # Add RCP scenario to the dataframe
      df_res$rcp <- rcp_list[rcp]
      
      # Determine the season based on the month
      df_res$season <- sapply(as.numeric(substring(df_res$month, 6, 7)), get_season)
      df_res$month <- NULL  # Remove the month column
      
      # Append to the total result dataframe
      df_res_total <- rbind(df_res_total, df_res)
    }
  }
  
  return(df_res_total)
}


df_plot <- scatterplot_dataframe(wsi="total")
df_plot_wo_eco <- scatterplot_dataframe(wsi="without_eco")
df_plot_surface <- scatterplot_dataframe(wsi="surface")

df_plot <- df_plot[df_plot$per=="2045-2074",]
df_plot_wo_eco <- df_plot_wo_eco[df_plot_wo_eco$per=="2045-2074",]
df_plot_surface <- df_plot_surface[df_plot_surface$per=="2045-2074",]


df_plot_original <- df_plot
df_plot_wo_eco_original <- df_plot_wo_eco
df_plot_surface_original <- df_plot_surface

