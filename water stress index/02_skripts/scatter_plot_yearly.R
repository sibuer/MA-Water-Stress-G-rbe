scatterplot_dataframe <- function() {
  
  # Create empty result dataframe
  df_res_total <- data.frame(month=vector(), irr=vector(), sf=vector(), rcp=vector(), per=vector(), season=vector())
  
  # Loop through all RCPs
  for (rcp in 1:length(rcp_list)) {
    
    # Loop through all periods
    for (period in 1:nrow(periods)) {
      
      # get the streamflow subset for the current RCP
      sf_sub <- streamflow_belp_mod_mm[, grep(colnames(streamflow_belp_mod_mm), pattern = rcp_list[rcp])]
      colnames(sf_sub) <- gsub("\\.", "_", colnames(sf_sub))
      
      # Filter for the current period
      date_vector <- irrigation_need_CTM$date
      true_false_vector_period <- year(date_vector) >= periods[period, 1] & year(date_vector) <= periods[period, 2]
      sf_sub <- sf_sub[true_false_vector_period, ]
      date_vector <- date_vector[true_false_vector_period]
      
      # Create a temporary result dataframe
      df_res <- data.frame(month=vector(), irr=vector(), sf=vector())
      
      # Get matching irrigation values for the same models
      for (i in 1:ncol(sf_sub)) {
        ir_sub <- irrigation_need_CTM[, grep(colnames(sf_sub)[i], colnames(irrigation_need_CTM))]
        
        # Ensure matching irrigation subset is found
        if (length(ir_sub) >= 1) {
          ir_sub <- ir_sub[true_false_vector_period]
          
          # Aggregation to yearly sums
          agg <- aggregate(ir_sub,
                           list(format(ymd(date_vector), "%Y")),
                           function(x) {
                             # Count NAs in each year and skip years with more than 10 NA values
                             if (sum(is.na(x)) > 10) {
                               return(NA) # Set year to NA if more than 10 NA values
                             } else {
                               return(sum(x, na.rm=TRUE))
                             }
                           })
          
          # Remove rows with NA values (from above condition)
          agg <- agg[complete.cases(agg), ]
          
          # Streamflow aggregation
          agg$sf <- aggregate(sf_sub[, i],
                              list(format(ymd(date_vector), "%Y")),
                              function(x) sum(x, na.rm=TRUE))[, 2]
          
          # Check for low streamflow sums
          if (any(agg$sf < 1)) {
            warning(paste("Low streamflow detected in column", colnames(sf_sub)[i]))
          }
          
          # Rename columns
          colnames(agg) <- c("year", "irr", "sf")
          
          # Append to the temporary result dataframe
          df_res <- rbind(df_res, agg)
        } else {
          next
        }
        
        # Order the results by year
        df_res <- df_res[order(df_res$year), ]
      }
      
      # Add period column
      df_res$per <- sapply(as.numeric(df_res$year), function(x) assign_period(x, periods = periods)) %>% as.vector()
      
      # Add rest of demand from WSI lists
      df_res$dem <- 
        sum(WSI[[(period - 1) * 3 + rcp]]$industrial) +
        sum(WSI[[(period - 1) * 3 + rcp]]$drinking) +
        sum(WSI[[(period - 1) * 3 + rcp]]$livestock_monthly)
      
      # Add demand and irrigation demand
      df_res$dem <- df_res$dem + df_res$irr
      df_res$irr <- NULL
      
      # Calculate WSI (Water Scarcity Index)
      df_res$wsi <- df_res$dem / df_res$sf
      
      # Add RCP scenario to the dataframe
      df_res$rcp <- rcp_list[rcp]
      
      # Append to the total result dataframe
      df_res_total <- rbind(df_res_total, df_res)
    }
  }
  
  return(df_res_total)
}

df_plot_yearly <- scatterplot_dataframe()





df_plot_yearly <- df_plot_yearly[df_plot_yearly$per=="2045-2074",]



# Step 1: Categorize WSI into discrete levels
df_plot_yearly$wsi_category <- cut(df_plot_yearly$wsi, 
                                   breaks = c(0, 0.1, 0.2, 0.4, 0.8, 1), 
                                   labels = c("No Stress", "Low Stress", "Moderate Stress", "High Stress", "Severe Stress"),
                                   include.lowest = TRUE)


# Step 1: Categorize WSI into discrete levels
df_plot_yearly$wsi_category <- cut(df_plot_yearly$wsi, 
                                   breaks = c(0, 0.1, 0.2, 0.4, 0.8, 1), 
                                   labels = c("No Stress", "Low Stress", "Moderate Stress", "High Stress", "Severe Stress"),
                                   include.lowest = TRUE)

# Step 2: Calculate the number of points for each combination of 'per' and 'rcp'
df_plot_yearly_counts <- df_plot_yearly %>%
  group_by(rcp) %>%
  summarise(n = n()) %>%
  ungroup()

# Step 3: Plot with discrete fill based on the categorized WSI levels
ggplot(data = df_plot_yearly) +
  geom_point(aes(y = dem, x = sf, fill = wsi_category),  # Use 'fill' for the discrete categories
             shape = 21,         # Use shape 21 for points with both fill and outline
             color = "black",     # Black outline
             size = 1.5,         # Point size
             stroke = 0.5) +     # Thickness of the outline
  scale_fill_manual(
    values = c("No Stress" = "#fcf6f0", 
               "Low Stress" = "#ffffb7", 
               "Moderate Stress" = "#FEB24C", 
               "High Stress" = "#FC4E2A", 
               "Severe Stress" = "#b10026"),  # Discrete colors for each WSI category
    name = "WSI Level"  # Name of the variable in the legend
  ) +
  facet_grid(per ~ rcp, scales = "fixed") +  # Create a facet grid with period as rows and rcp as columns
  ggtitle("annual WSI values Gürbe-Mühlimatt") +
  xlab("Streamflow [mm]") +
  ylab("Water Demand [mm]") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill="white", color = "black"),
    panel.background = element_rect(fill="transparent", color="darkgrey")
  ) +
  # Step 4: Add the count labels for each facet using geom_text
  geom_text(data = df_plot_yearly_counts, aes(x = Inf, y = Inf, label = paste("n =", n)),
            hjust = 1.1, vjust = 1.1, size = 3, inherit.aes = FALSE)


ggsave("01_graphs/scatterplot_yearly_wsi_no_ecology.png",width = 12, height = 4)






scatterplot_dataframe <- function() {
  
  # Create empty result dataframe
  df_res_total <- data.frame(month=vector(), irr=vector(), sf=vector(), rcp=vector(), per=vector(), season=vector())
  
  # Loop through all RCPs
  for (rcp in 1:length(rcp_list)) {
    
    # Loop through all periods
    for (period in 1:nrow(periods)) {
      
      # get the streamflow subset for the current RCP
      sf_sub <- streamflow_belp_mod_mm[, grep(colnames(streamflow_belp_mod_mm), pattern = rcp_list[rcp])]
      colnames(sf_sub) <- gsub("\\.", "_", colnames(sf_sub))
      
      # Filter for the current period
      date_vector <- irrigation_need_CTM$date
      true_false_vector_period <- year(date_vector) >= periods[period, 1] & year(date_vector) <= periods[period, 2]
      sf_sub <- sf_sub[true_false_vector_period, ]
      date_vector <- date_vector[true_false_vector_period]
      
      # Create a temporary result dataframe
      df_res <- data.frame(month=vector(), irr=vector(), sf=vector())
      
      # Get matching irrigation values for the same models
      for (i in 1:ncol(sf_sub)) {
        ir_sub <- irrigation_need_CTM[, grep(colnames(sf_sub)[i], colnames(irrigation_need_CTM))]
        
        # Ensure matching irrigation subset is found
        if (length(ir_sub) >= 1) {
          ir_sub <- ir_sub[true_false_vector_period]
          
          # Aggregation to yearly sums
          agg <- aggregate(ir_sub,
                           list(format(ymd(date_vector), "%Y")),
                           function(x) {
                             # Count NAs in each year and skip years with more than 10 NA values
                             if (sum(is.na(x)) > 10) {
                               return(NA) # Set year to NA if more than 10 NA values
                             } else {
                               return(sum(x, na.rm=TRUE))
                             }
                           })
          
          # Remove rows with NA values (from above condition)
          agg <- agg[complete.cases(agg), ]
          
          # Streamflow aggregation
          agg$sf <- aggregate(sf_sub[, i],
                              list(format(ymd(date_vector), "%Y")),
                              function(x) sum(x, na.rm=TRUE))[, 2]
          
          # Check for low streamflow sums
          if (any(agg$sf < 1)) {
            warning(paste("Low streamflow detected in column", colnames(sf_sub)[i]))
          }
          
          # Rename columns
          colnames(agg) <- c("year", "irr", "sf")
          
          # Append to the temporary result dataframe
          df_res <- rbind(df_res, agg)
        } else {
          next
        }
        
        # Order the results by year
        df_res <- df_res[order(df_res$year), ]
      }
      
      # Add period column
      df_res$per <- sapply(as.numeric(df_res$year), function(x) assign_period(x, periods = periods)) %>% as.vector()
      
      # Add rest of demand from WSI lists
      df_res$dem <- 
        sum(WSI[[(period - 1) * 3 + rcp]]$industrial) +
        sum(WSI[[(period - 1) * 3 + rcp]]$drinking) +
        sum(WSI[[(period - 1) * 3 + rcp]]$ecology) +
        sum(WSI[[(period - 1) * 3 + rcp]]$livestock_monthly)
      
      # Add demand and irrigation demand
      df_res$dem <- df_res$dem + df_res$irr
      df_res$irr <- NULL
      
      # Calculate WSI (Water Scarcity Index)
      df_res$wsi <- df_res$dem / df_res$sf
      
      # Add RCP scenario to the dataframe
      df_res$rcp <- rcp_list[rcp]
      
      # Append to the total result dataframe
      df_res_total <- rbind(df_res_total, df_res)
    }
  }
  
  return(df_res_total)
}

df_plot_yearly_with_eco <- scatterplot_dataframe()


scatterplot_dataframe <- function() {
  
  # Create empty result dataframe
  df_res_total <- data.frame(month=vector(), irr=vector(), sf=vector(), rcp=vector(), per=vector(), season=vector())
  
  # Loop through all RCPs
  for (rcp in 1:length(rcp_list)) {
    
    # Loop through all periods
    for (period in 1:nrow(periods)) {
      
      # get the streamflow subset for the current RCP
      sf_sub <- streamflow_belp_mod_mm[, grep(colnames(streamflow_belp_mod_mm), pattern = rcp_list[rcp])]
      colnames(sf_sub) <- gsub("\\.", "_", colnames(sf_sub))
      
      # Filter for the current period
      date_vector <- irrigation_need_CTM$date
      true_false_vector_period <- year(date_vector) >= periods[period, 1] & year(date_vector) <= periods[period, 2]
      sf_sub <- sf_sub[true_false_vector_period, ]
      date_vector <- date_vector[true_false_vector_period]
      
      # Create a temporary result dataframe
      df_res <- data.frame(month=vector(), irr=vector(), sf=vector())
      
      # Get matching irrigation values for the same models
      for (i in 1:ncol(sf_sub)) {
        ir_sub <- irrigation_need_CTM[, grep(colnames(sf_sub)[i], colnames(irrigation_need_CTM))]
        
        # Ensure matching irrigation subset is found
        if (length(ir_sub) >= 1) {
          ir_sub <- ir_sub[true_false_vector_period]
          
          # Aggregation to yearly sums
          agg <- aggregate(ir_sub,
                           list(format(ymd(date_vector), "%Y")),
                           function(x) {
                             # Count NAs in each year and skip years with more than 10 NA values
                             if (sum(is.na(x)) > 10) {
                               return(NA) # Set year to NA if more than 10 NA values
                             } else {
                               return(sum(x, na.rm=TRUE))
                             }
                           })
          
          # Remove rows with NA values (from above condition)
          agg <- agg[complete.cases(agg), ]
          
          # Streamflow aggregation
          agg$sf <- aggregate(sf_sub[, i],
                              list(format(ymd(date_vector), "%Y")),
                              function(x) sum(x, na.rm=TRUE))[, 2]
          
          # Check for low streamflow sums
          if (any(agg$sf < 1)) {
            warning(paste("Low streamflow detected in column", colnames(sf_sub)[i]))
          }
          
          # Rename columns
          colnames(agg) <- c("year", "irr", "sf")
          
          # Append to the temporary result dataframe
          df_res <- rbind(df_res, agg)
        } else {
          next
        }
        
        # Order the results by year
        df_res <- df_res[order(df_res$year), ]
      }
      
      # Add period column
      df_res$per <- sapply(as.numeric(df_res$year), function(x) assign_period(x, periods = periods)) %>% as.vector()
      
      # Add rest of demand from WSI lists
      df_res$dem <- 
        sum(WSI[[(period - 1) * 3 + rcp]]$industrial) +
        sum(WSI[[(period - 1) * 3 + rcp]]$drinking) +
        sum(WSI[[(period - 1) * 3 + rcp]]$livestock_monthly)
      
      # Add demand and irrigation demand
      df_res$dem <- df_res$dem + df_res$irr
      df_res$irr <- NULL
      
      # Calculate WSI (Water Scarcity Index)
      df_res$wsi <- df_res$dem / df_res$sf
      
      # Add RCP scenario to the dataframe
      df_res$rcp <- rcp_list[rcp]
      
      # Append to the total result dataframe
      df_res_total <- rbind(df_res_total, df_res)
    }
  }
  
  return(df_res_total)
}

df_plot_yearly_without_eco <- scatterplot_dataframe()


















df_plot <- df_plot_yearly_with_eco
# Define bin breaks
df_plot <- df_plot[df_plot$per=="2045-2074",]
df_plot_yearly_without_eco <- df_plot_yearly_without_eco[df_plot_yearly_without_eco$per=="2045-2074",]

bins_breaks <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)

# Cap WSI values at 1.02
df_plot$wsi_capped <- pmin(df_plot$wsi, 1.02)
df_plot$wsi_capped_wo_eco <- pmin(df_plot_yearly_without_eco$wsi, 1.02)

# Compute histogram counts for wsi_capped
wsi_counts <- df_plot %>%
  group_by(rcp) %>%
  mutate(bin = cut(wsi_capped, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(rcp) %>%
  mutate(density = n / sum(n))

# Compute histogram counts for wsi_capped_wo_eco
wsi_counts_wo_eco <- df_plot %>%
  group_by(rcp) %>%
  mutate(bin = cut(wsi_capped_wo_eco, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(rcp) %>%
  mutate(density = n / sum(n))

# Define bin center positions for consistent bar width
bin_centers <- (bins_breaks[-length(bins_breaks)] + bins_breaks[-1]) / 2
bin_width <- 0.1

# Plot using geom_col to ensure uniform bar widths
ggplot() +
  geom_col(data = wsi_counts, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, color = "grey", alpha = .2, position = "dodge") +
  geom_col(data = wsi_counts_wo_eco, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, alpha = .65, position = position_dodge(width = 0.1)) +
  
  
  facet_wrap(~rcp) +
  
  # Titles and labels
  ggtitle("Distribution of yearly water stress index (Gürbe catchment)") +
  xlab("Water Stress Index (WSI)") +
  ylab("relative frequency [%]") +  # Relative frequency on the y-axis
  theme_minimal() + 
  
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10, expand = c(0.01, 0.01)) +
  
  theme(
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(0.3, "cm"),
    plot.background = element_rect(fill="white",color = "black", linewidth = 0.1)
  ) +
  
  annotate(geom="segment",y = 0, yend=1, x=1, xend=1, linetype="dashed", color="black") +
  annotate(geom="segment",y = 0, yend=1, x=0.8, xend=0.8, linetype="solid", color="black", linewidth = 0.09) + 
  annotate(geom="segment",y = 0, yend=1, x=0.4, xend=0.4, linetype="solid", color="black", linewidth = 0.09) +
  annotate(geom="segment",y = 0, yend=1, x=0.2, xend=0.2, linetype="solid", color="black", linewidth = 0.09) +
  annotate(geom="segment",y = 0, yend=1, x=0.1, xend=0.1, linetype="solid", color="black", linewidth = 0.09) +
  
  scale_fill_manual(values = c("RCP26" = "#91bfdb", "RCP45" = "#ffd700", "RCP85" = "#fc8d59"))

ggsave("01_graphs/bars_yearly.png", width = 12, height = 4)


