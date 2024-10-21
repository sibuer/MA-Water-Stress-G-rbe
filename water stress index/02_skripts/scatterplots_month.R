# Function to assign meteorological seasons using DJF, MAM, JJA, SON
get_season <- function(month) {
  ifelse(month %in% c(12, 1, 2), "DJF",
         ifelse(month %in% c(3, 4, 5), "MAM",
                ifelse(month %in% c(6, 7, 8), "JJA", "SON")))
}

# Function to assign periods based on year
assign_period <- function(year_val, periods) {
  for (i in 1:nrow(periods)) {
    if (year_val >= periods[i, 1] && year_val <= periods[i, 2]) {
      return(paste(periods[i, 1], "-", periods[i, 2]))
    }
  }
  return(NA)  # Return NA if the year doesn't fall into any period (edge cases)
}










# # Create a new column that caps wsi values at 1 for the color scale
# df_plot$wsi_capped <- pmin(df_plot$wsi, 1)
# 
# # Plot the data using ggplot with facet grid
# ggplot(data = df_plot) +
#   geom_point(aes(y = dem, x = sf, fill = wsi_capped),  # Use 'fill' for the inside color
#              shape = 21,         # Use shape 21 for points with both fill and outline
#              color = "black",     # Black outline
#              size = 1.5,            # Point size
#              stroke = 0.5) +      # Thickness of the outline
#   scale_fill_gradientn(
#     colors = c("#fcf6f0", "#ffffb7", "#FEB24C", "#FC4E2A", "#b10026"),  # Define your color gradient
#     values = rescale(c(0, 0.5, 1)),  # Transition smoothly from 0 to 1
#     limits = c(0, 1),  # Set limits to 0-1 for the color scale
#     name = "wsi"       # Keep the name of the actual variable in the legend
#   ) +
#   facet_grid(per ~ rcp, scales = "fixed") +  # Create a facet grid with period as rows and rcp as columns
#   ggtitle("Scatterplot WSI") +
#   xlab("Streamflow [mm]") +
#   ylab("Water Demand [mm]") +
#   theme_minimal()+
#   theme(
#     plot.background = element_rect(fill="white",color = "black"),
#     panel.background = element_rect(fill="transparent",color="black")
#   )
# 
# ggsave("01_graphs/scatterplots_wsi_eco.png",height = 7, width = 7)
# 
# 
# library(scales)  # Needed for rescale function
# 
# # Let's print the summary of wsi_capped to ensure values are capped correctly
# summary(df_plot$wsi_capped)
# 
# # Now plotting the histograms
# # Apply the background color scale across the panel




bins_breaks <- seq(0.0, 1.1, by = 0.1)

df_plot$wsi_capped <- pmin(df_plot$wsi, 1.02)
df_plot$wsi_capped_wo_eco <- pmin(df_plot_wo_eco$wsi,1.02)

ggplot(df_plot)+
  
  geom_histogram(data=df_plot, aes(x=wsi_capped,y=..density..,fill=rcp), color="grey",
                 alpha=.15, position = "dodge",breaks = bins_breaks)+
  geom_histogram(aes(x=wsi_capped_wo_eco,y=..density..,fill=rcp),
                 alpha=.65, breaks = bins_breaks,
                 position = position_dodge(width = 0.1)) +
  
  facet_wrap(~per)+
  # Titles and labels
  ggtitle("Distribution of monthly water stress indexes (Gürbe catchment)") +
  xlab("Water Stress Index (WSI)") +
  ylab("Density") +  # Relative frequency on the y-axis
  theme_minimal() + 
  
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 10, expand = c(0.01, 0.01))+
  
  theme(
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(0.3, "cm"),
    plot.background = element_rect(fill="white",color = "black", linewidth = 0.1)
  )+
  
  annotate(geom="segment",y = 0,yend=8,x=1,xend=1, linetype="dashed",color="black")+
  annotate(geom="segment",y = 0,yend=8,x=0.8,xend=0.8, linetype="solid",color="black",linewidth = 0.09)+ 
  annotate(geom="segment",y = 0,yend=8,x=0.4,xend=0.4, linetype="solid",color="black",linewidth = 0.09)+
  annotate(geom="segment",y = 0,yend=8,x=0.2,xend=0.2, linetype="solid",color="black",linewidth = 0.09)+
  annotate(geom="segment",y = 0,yend=8,x=0.1,xend=0.1, linetype="solid",color="black",linewidth = 0.09)+
  
  scale_fill_manual(values = c("RCP26" = "#91bfdb", "RCP45" = "#ffd700", "RCP85" = "#fc8d59"))
                               
ggsave("01_graphs/histo_wsi.png", width = 12, height = 4)



# Define bin breaks
bins_breaks <- c(0.0, 0.1, 0.2, 0.4, 0.8, 1, 1.1)

# Cap WSI values at 1.02
df_plot$wsi_capped <- pmin(df_plot$wsi, 1.02)
df_plot$wsi_capped_wo_eco <- pmin(df_plot_wo_eco$wsi, 1.02)

# Compute histogram counts for wsi_capped
wsi_counts <- df_plot %>%
  group_by(per, rcp) %>%
  mutate(bin = cut(wsi_capped, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(per, rcp) %>%
  mutate(density = n / sum(n))

# Compute histogram counts for wsi_capped_wo_eco
wsi_counts_wo_eco <- df_plot %>%
  group_by(per, rcp) %>%
  mutate(bin = cut(wsi_capped_wo_eco, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(per, rcp) %>%
  mutate(density = n / sum(n))

# Define bin center positions for consistent bar width
bin_centers <- (bins_breaks[-length(bins_breaks)] + bins_breaks[-1]) / 2
bin_width <- 0.1

# Plot using geom_col to ensure uniform bar widths
ggplot() +
  geom_col(data = wsi_counts, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, color = "grey", alpha = .15, position = "dodge") +
  geom_col(data = wsi_counts_wo_eco, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, alpha = .65, position = position_dodge(width = 0.1)) +
  
  facet_wrap(~per) +
  
  # Titles and labels
  ggtitle("Distribution of monthly water stress indexes (Gürbe catchment)") +
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

ggsave("01_graphs/bars_lvl_wsi.png", width = 12, height = 4)




df_plot_summer <-  df_plot %>% filter(season=="JJA")



# Compute histogram counts for wsi_capped
wsi_counts <- df_plot_summer %>%
  group_by(per, rcp) %>%
  mutate(bin = cut(wsi_capped, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(per, rcp) %>%
  mutate(density = n / sum(n))

# Compute histogram counts for wsi_capped_wo_eco
wsi_counts_wo_eco <- df_plot_summer %>%
  group_by(per, rcp) %>%
  mutate(bin = cut(wsi_capped_wo_eco, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(per, rcp) %>%
  mutate(density = n / sum(n))

# Define bin center positions for consistent bar width
bin_centers <- (bins_breaks[-length(bins_breaks)] + bins_breaks[-1]) / 2
bin_width <- 0.1

# Plot using geom_col to ensure uniform bar widths
ggplot() +
  geom_col(data = wsi_counts, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, color = "grey", alpha = .15, position = "dodge") +
  geom_col(data = wsi_counts_wo_eco, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, alpha = .65, position = position_dodge(width = 0.1)) +
  
  facet_wrap(~per) +
  
  # Titles and labels
  ggtitle("Distribution of monthly water stress indexes (season:JJA; Gürbe catchment)") +
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

ggsave("01_graphs/bars_lvl_wsi_JJA.png", width = 12, height = 4)




# Define bin breaks
bins_breaks <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)

# Cap WSI values at 1.02
df_plot$wsi_capped <- pmin(df_plot$wsi, 1.02)
df_plot$wsi_capped_wo_eco <- pmin(df_plot_wo_eco$wsi, 1.02)

# Compute histogram counts for wsi_capped
wsi_counts <- df_plot %>%
  group_by(per, rcp) %>%
  mutate(bin = cut(wsi_capped, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(per, rcp) %>%
  mutate(density = n / sum(n))

# Compute histogram counts for wsi_capped_wo_eco
wsi_counts_wo_eco <- df_plot %>%
  group_by(per, rcp) %>%
  mutate(bin = cut(wsi_capped_wo_eco, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(per, rcp) %>%
  mutate(density = n / sum(n))

# Define bin center positions for consistent bar width
bin_centers <- (bins_breaks[-length(bins_breaks)] + bins_breaks[-1]) / 2
bin_width <- 0.1

# Plot using geom_col to ensure uniform bar widths
ggplot() +
  geom_col(data = wsi_counts, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, color = "grey", alpha = .15, position = "dodge") +
  geom_col(data = wsi_counts_wo_eco, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, alpha = .65, position = position_dodge(width = 0.1)) +
  
  facet_wrap(~per) +
  
  # Titles and labels
  ggtitle("Distribution of monthly water stress indexes (Gürbe catchment)") +
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

ggsave("01_graphs/bars_lvl_wsi_bins.png", width = 12, height = 4)




df_plot_summer <-  df_plot %>% filter(season=="JJA")



# Compute histogram counts for wsi_capped
wsi_counts <- df_plot_summer %>%
  group_by(per, rcp) %>%
  mutate(bin = cut(wsi_capped, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(per, rcp) %>%
  mutate(density = n / sum(n))

# Compute histogram counts for wsi_capped_wo_eco
wsi_counts_wo_eco <- df_plot_summer %>%
  group_by(per, rcp) %>%
  mutate(bin = cut(wsi_capped_wo_eco, breaks = bins_breaks, include.lowest = TRUE)) %>%
  count(bin) %>%
  ungroup() %>%
  group_by(per, rcp) %>%
  mutate(density = n / sum(n))

# Define bin center positions for consistent bar width
bin_centers <- (bins_breaks[-length(bins_breaks)] + bins_breaks[-1]) / 2
bin_width <- 0.1

# Plot using geom_col to ensure uniform bar widths
ggplot() +
  geom_col(data = wsi_counts, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, color = "grey", alpha = .15, position = "dodge") +
  geom_col(data = wsi_counts_wo_eco, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, alpha = .65, position = position_dodge(width = 0.1)) +
  
  facet_wrap(~per) +
  
  # Titles and labels
  ggtitle("Distribution of monthly water stress indexes (season:JJA; Gürbe catchment)") +
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

ggsave("01_graphs/bars_lvl_wsi_JJA_bins.png", width = 12, height = 4)

## plot summer

ggplot(df_plot_summer)+
  
  geom_histogram(data=df_plot_summer, aes(x=wsi_capped,y=..density..,fill=rcp), color="grey",
                 alpha=.15, position = "dodge",breaks = bins_breaks)+
  geom_histogram(aes(x=wsi_capped_wo_eco,y=..density..,fill=rcp),
                 alpha=.65, breaks = bins_breaks,
                 position = position_dodge(width = 0.1)) +
  
  facet_wrap(~per)+
  # Titles and labels
  ggtitle("Distribution of monthly water stress indexes (months JJA; Gürbe catchment)") +
  xlab("Water Stress Index (WSI)") +
  ylab("Density") +  # Relative frequency on the y-axis
  theme_minimal() + 
  
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 10, expand = c(0.01, 0.01))+
  
  theme(
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(0.3, "cm"),
    plot.background = element_rect(fill="white",color = "black", linewidth = 0.1)
  )+
  
  annotate(geom="segment",y = 0,yend=8,x=1,xend=1, linetype="dashed",color="black")+
  annotate(geom="segment",y = 0,yend=8,x=0.8,xend=0.8, linetype="solid",color="black",linewidth = 0.09)+ 
  annotate(geom="segment",y = 0,yend=8,x=0.4,xend=0.4, linetype="solid",color="black",linewidth = 0.09)+
  annotate(geom="segment",y = 0,yend=8,x=0.2,xend=0.2, linetype="solid",color="black",linewidth = 0.09)+
  annotate(geom="segment",y = 0,yend=8,x=0.1,xend=0.1, linetype="solid",color="black",linewidth = 0.09)+
  
  scale_fill_manual(values = c("RCP26" = "#91bfdb", "RCP45" = "#ffd700", "RCP85" = "#fc8d59"))

ggsave("01_graphs/histo_wsi_summer.png", width = 12, height = 4)











### create summary tables 

df_plot$wsi_category <- sapply(df_plot$wsi,function(x)wsi_to_text(x))


# Group by period and RCP, then calculate percentage of observations per category
summary_stats <- df_plot %>%
  group_by(per, rcp) %>%
  count(wsi_category) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  select(-n) %>% 
  spread(key = wsi_category, value = percentage, fill = 0)


# Ensure the columns are arranged from "No Stress" to "Water Availability Exceeded"
summary_stats <- summary_stats %>%
  select(per, rcp, `No Stress`, `Low Stress`, `Moderate Stress`, `High Stress`, `Severe Stress`, `Water Availability Exceeded`)

write.csv(file="03_tables/wsi_summary_table.csv",summary_stats)


counts_n <- df_plot %>%
  select(per,rcp) %>% 
  group_by(per, rcp) %>% 
  count()

write.csv(file="03_tables/wsi_summary_table_counts.csv",counts_n)





### create summary tables 

df_plot$wsi_category <- sapply(df_plot$wsi_capped_wo_eco,function(x)wsi_to_text(x))


# Group by period and RCP, then calculate percentage of observations per category
summary_stats <- df_plot %>%
  group_by(per, rcp) %>%
  count(wsi_category) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  select(-n) %>% 
  spread(key = wsi_category, value = percentage, fill = 0)


# Ensure the columns are arranged from "No Stress" to "Water Availability Exceeded"
summary_stats <- summary_stats %>%
  select(per, rcp, `No Stress`, `Low Stress`, `Moderate Stress`, `High Stress`, `Severe Stress`, `Water Availability Exceeded`)

write.csv(file="03_tables/wsi_summary_table_without_ecology.csv",summary_stats)


counts_n <- df_plot %>%
  select(per,rcp) %>% 
  group_by(per, rcp) %>% 
  count()

write.csv(file="03_tables/wsi_summary_table_counts_without_eology.csv",counts_n)


### create summary tables 

df_plot$wsi_category <- sapply(df_plot_only_surface_water$wsi,function(x)wsi_to_text(x))




# Group by period and RCP, then calculate percentage of observations per category
summary_stats <- df_plot %>%
  group_by(per, rcp) %>%
  count(wsi_category) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  select(-n) %>% 
  spread(key = wsi_category, value = percentage, fill = 0)


# Ensure the columns are arranged from "No Stress" to "Water Availability Exceeded"
summary_stats <- summary_stats %>%
  select(per, rcp, `No Stress`, `Low Stress`, `Moderate Stress`, `High Stress`, `Severe Stress`, `Water Availability Exceeded`)

write.csv(file="03_tables/wsi_surface_water_summary_table.csv",summary_stats)


counts_n <- df_plot %>%
  select(per,rcp) %>% 
  group_by(per, rcp) %>% 
  count()

write.csv(file="03_tables/wsi_summary_table_counts.csv",counts_n)







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
                           function(x) sum(x, na.rm=TRUE))
          
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
        WSI[[(period - 1) * 3 + rcp]]$industrial[1] +
        WSI[[(period - 1) * 3 + rcp]]$drinking[1] +
        WSI[[(period - 1) * 3 + rcp]]$livestock_monthly[1] +
        WSI[[(period - 1) * 3 + rcp]]$ecology[1]
      
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


hist(df_plot_yearly$sf)

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
  group_by(per, rcp) %>%
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


ggsave("01_graphs/scatterplot_yearly_wsi.png")



df_plot$wsi_category <- sapply(df_plot$wsi,function(x)wsi_to_text(x))

# Step 1: Ensure the 'wsi_category' includes all levels, including "Water Availability Exceeded"
df_plot$wsi_category <- factor(df_plot$wsi_category, 
                               levels = c("No Stress", "Low Stress", "Moderate Stress", "High Stress", "Severe Stress", "Water Availability Exceeded"))

# Step 2: Calculate the number of points for each combination of 'per' and 'rcp'
df_plot_counts <- df_plot %>%
  group_by(per, rcp) %>%
  summarise(n = n()) %>%
  ungroup()

# Step 3: Plot with discrete fill based on the categorized WSI levels
ggplot(data = df_plot) +
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
               "Severe Stress" = "#b10026", 
               "Water Availability Exceeded" = "#542788"),  # Added color for "Water Availability Exceeded"
    name = "WSI Level"  # Name of the variable in the legend
  ) +
  facet_grid(per ~ rcp, scales = "fixed") +  # Create a facet grid with period as rows and rcp as columns
  ggtitle("Monthly WSI-Levels Gürbe Belp-Mühlimatt") +
  xlab("Streamflow [mm]") +
  ylab("Water Demand [mm]") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill="white", color = "black"),
    panel.background = element_rect(fill="transparent", color="black")
  ) +
  # Step 4: Add the count labels for each facet using geom_text
  geom_text(data = df_plot_counts, aes(x = Inf, y = Inf, label = paste("n =", n)),
            hjust = 1.1, vjust = 1.1, size = 3, inherit.aes = FALSE)

ggsave("01_graphs/scatterplot_monthly_wsi.png")






df_plot$wsi_category <- sapply(df_plot$wsi_capped_wo_eco,function(x)wsi_to_text(x))


# Step 1: Ensure the 'wsi_category' includes all levels, including "Water Availability Exceeded"
df_plot$wsi_category <- factor(df_plot$wsi_category, 
                               levels = c("No Stress", "Low Stress", "Moderate Stress", "High Stress", "Severe Stress", "Water Availability Exceeded"))

# Step 2: Calculate the number of points for each combination of 'per' and 'rcp'
df_plot_counts <- df_plot %>%
  group_by(per, rcp) %>%
  summarise(n = n()) %>%
  ungroup()

df_plot$dem_wo_eco <- df_plot_wo_eco$dem

# Step 3: Plot with discrete fill based on the categorized WSI levels
ggplot(data = df_plot) +
  geom_point(aes(y = dem_wo_eco, x = sf, fill = wsi_category),  # Use 'fill' for the discrete categories
             shape = 21,         # Use shape 21 for points with both fill and outline
             color = "black",     # Black outline
             size = 1.5,         # Point size
             stroke = 0.5) +     # Thickness of the outline
  scale_fill_manual(
    values = c("No Stress" = "#fcf6f0", 
               "Low Stress" = "#ffffb7", 
               "Moderate Stress" = "#FEB24C", 
               "High Stress" = "#FC4E2A", 
               "Severe Stress" = "#b10026", 
               "Water Availability Exceeded" = "#542788"),  # Added color for "Water Availability Exceeded"
    name = "WSI Level"  # Name of the variable in the legend
  ) +
  facet_grid(per ~ rcp, scales = "fixed") +  # Create a facet grid with period as rows and rcp as columns
  ggtitle("Monthly WSI-Levels Gürbe Belp-Mühlimatt") +
  xlab("Streamflow [mm]") +
  ylab("Water Demand [mm]") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill="white", color = "black"),
    panel.background = element_rect(fill="transparent", color="black")
  ) +
  # Step 4: Add the count labels for each facet using geom_text
  geom_text(data = df_plot_counts, aes(x = Inf, y = Inf, label = paste("n =", n)),
            hjust = 1.1, vjust = 1.1, size = 3, inherit.aes = FALSE)

ggsave("01_graphs/scatterplot_monthly_wsi_wo_eco.png")





df_plot$wsi_category <- sapply(df_plot_only_surface_water$wsi,function(x)wsi_to_text(x))


# Step 1: Ensure the 'wsi_category' includes all levels, including "Water Availability Exceeded"
df_plot$wsi_category <- factor(df_plot$wsi_category, 
                               levels = c("No Stress", "Low Stress", "Moderate Stress", "High Stress", "Severe Stress", "Water Availability Exceeded"))

# Step 2: Calculate the number of points for each combination of 'per' and 'rcp'
df_plot_counts <- df_plot %>%
  group_by(per, rcp) %>%
  summarise(n = n()) %>%
  ungroup()

df_plot$dem_surface <- df_plot_only_surface_water$dem

# Step 3: Plot with discrete fill based on the categorized WSI levels
ggplot(data = df_plot) +
  geom_point(aes(y = dem_surface, x = sf, fill = wsi_category),  # Use 'fill' for the discrete categories
             shape = 21,         # Use shape 21 for points with both fill and outline
             color = "black",     # Black outline
             size = 1,         # Point size
             stroke = 0.5) +     # Thickness of the outline
  scale_fill_manual(
    values = c("No Stress" = "#fcf6f0", 
               "Low Stress" = "#ffffb7", 
               "Moderate Stress" = "#FEB24C", 
               "High Stress" = "#FC4E2A", 
               "Severe Stress" = "#b10026", 
               "Water Availability Exceeded" = "#542788"),  # Added color for "Water Availability Exceeded"
    name = "WSI Level"  # Name of the variable in the legend
  ) +
  facet_grid(per ~ rcp, scales = "fixed") +  # Create a facet grid with period as rows and rcp as columns
  ggtitle("Monthly WSI-Levels Gürbe Belp-Mühlimatt(surface water use)") +
  xlab("Streamflow [mm]") +
  ylab("Water Demand [mm]") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill="white", color = "black"),
    panel.background = element_rect(fill="transparent", color="black")
  ) +
  # Step 4: Add the count labels for each facet using geom_text
  geom_text(data = df_plot_counts, aes(x = Inf, y = Inf, label = paste("n =", n)),
            hjust = 1.1, vjust = 1.1, size = 3, inherit.aes = FALSE)

ggsave("01_graphs/scatterplot_monthly_wsi_wo_surface_water.png")




rm(df_mock,df_plot,df_plot_counts,df_plot_yearly,df_plot_summer,df_plot_only_surface_water)

