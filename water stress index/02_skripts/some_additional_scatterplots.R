
df_plot <- df_plot[df_plot$per=="2045-2074",]
df_plot_wo_eco <- df_plot_wo_eco[df_plot_wo_eco$per=="2045-2074",]



# Define bin breaks
bins_breaks <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)

# Cap WSI values at 1.02
df_plot$wsi_capped <- pmin(df_plot$wsi, 1.02)
df_plot$wsi_capped_wo_eco <- pmin(df_plot_wo_eco$wsi, 1.02)

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
           width = bin_width, color = "grey", alpha = .15, position = "dodge") +
  geom_col(data = wsi_counts_wo_eco, aes(x = bin_centers[bin], y = density, fill = rcp), 
           width = bin_width, alpha = .65, position = position_dodge(width = 0.1)) +
  
  facet_wrap(~rcp) +
  
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

ggsave("01_graphs/bars_lvl_wsi_bins_2045_2074.png", width = 12, height = 4)










df_plot <- df_plot[df_plot$per=="2045-2074",]
df_plot_wo_eco <- df_plot_wo_eco[df_plot_wo_eco$per=="2045-2074",]
df_plot_only_surface_water <- df_plot_only_surface_water[df_plot_only_surface_water$per=="2045-2074",]

df_plot <- df_plot_only_surface_water

# Define bin breaks
bins_breaks <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)

# Cap WSI values at 1.02
df_plot$wsi_capped <- pmin(df_plot$wsi, 1.02)
df_plot$wsi_capped_wo_eco <- pmin(df_plot_wo_eco$wsi, 1.02)

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
           width = bin_width, color = "grey", alpha = .6, position = "dodge") +

  
  facet_wrap(~rcp) +
  
  # Titles and labels
  ggtitle("Distribution of monthly water stress index (surface water; Gürbe catchment)") +
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

ggsave("01_graphs/bars_lvl_wsi_bins_2045_2074_only_surface_water.png", width = 12, height = 4)








df_plot <- df_plot[df_plot$per=="2045-2074",]

# Define bin breaks
bins_breaks <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)

# Cap WSI values at 1.02
df_plot$wsi_capped <- pmin(df_plot$wsi, 1.02)
df_plot$wsi_capped_wo_eco <- pmin(df_plot_wo_eco$wsi, 1.02)

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
           width = bin_width, color = "grey", alpha = .6, position = "dodge") +
  
  
  facet_wrap(~rcp) +
  
  # Titles and labels
  ggtitle("Distribution of monthly water stress index (total water need; Gürbe catchment)") +
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

ggsave("01_graphs/bars_lvl_wsi_bins_2045_2074_total_water_need.png", width = 12, height = 4)



















 
df_plot <- df_plot[df_plot$per=="2045-2074",]
df_plot <- df_plot[df_plot$season=="JJA",]


df_plot_wo_eco <- df_plot_wo_eco[df_plot_wo_eco$per=="2045-2074",]
df_plot_wo_eco <- df_plot_wo_eco[df_plot_wo_eco$season=="JJA",]


# Define bin breaks
bins_breaks <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)

# Cap WSI values at 1.02
df_plot$wsi_capped <- pmin(df_plot$wsi, 1.02)
df_plot$wsi_capped_wo_eco <- pmin(df_plot_wo_eco$wsi, 1.02)

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
  ggtitle("Distribution of monthly water stress index (months: JJA; total water need; Gürbe catchment)") +
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

ggsave("01_graphs/bars_lvl_wsi_bins_2045_2074_JJA_total_water_need.png", width = 12, height = 4)


