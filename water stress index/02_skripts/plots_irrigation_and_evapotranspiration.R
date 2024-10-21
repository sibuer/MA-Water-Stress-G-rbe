### plots_irrigation
pl_lines_color <- c("#91bfdb", "#ffd700", "#fc8d59")




## IRRIGATION

pl_df <- tibble(value=vector(),names=vector(),rcp=vector(),period=vector())

for(i in 1:9){
  # Select and pivot the relevant columns
  pl_sub <- WSI[[i]] %>% select(irr_median, irr_q25, irr_q75,month)
  #pl_sub <- pivot_longer(pl_sub, cols = c(irr_median, irr_q25, irr_q75), values_to = "value", names_to = "names")
  
  # Add RCP and period columns
  pl_sub$rcp <- paste(
    "RCP ",
    substring(names(WSI)[i], 14, 14),
    ".",
    substring(names(WSI)[i], 15, 15),
    sep = ""
  )
  pl_sub$period <- paste(
    substring(names(WSI)[i], 1, 4),
    "-",
    substring(names(WSI)[i], 6, 9),
    sep = ""
  )
  
  # Append pl_sub to pl_df
  pl_df <- rbind(pl_df, pl_sub)
}

pl_df <- tibble(pl_df)
pl_df$month <- factor(pl_df$month, levels = 1:12, labels = month.abb)


ggplot(data = pl_df, aes(x = month, group = 1)) +
  facet_grid(rcp ~ period, scales = "fixed") +
  #scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  
  # Add custom ribbon and line
  geom_ribbon(
    aes(ymin = irr_q25, ymax = irr_q75, fill = rcp),
    alpha = 0.5, color="grey", linewidth = 0.1
  ) +
  geom_line(aes(y = irr_median, color=rcp), linewidth = 1.2, show.legend = F) +
  scale_color_manual(values =
                       c("RCP 2.6" = pl_lines_color[1],
                         "RCP 4.5" = pl_lines_color[2],
                         "RCP 8.5" = pl_lines_color[3])) +
  # Custom theme settings
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey", linewidth = 0.25),
    panel.grid.minor.x = element_blank(),  # Disable minor x grid lines
    panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey", linewidth = 0.1),
    plot.background = element_rect(color = "transparent", fill = "white"), 
    axis.text.x = element_text(angle = 90, size = 7, hjust = 1,vjust=0),
    panel.border = element_rect(color = "black", fill = "transparent"),
    panel.background = element_rect(fill="white"),
    panel.spacing = unit(0.8, "lines"),
  ) +
  
  # Customize axis and grid
  scale_x_discrete(expand = c(0, 0)) +  # Ensure month grid aligns with the axis
  labs(fill = "RCP-Scenario", title = "Irrigation Need") +
  xlab("") +
  ylab("[mm]")

ggsave("01_graphs/final/Irrigation.png")









## STREAMFLOW

pl_df <- tibble(value=vector(),names=vector(),rcp=vector(),period=vector())

for(i in 1:9){
  # Select and pivot the relevant columns
  pl_sub <- WSI[[i]] %>% select(sfm_median, sfm_q25, sfm_q75,month)
  #pl_sub <- pivot_longer(pl_sub, cols = c(irr_median, irr_q25, irr_q75), values_to = "value", names_to = "names")
  
  # Add RCP and period columns
  pl_sub$rcp <- paste(
    "RCP ",
    substring(names(WSI)[i], 14, 14),
    ".",
    substring(names(WSI)[i], 15, 15),
    sep = ""
  )
  pl_sub$period <- paste(
    substring(names(WSI)[i], 1, 4),
    "-",
    substring(names(WSI)[i], 6, 9),
    sep = ""
  )
  
  # Append pl_sub to pl_df
  pl_df <- rbind(pl_df, pl_sub)
}

pl_df <- tibble(pl_df)
pl_df$month <- factor(pl_df$month, levels = 1:12, labels = month.abb)


ggplot(data = pl_df, aes(x = month, group = 1)) +
  facet_grid(rcp ~ period, scales = "fixed") +
  #scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  
  # Add custom ribbon and line
  geom_ribbon(
    aes(ymin = sfm_q25, ymax = sfm_q75, fill = rcp),
    alpha = 0.5, color="grey", linewidth = 0.1
  ) +
  geom_line(aes(y = sfm_median, color=rcp), linewidth = 1.2, show.legend = F) +
  scale_color_manual(values =
                       c("RCP 2.6" = pl_lines_color[1],
                         "RCP 4.5" = pl_lines_color[2],
                         "RCP 8.5" = pl_lines_color[3])) +
  # Custom theme settings
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey", linewidth = 0.25),
    panel.grid.minor.x = element_blank(),  # Disable minor x grid lines
    panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey", linewidth = 0.1),
    plot.background = element_rect(color = "transparent", fill = "white"), 
    axis.text.x = element_text(angle = 90, size = 7, hjust = 1,vjust=0),
    panel.border = element_rect(color = "black", fill = "transparent"),
    panel.background = element_rect(fill="white"),
    panel.spacing = unit(0.8, "lines"),
  ) +
  
  # Customize axis and grid
  scale_x_discrete(expand = c(0, 0)) +  # Ensure month grid aligns with the axis
  labs(fill = "RCP-Scenario", title = "Streamflow Gürb Belp-Mühlimatte") +
  xlab("") +
  ylab("[mm]")

ggsave("01_graphs/final/Streamflow_interval_mm.png")


periods

### precipitation plots 

# create empty matrix 
pl_df <- matrix(ncol = 6,nrow=0)
# precipitation <- precipitation %>% rename(date=day)
rcp_list_nice <- c("RCP 2.6","RCP 4.5", "RCP 8.5")

for(i in 1:3){
  
  # subset data period
  pl_sub <- precipitation %>% filter(
    year(precipitation$date)>=periods[i,1]&
      year(precipitation$date)<=periods[i,2]
  )
  
  # aggregate data with monthly_sums()
  pl_list <- monthly_sums(pl_sub, q_min = q_min, q_max = q_max, method = method_aggregation)
  
  # create period-string
  pl_period <- paste(periods[i,1],"-",periods[i,2],sep="")
  for(k in 1:3){
    
    # create df for all rcp scen & periods
    pl_temp <- pl_list[[k]]
    pl_temp$rcp <- rcp_list_nice[k]
    pl_temp$period <- pl_period
    
    # synch col names to ensure rbind
    colnames(pl_df) <- colnames(pl_temp) 
    
    # end new df to end of existing one 
    pl_df <- rbind(pl_df, pl_temp)
  }
}

pl_df$month <- factor(pl_df$month, levels = 1:12, labels = month.abb)

ggplot(data = pl_df, aes(x = month, group = 1)) +
  facet_grid(rcp ~ period, scales = "fixed") +
  #scale_color_brewer(palette = "RdYlBu", direction = -1) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  
  # Add custom ribbon and line
  geom_ribbon(
    aes(ymin = q_min, ymax = q_max, fill = rcp),
    alpha = 0.5, color="grey", linewidth = 0.1
  ) +
  geom_line(aes(y = median, color=rcp), linewidth = 1.2, show.legend = F) +
  scale_color_manual(values =
                       c("RCP 2.6" = pl_lines_color[1],
                         "RCP 4.5" = pl_lines_color[2],
                         "RCP 8.5" = pl_lines_color[3])) +
  # Custom theme settings
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey", linewidth = 0.25),
    panel.grid.minor.x = element_blank(),  # Disable minor x grid lines
    panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey", linewidth = 0.1),
    plot.background = element_rect(color = "transparent", fill = "white"), 
    axis.text.x = element_text(angle = 90, size = 7, hjust = 1,vjust=0),
    panel.border = element_rect(color = "black", fill = "transparent"),
    panel.background = element_rect(fill="white"),
    panel.spacing = unit(0.8, "lines"),
  ) +
  
  # Customize axis and grid
  scale_x_discrete(expand = c(0, 0)) +  # Ensure month grid aligns with the axis
  labs(fill = "RCP-Scenario", title = "Precipitation Sums") +
  xlab("") +
  ylab("[mm]")

ggsave("01_graphs/final/Precipiation_conf_interval.png")






# precip monthly
pl_df <- aggregate_for_boxplots(precipitation,scale = "monthly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~month,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Monthly Precipitation") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[mm]")  

ggsave("01_graphs/final/Precipiation_monthly.png")

# precip yearly
pl_df <- aggregate_for_boxplots(precipitation,scale = "yearly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  #facet_wrap(~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Yearly Precipitation") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[mm]")  

ggsave("01_graphs/final/Precipiation_monthly_yearly.png")










# streamflow monthly
pl_df <- aggregate_for_boxplots(streamflow_belp_mod_m3,scale = "monthly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~month,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Monthly Streamflow Gürbe Belp-Mühlimatte") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[m3/s]")  

ggsave("01_graphs/final/streamflow_monthly_m3s.png")


ggplot(data = pl_df, aes(x=month, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Monthly Streamflow Gürbe Belp-Mühlimatte") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[m3/s]")  

ggsave("01_graphs/final/streamflow_monthly_m3s_2.png")

# streamflow yearly
pl_df <- aggregate_for_boxplots(streamflow_belp_mod_m3,scale = "yearly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  #facet_wrap(~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Yearly Streamflow Gürbe Belp Mühlimatte") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[m3/s]")  

ggsave("01_graphs/final/streamflow_yearly_m3s.png")










# irrigation monthly
pl_df <- aggregate_for_boxplots(irrigation_need_CTM,scale = "monthly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~month,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Irrigation Water Need") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[mm]")  

ggsave("01_graphs/final/irrigation_monthly.png")

ggplot(data = pl_df, aes(x=month, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Irrigation Water Need") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[mm]")  

ggsave("01_graphs/final/irrigation_monthly_2.png")

# streamflow yearly
pl_df <- aggregate_for_boxplots(irrigation_need_CTM,scale = "yearly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  #facet_wrap(~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Irrigation Water Need") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[mm]")  

ggsave("01_graphs/final/irrigation_yearly.png")









# ETo monthly
pl_df <- aggregate_for_boxplots(ET0_CTM,scale = "monthly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~month,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Potential Evapotranspiration (FAO Penman-Monteith)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[m3/s]")  

ggsave("01_graphs/final/ETo_monthly.png")

ggplot(data = pl_df, aes(x=month, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Potential Evapotranspiration (FAO Penman-Monteith)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[m3/s]")  

ggsave("01_graphs/final/ETo_monthly_2.png")

# ETo yearly
pl_df <- aggregate_for_boxplots(ET0_CTM,scale = "yearly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  #facet_wrap(~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Potential Evapotranspiration (FAO Penman-Monteith)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[m3/s]")  

ggsave("01_graphs/final/ETo_yearly.png")





# ETo monthly
tas_CTM <- tas_CTM %>% rename(date=day)
pl_df <- aggregate_for_boxplots(tas_CTM,scale = "monthly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~month,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Temperature above Surface") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[C°]")  

ggsave("01_graphs/final/tas_monthly.png")

ggplot(data = pl_df, aes(x=month, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Temperature above Surface") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[C°]")  

ggsave("01_graphs/final/tas_monthly_2.png")

# ETo yearly
pl_df <- aggregate_for_boxplots(tas_CTM,scale = "yearly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  #facet_wrap(~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Temperature above Surface") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[C°]")  

ggsave("01_graphs/final/tas_yearly.png")












## comparison graph ETo und ETc 

pl_df <- aggregate_for_boxplots(ET0_CTM,scale = "yearly")
pl_df$type <- "ET0"

pl_df2 <- aggregate_for_boxplots(ETc_CTM,scale = "yearly")
pl_df2$type <- "ETc"

pl_df <- rbind(pl_df,pl_df2)

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_wrap(~type,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Potential Evapotranspiration (FAO Penman-Monteith)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[m3/s]")  

ggsave("01_graphs/final/comparison_ETo_ETc.png")




# rem. newly created variables 
pl_vars <- grep("^pl", ls(), value = TRUE)
rm(list = pl_vars)





























#### hurs 

pl_df <- aggregate_for_boxplots(irrigation_need_CTM,scale = "monthly")



# ETo yearly
pl_df <- aggregate_for_boxplots(tas_CTM,scale = "yearly")

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  #facet_wrap(~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Temperature above Surface") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7)
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[C°]")  

ggsave("01_graphs/final/tas_yearly.png")


