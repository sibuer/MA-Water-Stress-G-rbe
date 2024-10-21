library(scales)
library(scico)

####---------------------------------------------------------------------------
# observed data

# boxplot 
bp_runoff <- belp_daily %>% filter(year(time)>1993) %>% filter(year(time)<2024)
bp_runoff$month <- month(bp_runoff$time)

# Define the proper order for months
bp_runoff <- bp_runoff %>%
  mutate(month = as.character(month)) %>% 
  mutate(month = factor(month, 
                        levels = 1:12, 
                        labels = month(1:12, label = TRUE, abbr = TRUE, locale = "US")))

# plot 
ggplot(data = bp_runoff, aes(x = month, y = runoff, group = month)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(outlier.colour = "lightgrey", outlier.shape = 1, fill = "#6CB0D6") +  # Set single fill color and outliers to grey
  
  ggtitle("Streamflow Gürbe Belp (1994-2023)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey", linewidth = 0.1),
    plot.background = element_rect(color = "transparent", fill = "white"), 
    axis.text.x = element_text(angle = 0, size = 8, hjust = 0.5),
    panel.border = element_rect(color = "grey", fill = "transparent"),
    legend.position = "none"  # Remove legend
  ) +
  xlab("") +
  ylab("[m3/s]") + 
  ylim(0, 7)

ggsave("01_graphs/boxplot_streamflow_month.png")#, width = 10.58, height = 7.92)

# regime plot 
bp_regime <- data.frame(month = month(1:12, label = TRUE, abbr = TRUE, locale = "US"))
bp_regime$mean <- aggregate(bp_runoff$runoff, by=list(bp_runoff$month), function(x)mean(x))[,2]
bp_regime$median <- aggregate(bp_runoff$runoff, by=list(bp_runoff$month), function(x)median(x))[,2]

bp_regime <- bp_regime %>%
  pivot_longer(cols = c(mean, median), 
               names_to = "statistic", 
               values_to = "value")

ggplot(data=bp_regime, aes(x=month, y=value, group=statistic, color=statistic)) +
  geom_line(linewidth = 1.4) +
  ggtitle("Streamflow Gürbe Belp (1994-2023)") +
  scale_color_manual(values = c("mean" = "#FF1F5B", "median" = "#6CB0D6")) +  # Define custom colors
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(linetype = "dashed"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey", linewidth = 0.1),
    plot.background = element_rect(color="transparent", fill="white"),
    legend.position = c(0.95, 0.95),  # Position legend in top right corner
    legend.justification = c("right", "top"),  # Anchor the legend to the top right
    legend.background = element_rect(fill = "white", color = "white"),
    axis.line.x = element_line(color = "black", size = 1.5),  # Add axis line here
    axis.ticks.length = unit(0.1, "cm")  # Set small ticks length
  ) +
  labs(color = NULL) +  # Remove legend title
  xlab("") +
  ylab("[m3/s]") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,5)) +  # Remove space between y=0 and x-axis
  theme(axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.2, "cm"))

ggsave("01_graphs/regime_streamflow_month.png")

# List all objects in the workspace that start with "irr_"
objects_with_irr_prefix <- ls(pattern = "^bp_")
rm(list = objects_with_irr_prefix, objects_with_irr_prefix, df_boxplot_streamflow)









####---------------------------------------------------------------------------
## modeled data
bp_26 <- belp_modelled_RCP26 %>%
  pivot_longer(cols = names(belp_modelled_RCP26)[2:ncol(belp_modelled_RCP26)], 
               names_to = "rcp", 
               values_to = "value") %>% 
  mutate(rcp="RCP 2.6") 

bp_45 <- belp_modelled_RCP45 %>%
  pivot_longer(cols = names(belp_modelled_RCP45)[2:ncol(belp_modelled_RCP45)], 
               names_to = "rcp", 
               values_to = "value") %>% 
  mutate(rcp="RCP 4.5") 

bp_85 <- belp_modelled_RCP85 %>%
  pivot_longer(cols = names(belp_modelled_RCP85)[2:ncol(belp_modelled_RCP85)], 
               names_to = "rcp", 
               values_to = "value") %>% 
  mutate(rcp="RCP 8.5") 

bp_model <- rbind(bp_26,bp_45,bp_85)


bp_model <- bp_model %>%
  mutate(period = factor(findInterval(year(time), c(2021, 2041, 2061, 2081, 2101)),
                         labels = c("2021-2040", "2041-2060", "2061-2080", "2081-2100")))


bp_model <- bp_model %>% filter(year(time)>=2041&year(time)<=2080)

bp_model$month <- bp_model$time %>% month() 
bp_model <- bp_model %>%
  mutate(month = as.character(month)) %>% 
  mutate(month = factor(month, 
                        levels = 1:12, 
                        labels = month(1:12, label = TRUE, abbr = TRUE, locale = "US")))
  
ggplot(data = bp_model, aes(x=month, y=value, fill=rcp, group = month)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~period,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Modeled Streamflow Belp (Mülchi et al. 2022)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1, linetype = "dashed"),
    plot.background = element_rect(color="transparent",fill="white"), 
    axis.text.x = element_text(angle = 90, size = 7,hjust=1, vjust=0.5),
    panel.border = element_rect(color = "grey",fill = "transparent"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length.x = unit(0.05, "cm")
  ) +
  scale_y_continuous(minor_breaks = seq(0,7,0.5),
                     breaks = seq(0,7,1),
                     labels = function(x) ifelse(x %% 2 == 0, x, "")) + 
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[mm]")


ggsave("01_graphs/modeled_streamflow_belp.png")


# List all objects in the workspace that start with "irr_"
objects_with_irr_prefix <- ls(pattern = "^bp_")
rm(list = objects_with_irr_prefix, objects_with_irr_prefix, df_boxplot_streamflow)



