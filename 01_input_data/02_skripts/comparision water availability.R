# comparision water availablity 

lm.yearly<-list()
for(i in 1:3){
  file <- list.files("00_data/hydro_ch18_runoff/yearly")[grepl("2159",list.files("00_data/hydro_ch18_runoff/yearly"))][i]
  file <- paste("00_data/hydro_ch18_runoff/yearly/",file,sep="")
  name <- paste("yearly_",substring(file,(nchar(file)-8),nchar(file)-4), sep = "")
  lm.yearly[[name]] <-  read.csv(file = file, header = T)
  rm(file)
}

lm.yearly.mean <- data.frame(year=c(1981:2099),"RCP26" = apply(lm.yearly$yearly_RCP26[2:ncol(lm.yearly$yearly_RCP26)],MARGIN = 1, FUN = mean),
           "RCP45" = apply(lm.yearly$yearly_RCP45[2:ncol(lm.yearly$yearly_RCP45)],MARGIN = 1, FUN = mean),
           "RCP85" = apply(lm.yearly$yearly_RCP85[2:ncol(lm.yearly$yearly_RCP85)],MARGIN = 1, FUN = mean))
lm.yearly.mean$obs <- rep(NA,nrow(lm.yearly.mean))

obs_yearly_mean <- aggregate(runoff_daily_belp_obs$runoff,list(year(runoff_daily_belp_obs$time)),FUN=mean)
colnames(obs_yearly_mean) <- c("year","runoff")

lm.yearly.mean$obs[lm.yearly.mean$year>=1981&lm.yearly.mean$year<=2023] <- 
  obs_yearly_mean$runoff[obs_yearly_mean$year>=1981&obs_yearly_mean$year<=2023]



## plot 

# Assuming 'lm.yearly.mean' exists and is already loaded in your environment
# Create a new variable 'Period' for 5-year aggregation with specific handling for 2020-2023
lm.yearly.mean$Period <- cut(lm.yearly.mean$year, 
                             breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024), 
                             right = FALSE, 
                             labels = c("1980-1984", "1985-1989", "1990-1994", "1995-1999", "2000-2004", 
                                        "2005-2009", "2010-2014", "2015-2019", "2020-2023"))

# Reshape the data to long format
df_long <- pivot_longer(lm.yearly.mean, cols = c(RCP26, RCP45, RCP85, obs), names_to = "Scenario", values_to = "Runoff")

# Handle missing values for 'obs' in 2015 and create a specific aggregation for 2020-2023
# Here we aggregate by 5-year period and then manually ensure that all periods are represented
df_summary <- df_long %>%
  group_by(Period, Scenario) %>%
  summarize(Avg_Runoff = mean(Runoff, na.rm = TRUE), .groups = 'drop')

df_summary <- df_summary[-(37:40), ]

# Define colors (teal shades for scenarios and orange for obs)
colors <- c("RCP26" = "#3CB4AC", "RCP45" = "#66C2A5", "RCP85" = "#008080", "obs" = "#FF7043")

# Generate and display the plot directly using ggplot
ggplot(df_summary, aes(x = Period, y = Avg_Runoff, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(x = "", y = "Mean Streamflow [m3/s]", title = "Streamflow Gürbe Mühlimatte \nMean modeled Streamflow (Mülchi et al 2020) vs. observed values") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        plot.background = element_rect(fill = "#FFFFFF", color = NA))+
  scale_fill_manual(values = colors) +  # Apply custom colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve x-axis label readability

ggsave("01_graphs/water_availability_comparision_1981-2020.png", width = 5000, height = 3500, units = "px", dpi = 800)


rm(df_summary,df_long)




# Assuming 'lm.yearly.mean' exists and is already loaded in your environment
# Define 20-year periods starting from 1981, adjusting for observed data up to 2020
lm.yearly.mean$Period <- cut(lm.yearly.mean$year,
                             breaks = c(1981, 2001, 2021, 2041, 2061, 2081, 2101), 
                             right = FALSE,
                             labels = c("1981-2000", "2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))

# Reshape the data to long format
df_long <- pivot_longer(lm.yearly.mean, cols = c(RCP26, RCP45, RCP85, obs), names_to = "Scenario", values_to = "Runoff")

# Apply condition to restrict obs data after 2020
df_long <- df_long %>% 
  mutate(Runoff = ifelse(Scenario == "obs" & year > 2020, NA, Runoff))

# Aggregate data by 20-year period, calculating mean while ignoring NAs
df_summary <- df_long %>%
  group_by(Period, Scenario) %>%
  summarize(Avg_Runoff = mean(Runoff, na.rm = TRUE), .groups = 'drop')

# Define colors (teal shades for scenarios and orange for obs)
colors <- c("RCP26" = "#3CB4AC", "RCP45" = "#66C2A5", "RCP85" = "#008080", "obs" = "#FF7043")

# Generate and display the plot directly using ggplot
ggplot(df_summary, aes(x = Period, y = Avg_Runoff, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(x = "", y = "Mean Streamflow [m3/s]", title = "Mean Streamflow Gürbe Mühlimatte \nModeled Values (Mülchi et al. 2022) and observed Values") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFFFFF",color = NA),
        panel.border = element_rect(fill = NA, color = "black", size = 0.5))+
  scale_fill_manual(values = colors) +  # Apply custom colors
  theme(axis.text.x = element_text(angle = 0, hjust = .5))  # Improve x-axis label readability
ggsave("01_graphs/water_availability_periods.png", width = 5000, height = 3500, units = "px", dpi = 800)


# Aggregate data by 20-year period, calculating mean while ignoring NAs
df_summary <- df_long %>%
  group_by(Period, Scenario) %>%
  summarize(Avg_Runoff = mean(Runoff, na.rm = TRUE), .groups = 'drop')

df_summary$Avg_Runoff <- (df_summary$Avg_Runoff*seconds_per_year/size_catchment_m2)*1000

# 
# Define colors (teal shades for scenarios and orange for obs)
colors <- c("RCP26" = "#3CB4AC", "RCP45" = "#66C2A5", "RCP85" = "#008080", "obs" = "#FF7043")

# Generate and display the plot directly using ggplot
ggplot(df_summary, aes(x = Period, y = Avg_Runoff, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(x = "", y = "Mean Streamflow [mm]", title = "Mean Streamflow Gürbe Mühlimatte \nModeled Values (Mülchi et al. 2022) and observed Values") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFFFFF",color = NA),
        panel.border = element_rect(fill = NA, color = "black", size = 0.5))+
  scale_fill_manual(values = colors) +  # Apply custom colors
  theme(axis.text.x = element_text(angle = 0, hjust = .5))  # Improve x-axis label readability
ggsave("01_graphs/water_availability_periods_mm.png", width = 5000, height = 3500, units = "px", dpi = 800)


rm(df_long,df_summary,colors)
s