# flow duration curves
library(ggplot2)
library(dplyr)
library(readr)  # for read_csv if files are CSVs


lm.daily<-list()
for(i in 1:3){
  file <- list.files("00_data/hydro_ch18_runoff/daily")[grepl("2159",list.files("00_data/hydro_ch18_runoff/daily"))][i]
  file <- paste("00_data/hydro_ch18_runoff/daily/",file,sep="")
  name <- paste("daily_",substring(file,(nchar(file)-8),nchar(file)-4), sep = "")
  lm.daily[[name]] <-  read.csv(file = file, header = T)
  rm(file)
}


# Step 1: Load datasets
rcp26 <- lm.daily$daily_RCP26
rcp45 <- lm.daily$daily_RCP45
rcp85 <- lm.daily$daily_RCP85
# Convert dates to Date format if not already
rcp26$date <- as.Date(rcp26$time)
rcp45$date <- as.Date(rcp45$time)
rcp85$date <- as.Date(rcp85$time)

obs_belp <- belp_daily %>% filter(year(belp_daily$time)<=2019)
obs_belp$date <- ymd(obs_belp$time)
obs_belp$time <- NULL

# Define periods and the datasets list with appropriate scenarios
periods <- c("1990-2019", "2020-2049", "2045-2074")
datasets <- list(RCP26 = rcp26, RCP45 = rcp45, RCP85 = rcp85, obs = obs_belp)

# Initialize an empty list to store combined data for all periods
all_period_data <- list()

for (period in periods) {
  start_year <- as.numeric(substr(period, 1, 4))
  end_year <- as.numeric(substr(period, 6, 9))
  
  # Process each scenario separately
  period_data_list <- lapply(datasets, function(data) {
    # Select only runoff columns, explicitly avoiding date or time columns
    model_columns <- grep("RCP|runoff", names(data), value = TRUE)  # Adjust regex to match runoff data columns
    scenario_data <- data %>%
      filter(year(date) >= start_year & year(date) <= end_year) %>%
      select(date, all_of(model_columns))
    
    # Calculate FDC for each model run individually
    fdc_data <- scenario_data %>%
      pivot_longer(cols = model_columns, names_to = "model", values_to = "runoff") %>%
      arrange(desc(runoff)) %>%
      mutate(exceedance = rank(-runoff) / n() * 100)  # Calculate exceedance probability
    return(fdc_data)
  })
  
  # Combine all scenario data
  combined_data <- bind_rows(period_data_list, .id = "scenario") %>%
    mutate(period = period)  # Add period information for faceting
  all_period_data[[period]] <- combined_data
}

# Combine data for all periods into one data frame
combined_fdc_data <- bind_rows(all_period_data)

# Define Q347 (which is approximately the 95th percentile for daily runoff values over the year)
# Compute Q347 outside the ggplot call to handle NA values correctly
Q347_values <- combined_fdc_data %>%
  group_by(period) %>%
  summarize(Q347 = quantile(runoff, 0.95, na.rm = TRUE))  # Ensure na.rm = TRUE to handle NAs

# Plot
fdc_plot <- ggplot(combined_fdc_data, aes(x = exceedance, y = runoff, color = scenario, group = interaction(scenario, period))) +
  geom_line() +
  facet_wrap(~ period, scales = "free_y") +
  labs(title = "Flow Duration Curves Gürbe Mühlimatte",
       x = "Exceedance Probability [%]",
       y = "Streamflow [m3/s]") +
  scale_y_log10(limits = c(0.1, 80), breaks = c(0.1, 1, 10, 20, 40, 80), labels = scales::comma) +
  scale_color_manual(values = c("RCP26" = "#3CB4AC", "RCP45" = "#66C2A5", "RCP85" = "#008080", "obs" = "#FF7043")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.background = element_rect(fill=NA,color="black"),
        plot.background = element_rect(fill="#FFFFFF",color = NA))

# Compute Q347 values correctly as the 0.05 quantile, but only for the RCP45 scenario
Q347_values <- combined_fdc_data %>%
  filter(scenario == "RCP45") %>%
  group_by(period) %>%
  summarize(Q347 = quantile(runoff, 0.05, na.rm = TRUE), .groups = "drop")  # Compute the 0.05 quantile for each period

# Add Q347 horizontal line across the plots for RCP45
fdc_plot <- fdc_plot +
  geom_hline(data = Q347_values, aes(yintercept = Q347), linetype = "dashed", color = "red")

# Add the Q347 label on the first plot only (1981-2000), positioning it more centrally
label_data <- Q347_values %>%
  filter(period == "1981-2000") %>%
  mutate(x = 50)  # Set 'x' to 50 to place the label more centrally

fdc_plot <- fdc_plot + geom_text(data = label_data, 
                                 aes(x = x, y = Q347, label = "Q347"), 
                                 inherit.aes = FALSE,  # Prevent inheriting global aesthetics
                                 color = "red", vjust = 0, hjust = 1)

# Print the plot
print(fdc_plot)

ggsave("01_graphs/FDC_periods.png", width = 18, height = 7, units = "cm", dpi = 800)




### single plot observed data
fdc_data_obs <-  combined_fdc_data %>% filter(scenario=="obs")
fdc_data_obs <- fdc_data_obs %>% filter(period=="1994-2023")

ggplot(fdc_data_obs, aes(x = exceedance, y = runoff, color = scenario, group = 1)) +
  geom_line(linewidth=1) +
  labs(title = "Flow Duration Curve Gürbe Mühlimatte",
       x = "Exceedance Probability [%]",
       y = "Streamflow [m3/s]") +
  scale_y_continuous(limits = c(0.1, 40), breaks = c(0.1, 1, 10, 20, 40, 80), labels = scales::comma) +
  scale_color_manual(values = c("RCP26" = "#3CB4AC", "RCP45" = "#66C2A5", "RCP85" = "#008080", "obs" = "#6CB0D6")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.background = element_rect(fill=NA,color="black"),
        plot.background = element_rect(fill="#FFFFFF",color = NA),
        legend.position = "none"
  )

ggsave("01_graphs/fdc_observed.png", width = 5, height = 5, units = "in")
# Assuming combined_fdc_data contains all the necessary data with 'scenario', 'period', and 'runoff'

# Compute Q347 values for each scenario within each period
Q347_values <- combined_fdc_data %>%
  group_by(period, scenario) %>%
  summarize(Q347 = quantile(runoff, 0.05, na.rm = TRUE), .groups = "drop")  # Compute the 0.05 quantile

# Create a pivot table with periods as rows and scenarios as columns
Q347_table <- Q347_values %>%
  pivot_wider(names_from = scenario, values_from = Q347)  # Reshape data

# Since you want observed data included only for the first two periods
# We need to ensure 'obs' is included correctly
# Let's assume 'obs' is part of the scenarios and has data for all periods; we'll conditionally remove it later

# Optionally, manually adjust if 'obs' data is only available or should only appear for first two periods
Q347_table <- Q347_table %>%
  mutate(obs = if_else(period %in% c("1981-2000", "2001-2020"), obs, NA_real_))

# Print the table to check the output
print(Q347_table)

factor_year_mm <- 365.25*24*60*60/116000000*1000
Q347_mm <- data.frame(Q347_table[,1], (Q347_table[,2:5] * factor_year_mm))

rm(all_period_data,combined_data,combined_fdc_data,constants,datasets,df_long,fdc_plot,fdc_plots, obs_belp,
   label_data,period_data_list,processeddata,Q347_values,rcp26,rcp45,rcp85,custom_colors,name,period,test)
