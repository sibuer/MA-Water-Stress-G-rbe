library(dplyr)
library(lubridate)

# Ensure date is of class Date if not already
rhires$date <- as.Date(rhires$date)

# Filter data for the years 1992-2021
filtered_data <- rhires %>%
  filter(between(year(date), 1992, 2021))

# Define rainy days
filtered_data <- filtered_data %>%
  mutate(rainy_day = precip > 0)

# Label continuous rainy days as a single event
filtered_data <- filtered_data %>%
  group_by(season, year = year(date)) %>%  # Group by season and year
  mutate(event_id = cumsum(ifelse(rainy_day & (lag(rainy_day, default = FALSE) == FALSE), 1, 0))) %>%
  filter(rainy_day) %>%
  ungroup()

# Calculate event data
event_data <- filtered_data %>%
  group_by(event_id, season, year) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    total_precip = sum(precip),
    event_duration = n(),
    .groups = 'drop'
  )

# Summarize by season
seasonal_summary <- event_data %>%
  group_by(season) %>%
  summarise(
    average_event_duration = mean(event_duration),
    average_precipitation_per_event = mean(total_precip),
    .groups = 'drop'
  )

# Print the results
print(seasonal_summary)

### ----- 

library(dplyr)

# Assuming t1 is already loaded into R
# If not, you would typically load it using read.csv(), read.table(), etc.

# Calculate the mean daily PET per season
seasonal_pet <- t1 %>%
  group_by(season) %>%
  summarise(mean_pet = mean(pet_harg, na.rm = TRUE))  # Calculate mean, removing NA values if any

# Print the results
print(seasonal_pet)

### - average breaks between rain events 

library(dplyr)
library(lubridate)

# Ensure date is of class Date if not already
rhires$date <- as.Date(rhires$date)

# Define rainy and non-rainy days
rhires <- rhires %>%
  mutate(rainy_day = precip > 0)

# Label continuous rainy days as a single event and identify gaps between events
event_data <- rhires %>%
  mutate(event_id = cumsum(ifelse(rainy_day & lag(rainy_day, default = FALSE) == FALSE, 1, 0))) %>%
  filter(rainy_day) %>%
  group_by(event_id) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    season = first(season),  # Ensure that 'season' is carried through
    .groups = 'drop'
  )

# Calculate days without rain between events
gap_data <- event_data %>%
  arrange(start_date) %>%
  mutate(next_start_date = lead(start_date)) %>%
  mutate(days_between_events = next_start_date - end_date - 1)

# Calculate the mean gap for each season
seasonal_mean_gap <- gap_data %>%
  filter(!is.na(days_between_events)) %>%
  group_by(season) %>%
  summarise(
    average_days_without_rain = mean(days_between_events, na.rm = TRUE),
    .groups = 'drop'
  )

# Print the mean gap by season
print(seasonal_mean_gap)
