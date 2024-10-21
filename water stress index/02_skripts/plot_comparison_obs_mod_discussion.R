library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(gridExtra)


# Prepare the model data (mod)
mod <- streamflow_belp_mod_m3 %>%
  filter(year(date) >= 1985 & year(date) <= 2014)

mod <- aggregate(mod[2:ncol(mod)],
                 list(format(mod$date, "%Y_%m")),
                 FUN = function(x) sum(x))

mod <- pivot_longer(mod,
                    cols = -Group.1,
                    names_to = "variable",
                    values_to = "value") %>%
  select(-variable)

df_plot <- aggregate(mod$value, list(substring(mod$Group.1, 6, 7)), function(x) median(x))

df_range_mod <- data.frame(
  month = 1:12,  # Numeric months 1:12
  qmin = aggregate(mod$value, list(substring(mod$Group.1, 6, 7)), function(x) quantile(x, 0.05))[, 2],
  qmax = aggregate(mod$value, list(substring(mod$Group.1, 6, 7)), function(x) quantile(x, 0.95))[, 2]
)

# Prepare the observed data (obs)
obs <- belp_daily %>%
  filter(year(time) >= 1985 & year(time) <= 2014)

obs <- aggregate(obs$runoff,
                 list(format(ymd(obs$time), "%Y_%m")),
                 FUN = function(x) sum(x))

obs <- pivot_longer(obs,
                    cols = -Group.1,
                    names_to = "variable",
                    values_to = "value") %>%
  select(-variable)

df_plot$obs <- aggregate(obs$value, list(substring(obs$Group.1, 6, 7)), function(x) median(x))[, 2]

df_range_obs <- data.frame(
  month = 1:12,  # Numeric months 1:12
  qmin = aggregate(obs$value, list(substring(obs$Group.1, 6, 7)), function(x) quantile(x, 0.05))[, 2],
  qmax = aggregate(obs$value, list(substring(obs$Group.1, 6, 7)), function(x) quantile(x, 0.95))[, 2]
)

# Prepare the plot data
df_plot$Group.1 <- 1:12  # Set numeric months for the plot data
colnames(df_plot) <- c("month", "mod", "obs")

df_plot <- pivot_longer(df_plot,
                        cols = -month,
                        names_to = "type",
                        values_to = "values")

# Plot with quantile ribbons and numeric x-axis with manual labels
color_mod <- "#0072B2"  # Color for model line (adjustable)
color_obs <- "#D55E00"  # Color for observed line (adjustable)

p1 <- ggplot(data = df_plot) +
  # # Adding the lines for model and observed
  # geom_line(aes(x = month, y = values, color = type, group = type), size = 1.2) +
  # 
  # Adding the quantile ribbons for model and observed across all months
  geom_ribbon(data = df_range_mod, aes(x = month, ymin = qmin, ymax = qmax),
              fill = color_mod, alpha = 0.3, show.legend = FALSE) +
  geom_ribbon(data = df_range_obs, aes(x = month, ymin = qmin, ymax = qmax),
              fill = color_obs, alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(x = month, y = values, color = type, group = type), size = 1.2) +  
  # Customizing the color for lines
  scale_color_manual(values = c("mod" = color_mod, "obs" = color_obs)) +
  
  ggtitle("Monthly Streamflow (1985-2014)") +
  
  # Labels and limits
  ylim(0, 200) +
  labs(x = "", y = "monthly streamflow [m3]") +
  
  # Manually set x-axis labels to months (Jan, Feb, etc.)
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill="transparent",color = "black"),
        plot.background = element_rect(fill="white", colour = "transparent"),
        legend.position  = c(0.85, 0.85),
        legend.background = element_rect(fill = "white", color = "black"), # Optional: white background for the legend
        
  )






# Second part of the script: Creating a graph with Parde coefficients for observed and modeled data

# Assume we have the Parde coefficients data for both observed and modeled data 
# in a similar structure as the streamflow data, as 'parde_mod' and 'parde_obs'.

# Example data preparation (replace this with your actual Parde coefficient data)
# For simplicity, here I assume Parde coefficients are available on a monthly basis (1 to 12).

parde_mod <- df_plot %>% filter(type=="mod") %>% select(values) %>% as.list() %>% as.vector()
parde_mod <- as.vector(parde_mod$values)

parde_obs <- df_plot %>% filter(type=="obs") %>% select(values) %>% as.list() %>% as.vector()
parde_obs <- as.vector(parde_obs$values)



parde_coeff <- data.frame(
  month = 1:12,
  obs = parde_obs / mean(parde_obs),
  mod = parde_mod / mean(parde_mod)
)

# Reshape the Parde coefficient data for ggplot
parde_coeff <- pivot_longer(parde_coeff,
                            cols = c(obs,mod),
                            names_to = "type",
                            values_to = "parde_value")

# Plot the Parde coefficients for observed and modeled data
p2 <- ggplot(data = parde_coeff) +
  # Adding the lines for modeled and observed Parde coefficients
  geom_line(aes(x = month, y = parde_value, color = type, group = type), size = 1.2) +
  
  # Customizing the color for lines (you can adjust these as needed)
  scale_color_manual(values = c("mod" = color_mod, "obs" = color_obs)) +
  
  ggtitle("Pardé Coefficients (1985-2014)") +
  
  # Labels and limits
  ylim(0.5, 1.75) +
  labs(x = "", y = "Pardé Coefficient") +
  
  # Manually set x-axis labels to months (Jan, Feb, etc.)
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill="transparent",color = "black"),
        plot.background = element_rect(fill="white",colour = "transparent"),
        legend.position  = c(0.85, 0.85),
        legend.background = element_rect(fill = "white", color = "black"), # Optional: white background for the legend
  )



arranged_plot <- grid.arrange(grobs = list(p1,p2), ncol = 2)
ggsave("01_graphs/comp_obs_mod.png",plot = arranged_plot, units = "in", width = 10, height = 4)

