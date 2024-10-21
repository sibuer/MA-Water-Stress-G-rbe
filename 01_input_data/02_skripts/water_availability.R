obs.periods.monthly <- list()
obs.periods.monthly[["obs_1981_2000"]] <- aggregate(runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>1981&year(runoff_daily_belp_obs$time)<=2000],
          by = list(month(runoff_daily_belp_obs$time[year(runoff_daily_belp_obs$time)>1981&year(runoff_daily_belp_obs$time)<=2000])),
                    FUN = mean)
obs.periods.monthly[["obs_2001_2020"]] <- aggregate(runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>2001&year(runoff_daily_belp_obs$time)<=2020],
                                                by = list(month(runoff_daily_belp_obs$time[year(runoff_daily_belp_obs$time)>2001&year(runoff_daily_belp_obs$time)<=2020])),
                                                FUN = mean)
obs.periods.monthly.mm <- list()
obs.periods.monthly.mm[["obs_1981_2000"]] <- data.frame(month=c(1:12), 
                                                        runoff_mm = obs.periods.monthly$obs_1981_2000$x*factor_m3_s_mm_monthly/size_catchment_m2)
obs.periods.monthly.mm[["obs_2001_2020"]] <- data.frame(month=c(1:12), 
                                                        runoff_mm = obs.periods.monthly$obs_2001_2020$x*factor_m3_s_mm_monthly/size_catchment_m2)
####
#### for [mm]

lm.mean.mm <- list()
for(i in 1:4){
  df <- lm.periods.monthly.mm[grepl("RCP26",names(lm.periods.monthly.mm))][[i]]
  name <- names(lm.periods.monthly.mm[grepl("RCP26",names(lm.periods.monthly.mm))][i])
  df <- data.frame(month=1:12,runoff=apply(df[,2:ncol(df)],MARGIN=1,FUN=mean))
  lm.mean.mm[[name]] <- df
  rm(df)
}
for(i in 1:4){
  df <- lm.periods.monthly.mm[grepl("RCP45",names(lm.periods.monthly.mm))][[i]]
  name <- names(lm.periods.monthly.mm[grepl("RCP45",names(lm.periods.monthly.mm))][i])
  df <- data.frame(month=1:12,runoff=apply(df[,2:ncol(df)],MARGIN=1,FUN=mean))
  lm.mean.mm[[name]] <- df
  rm(df)
}
for(i in 1:4){
  df <- lm.periods.monthly.mm[grepl("RCP85",names(lm.periods.monthly.mm))][[i]]
  name <- names(lm.periods.monthly.mm[grepl("RCP85",names(lm.periods.monthly.mm))][i])
  df <- data.frame(month=1:12,runoff=apply(df[,2:ncol(df)],MARGIN=1,FUN=mean))
  lm.mean.mm[[name]] <- df
  rm(df)
}

# dataframe with col for each IPCC scenario 
comp.monthly.mm <- list()
comp.monthly.mm[["RCP26"]] <- cbind(sapply(obs.periods.monthly.mm, function(x) x[, 2]),
                                    sapply(lm.mean.mm[grepl("RCP26", names(lm.mean.mm))], function(x) x[, 2]))
comp.monthly.mm[["RCP45"]] <- cbind(sapply(obs.periods.monthly.mm, function(x) x[, 2]),
                                    sapply(lm.mean.mm[grepl("RCP45", names(lm.mean.mm))], function(x) x[, 2]))
comp.monthly.mm[["RCP85"]] <- cbind(sapply(obs.periods.monthly.mm, function(x) x[, 2]),
                                    sapply(lm.mean.mm[grepl("RCP85", names(lm.mean.mm))], function(x) x[, 2]))

comp.yearly.mm <- as.data.frame(do.call(cbind, lapply(comp.monthly.mm, colSums)))
rownames(comp.yearly.mm) <- c("1981-2000","2001-2020","2021-2040","2041-2060","2061-2080","2081-2100")

### plot 

comp.yearly.mm$TimePeriod <- rownames(comp.yearly.mm)
comp.yearly.mm <- pivot_longer(comp.yearly.mm, cols = -TimePeriod, names_to = "Scenario", values_to = "Value")

# Refined ggplot code for yearly summaries by scenario, emphasizing the plotting area
ggplot(comp.yearly.mm, aes(x = TimePeriod, y = Value, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  # Bars side by side with adjusted dodge width
  theme_minimal() +  # Clean theme
  labs(x = "", y = "[mm]", title = "Water Availability: Yearly means per Period\nObserved (1981-2020) and modeled data (2021-2100)")+
  scale_fill_manual(values = c("RCP26" = "#3CB4AC", "RCP45" = "#66C2A5", "RCP85" = "#008080")) +  # Teal color variations
  theme(
    panel.border = element_rect(fill = NA, color = "black", size = 0.5),  # Black border around the plotting area
    panel.background = element_blank(),  # Remove background fill from plotting area
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(vjust = 0.5),
  ) +
  scale_y_continuous(labels = function(x) format(x, nsmall = 1))  # Basic numeric formatting

####
#### for [m3/s]

lm.mean <- list()
for(i in 1:4){
  df <- lm.periods.monthly[grepl("RCP26",names(lm.periods.monthly))][[i]]
  name <- names(lm.periods.monthly[grepl("RCP26",names(lm.periods.monthly))][i])
  df <- data.frame(month=1:12,runoff=apply(df[,2:ncol(df)],MARGIN=1,FUN=mean))
  lm.mean[[name]] <- df
  rm(df)
}
for(i in 1:4){
  df <- lm.periods.monthly[grepl("RCP45",names(lm.periods.monthly))][[i]]
  name <- names(lm.periods.monthly[grepl("RCP45",names(lm.periods.monthly))][i])
  df <- data.frame(month=1:12,runoff=apply(df[,2:ncol(df)],MARGIN=1,FUN=mean))
  lm.mean[[name]] <- df
  rm(df)
}
for(i in 1:4){
  df <- lm.periods.monthly[grepl("RCP85",names(lm.periods.monthly))][[i]]
  name <- names(lm.periods.monthly[grepl("RCP85",names(lm.periods.monthly))][i])
  df <- data.frame(month=1:12,runoff=apply(df[,2:ncol(df)],MARGIN=1,FUN=mean))
  lm.mean[[name]] <- df
  rm(df)
}

# dataframe with col for each IPCC scenario 
comp.monthly <- list()
comp.monthly[["RCP26"]] <- cbind(sapply(obs.periods.monthly, function(x) x[, 2]),
                                 sapply(lm.mean[grepl("RCP26", names(lm.mean))], function(x) x[, 2]))
comp.monthly[["RCP45"]] <- cbind(sapply(obs.periods.monthly, function(x) x[, 2]),
                                 sapply(lm.mean[grepl("RCP45", names(lm.mean))], function(x) x[, 2]))
comp.monthly[["RCP85"]] <- cbind(sapply(obs.periods.monthly, function(x) x[, 2]),
                                 sapply(lm.mean[grepl("RCP85", names(lm.mean))], function(x) x[, 2]))

comp.yearly <- as.data.frame(do.call(cbind, lapply(comp.monthly, colSums)))
rownames(comp.yearly) <- c("1981-2000", "2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")


### plot 

comp.yearly$TimePeriod <- rownames(comp.yearly)
comp.yearly <- pivot_longer(comp.yearly, cols = -TimePeriod, names_to = "Scenario", values_to = "Value")

# Refined ggplot code for yearly summaries by scenario, emphasizing the plotting area
ggplot(comp.yearly, aes(x = TimePeriod, y = Value, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  # Bars side by side with adjusted dodge width
  theme_minimal() +  # Clean theme
  labs(x = "", y = "[m3/s]", title = "Water Availability (Streamflow): Yearly means per Period \nObserved (1981-2020) and modeled (2021-2100) data")+
  scale_fill_manual(values = c("RCP26" = "#3CB4AC", "RCP45" = "#66C2A5", "RCP85" = "#008080")) +  # Teal color variations
  theme(
    panel.border = element_rect(fill = NA, color = "black", size = 0.5),  # Black border around the plotting area
    panel.background = element_blank(),  # Remove background fill from plotting area
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(vjust = 0.5),
  ) +
  scale_y_continuous(labels = function(x) format(x, nsmall = 1))  # Basic numeric formatting





belp_modeled_daily <-  cbind(belp_modelled_RCP26, belp_modelled_RCP45[,2:ncol(belp_modelled_RCP45)],
                             belp_modelled_RCP85[,2:ncol(belp_modelled_RCP85)])

write.table(x = belp_modeled_daily, file =  "C:/Users/Simon/OneDrive - Universitaet Bern/03_Masterarbeit/water stress index/03_tables/runoff_daily_belp_modeled.csv",
            sep=";", dec=".")
