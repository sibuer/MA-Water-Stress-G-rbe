
baseflow_belp <- belp_daily %>% filter(year(belp_daily$time)>=1994&year(belp_daily$time)<=2023) %>% as_tibble()

duncan_bf <- BaseFlowSeparation(streamflow = baseflow_belp$runoff, bf_method = "Duncan",
                                k=0.97, c=quantile(baseflow_belp$runoff,0.25),filter_parameter = 0.97)

# plot(c(1:300),duncan_bf$qflow[12101:12400], type ="l", col="blue")
# lines(c(1:300),duncan_bf$bt[12101:12400], type ="l", col="red", lty=1)
# lines(c(1:300),duncan_bf$mn_1[12101:12400], type ="l", col="red", lty=2)

baseflow_belp$baseflow <- duncan_bf$bt 
rm(duncan_bf)

baseflow_belp$season <- sapply(X=month(baseflow_belp$time),FUN = get_meteorological_season)

baseflow_belp <- baseflow_belp[lubridate::year(baseflow_belp$time)>=1994,]




### plot 
baseflow_belp_agg <- data.frame(
  season = aggregate(x = as.vector(baseflow_belp$runoff), by=list(baseflow_belp$season), FUN = sum)[1],
  runoff = aggregate(x = as.vector(baseflow_belp$runoff), by=list(baseflow_belp$season), FUN = sum)[2],
  baseflow = aggregate(x = as.vector(baseflow_belp$baseflow), by=list(baseflow_belp$season), FUN = sum)[2]
  )

colnames(baseflow_belp_agg) <- c("season",   "runoff",   "baseflow")

baseflow_belp_agg$per_baseflow <- paste(round(baseflow_belp_agg$baseflow/baseflow_belp_agg$runoff,2),"%", sep = "")

# plot 
p <- ggplot(baseflow_belp_agg, aes(x = season, y = runoff))
baseflow_belp_agg$season <- factor(baseflow_belp_agg$season, levels = c("DJF", "MAM", "JJA", "SON"))
# inhalte 
p <- p + geom_bar(stat = "identity", position = "dodge", width = 0.7, fill = "grey")
p <- p + geom_bar(aes(y = baseflow, fill = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))), 
                  stat = "identity", position = "dodge", width = 0.7, fill = c("#37abb4", "#7ed2bb", "#f49e89", "#d49e4a"))
# text 
p <- p + geom_text(aes(label = per_baseflow, y = baseflow + 200), position = position_dodge(width = 0.7), vjust = -0.5)
# box etc 
p <- p + theme_bw()
# labs
p <- p + scale_y_continuous(expand = expansion(mult = c(0.01, .1)))
p <- p + labs(x = element_blank(), y = "Total runoff (1994-2023) [m3/s]")
p <- p + ggtitle("Baseflow Index (BFI)\nGürbe Belp-Mühlimatte (1994-2023)")
# edit general plot-visuals
p <- p + theme(plot.margin = margin(r = 15, l = 5, b=5, t = 5))  # Adjust the margin on the right side
p <- p + theme(panel.grid.major.y = element_line(size = 0.25, colour = "grey"),
               panel.grid.major.x = element_blank())

plot(p)
ggsave("01_graphs/baseflow_index_season_total_Belp.png", width = 3500, height = 3500, units = "px", dpi = 800)


### plot 
baseflow_belp_month <- data.frame(
  month = aggregate(x = as.vector(baseflow_belp$runoff), by=list(lubridate::month(baseflow_belp$date)), FUN = sum)[1],
  runoff = aggregate(x = as.vector(baseflow_belp$runoff), by=list(lubridate::month(baseflow_belp$date)), FUN = sum)[2],
  baseflow = aggregate(x = as.vector(baseflow_belp$baseflow), by=list(lubridate::month(baseflow_belp$date)), FUN = sum)[2])
colnames(baseflow_belp_month) <- c("month",   "runoff",   "baseflow")

baseflow_belp_month$per_baseflow <- paste(round(baseflow_belp_month$baseflow/baseflow_belp_month$runoff,2),"%", sep = "")
baseflow_belp_month$per_baseflow_num <- baseflow_belp_month$baseflow/baseflow_belp_month$runoff
baseflow_belp_month$month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

baseflow_belp_month$month <- factor(baseflow_belp_month$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# plot 
p <- ggplot(baseflow_belp_month, aes(x = month, y = per_baseflow_num))
p <- p + geom_line(aes(y=per_baseflow_num, group=1), linewidth=1.1)
p <- p + theme_bw()
# labs
p <- p + scale_y_continuous(expand = expansion(mult = c(0.01, .1)))
p <- p + labs(x = element_blank(), y = "Baseflow Index (BFI)")
p <- p + ggtitle("Baseflow Index (BFI) of total runoff\nGürbe Belp-Mühlimatte (1994-2023)")
# edit general plot-visuals
p <- p + theme(plot.margin = margin(r = 15, l = 5, b=5, t = 5)) + # Adjust the margin on the right side
    ylim(.5,.75)
p <- p + theme(panel.grid.major.y = element_line(size = 0.25, colour = "grey"),
               panel.grid.major.x = element_blank())

plot(p)
ggsave("01_graphs/baseflow_index_monthly_total_Belp.png", width = 3500, height = 3500, units = "px", dpi = 800)

rm(baseflow_belp_agg,baseflow_belp_month)



### ROLLMEAN 


baseflow_belp <- tibble(runoff = runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>=1970],
                        date = runoff_daily_belp_obs$time[year(runoff_daily_belp_obs$time)>=1970])

duncan_bf <- BaseFlowSeparation(streamflow = baseflow_belp$runoff, bf_method = "Duncan",
                                k=0.97, c=quantile(baseflow_belp$runoff,0.25),filter_parameter = 0.97)

baseflow_belp$baseflow <- duncan_bf$bt 
rm(duncan_bf)

baseflow_belp$season <- sapply(X=month(baseflow_belp$date),FUN = get_meteorological_season)

baseflow_belp <- baseflow_belp[lubridate::year(baseflow_belp$date)>=1979,]



### baseflow yearly 

bfi_yearly <- aggregate( 
  x = baseflow_belp$baseflow,
  by = list(lubridate::year(baseflow_belp$date)),
  FUN=sum
)
bfi_yearly$runoff <- aggregate( 
  x = baseflow_belp$runoff,
  by = list(lubridate::year(baseflow_belp$date)),
  FUN=sum
)[[2]]
colnames(bfi_yearly) <- c("year","baseflow","runoff")
bfi_yearly$bfi <- bfi_yearly$baseflow/bfi_yearly$runoff

# bfi_summer$fiveyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=5, align = "center")
# bfi_summer$fifteenyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=15, align = "center")
# rm(season)

#### Plot baseflow Summer 
season <- "JJA"
bfi_summer <- aggregate( 
  x = baseflow_belp$baseflow[baseflow_belp$season==season],
  by = list(lubridate::year(baseflow_belp$date[baseflow_belp$season==season])),
  FUN=sum
  )
bfi_summer$runoff <- aggregate( 
  x = baseflow_belp$runoff[baseflow_belp$season==season],
  by = list(lubridate::year(baseflow_belp$date[baseflow_belp$season==season])),
  FUN=sum
)[[2]]
colnames(bfi_summer) <- c("year","baseflow","runoff")
bfi_summer$bfi <- bfi_summer$baseflow/bfi_summer$runoff

bfi_summer$fiveyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=5, align = "center")
bfi_summer$fifteenyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=15, align = "center")
rm(season)

# Create a ggplot object
p <- ggplot(bfi_summer, aes(x = year))
# bars 
p <- p + geom_bar(aes(y = bfi),fill="#f49e89", stat = "identity", show.legend = F) # Add a bar plot for 'values'
p <- p + coord_cartesian(ylim=c(.3,1),xlim=c(1980,2022))

#plot
p <- p + geom_line(aes(y = fiveyearrollmean, group =1, color = "5-Year rollmean"),
                   linewidth = 1, linetype = "solid", alpha = 0.7) # Add a line plot for 'rollmean'
p <- p + geom_line(aes(y = fifteenyearrollmean, group =1, color = "15-Year rollmean"),
                   linewidth = 1.2) # Add a line plot for 'rollmean'
p <- p + scale_color_manual(values = c("15-Year rollmean" = "#111111", "5-Year rollmean" = "grey"))

p <- p + labs(color = NULL)
# edit axis-visuals
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p <- p + scale_y_continuous(expand = expansion(mult = c(0.1, .1)))
# labs
p <- p + labs(x = element_blank(), y = "Baseflow Index (BFI)")
p <- p + ggtitle("Gürbe, Belp-Mühlimatte: BFI Summer\nSeasonal sums (JJA; 1979-2023)")
# edit general plot-visuals
# p <- p + theme(plot.margin = margin(l = 5, b=5, t = 5))
# p <- p + theme(panel.grid.major.y = element_line(size = 0.25, colour = "grey"),
#                panel.grid.major.x = element_blank())
# p <- p + theme(plot.background = element_rect(fill = "white"),
#                panel.background = element_rect(fill = "white", color = "black"))
p <- p + theme_bw()
p <- p + theme(legend.position = c(0.2, 0.85))
# Display the plot
print(p)
ggsave("01_graphs/baseflow_index_JJA_74_18_Belp.png", width = 3500, height = 3500, units = "px", dpi = 800)

#### Plot baseflow Autumn 
season <- "SON"
bfi_summer <- aggregate( 
  x = baseflow_belp$baseflow[baseflow_belp$season==season],
  by = list(lubridate::year(baseflow_belp$date[baseflow_belp$season==season])),
  FUN=sum
)
bfi_summer$runoff <- aggregate( 
  x = baseflow_belp$runoff[baseflow_belp$season==season],
  by = list(lubridate::year(baseflow_belp$date[baseflow_belp$season==season])),
  FUN=sum
)[[2]]
colnames(bfi_summer) <- c("year","baseflow","runoff")
bfi_summer$bfi <- bfi_summer$baseflow/bfi_summer$runoff

bfi_summer$fiveyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=5, align = "center")
bfi_summer$fifteenyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=15, align = "center")
rm(season)

# Create a ggplot object
p <- ggplot(bfi_summer, aes(x = year))
# bars 
p <- p + geom_bar(aes(y = bfi), fill="#d49e4a", stat = "identity", show.legend = F) # Add a bar plot for 'values'
p <- p + coord_cartesian(ylim=c(.3,1),xlim=c(1980,2022))
#p <- p + scale_color_manual(values = c(""))
#plot
p <- p + geom_line(aes(y = fiveyearrollmean, group =1, color = "5-Year rollmean"),
                   linewidth = 1, linetype = "solid", alpha = 0.7) # Add a line plot for 'rollmean'
p <- p + geom_line(aes(y = fifteenyearrollmean, group =1, color = "15-Year rollmean"),
                   linewidth = 1.2) # Add a line plot for 'rollmean'
p <- p + scale_color_manual(values = c("15-Year rollmean" = "#111111", "5-Year rollmean" = "grey"))

p <- p + labs(color = NULL)
# edit axis-visuals
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p <- p + scale_y_continuous(expand = expansion(mult = c(0.1, .1)))
# labs
p <- p + labs(x = element_blank(), y = "Baseflow Index (BFI)")
p <- p + ggtitle("Gürbe, Belp-Mühlimatte: BFI Autumn\nSeasonal sums (SON; 1979-2023)")
# edit general plot-visuals
# p <- p + theme(plot.margin = margin(l = 5, b=5, t = 5))
# p <- p + theme(panel.grid.major.y = element_line(size = 0.25, colour = "grey"),
#                panel.grid.major.x = element_blank())
# p <- p + theme(plot.background = element_rect(fill = "white"),
#                panel.background = element_rect(fill = "white", color = "black"))
p <- p + theme_bw()
p <- p + theme(legend.position = c(0.2, 0.85))
# Display the plot
print(p)
ggsave("01_graphs/baseflow_index_SON_74_18_Belp.png", width = 3500, height = 3500, units = "px", dpi = 800)


#### Plot baseflow Spring 
season <- "MAM"
bfi_summer <- aggregate( 
  x = baseflow_belp$baseflow[baseflow_belp$season==season],
  by = list(lubridate::year(baseflow_belp$date[baseflow_belp$season==season])),
  FUN=sum
)
bfi_summer$runoff <- aggregate( 
  x = baseflow_belp$runoff[baseflow_belp$season==season],
  by = list(lubridate::year(baseflow_belp$date[baseflow_belp$season==season])),
  FUN=sum
)[[2]]
colnames(bfi_summer) <- c("year","baseflow","runoff")
bfi_summer$bfi <- bfi_summer$baseflow/bfi_summer$runoff

bfi_summer$fiveyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=5, align = "center")
bfi_summer$fifteenyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=15, align = "center")
rm(season)

# Create a ggplot object
p <- ggplot(bfi_summer, aes(x = year))
# bars 
p <- p + geom_bar(aes(y = bfi), fill="#7ed2bb", stat = "identity", show.legend = F) # Add a bar plot for 'values'
p <- p + coord_cartesian(ylim=c(.3,1),xlim=c(1980,2022))
#p <- p + scale_color_manual(values = c(""))
#plot
p <- p + geom_line(aes(y = fiveyearrollmean, group =1, color = "5-Year rollmean"),
                   linewidth = 1, linetype = "solid", alpha = 0.7) # Add a line plot for 'rollmean'
p <- p + geom_line(aes(y = fifteenyearrollmean, group =1, color = "15-Year rollmean"),
                   linewidth = 1.2) # Add a line plot for 'rollmean'
p <- p + scale_color_manual(values = c("15-Year rollmean" = "#111111", "5-Year rollmean" = "grey"))

p <- p + labs(color = NULL)
# edit axis-visuals
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p <- p + scale_y_continuous(expand = expansion(mult = c(0.1, .1)))
# labs
p <- p + labs(x = element_blank(), y = "Baseflow Index (BFI)")
p <- p + ggtitle("Gürbe, Belp-Mühlimatte: BFI Spring\nSeasonal sums (MAM; 1979-2023)")
# edit general plot-visuals
# p <- p + theme(plot.margin = margin(l = 5, b=5, t = 5))
# p <- p + theme(panel.grid.major.y = element_line(size = 0.25, colour = "grey"),
#                panel.grid.major.x = element_blank())
# p <- p + theme(plot.background = element_rect(fill = "white"),
#                panel.background = element_rect(fill = "white", color = "black"))
p <- p + theme_bw()
p <- p + theme(legend.position = c(0.2, 0.85))
# Display the plot
print(p)
ggsave("01_graphs/baseflow_index_MAM_74_18_Belp.png", width = 3500, height = 3500, units = "px", dpi = 800)



#### Plot baseflow Winter 
season <- "DJF"
bfi_summer <- aggregate( 
  x = baseflow_belp$baseflow[baseflow_belp$season==season],
  by = list(lubridate::year(baseflow_belp$date[baseflow_belp$season==season])),
  FUN=sum
)
bfi_summer$runoff <- aggregate( 
  x = baseflow_belp$runoff[baseflow_belp$season==season],
  by = list(lubridate::year(baseflow_belp$date[baseflow_belp$season==season])),
  FUN=sum
)[[2]]
colnames(bfi_summer) <- c("year","baseflow","runoff")
bfi_summer$bfi <- bfi_summer$baseflow/bfi_summer$runoff

bfi_summer$fiveyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=5, align = "center")
bfi_summer$fifteenyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=15, align = "center")
rm(season)

# Create a ggplot object
p <- ggplot(bfi_summer, aes(x = year))
# bars 
p <- p + geom_bar(aes(y = bfi), fill="#37abb4", stat = "identity", show.legend = F) # Add a bar plot for 'values'
p <- p + coord_cartesian(ylim=c(.3,1),xlim=c(1980,2022))
#p <- p + scale_color_manual(values = c(""))
#plot
p <- p + geom_line(aes(y = fiveyearrollmean, group =1, color = "5-Year rollmean"),
                   linewidth = 1, linetype = "solid", alpha = 0.7) # Add a line plot for 'rollmean'
p <- p + geom_line(aes(y = fifteenyearrollmean, group =1, color = "15-Year rollmean"),
                   linewidth = 1.2) # Add a line plot for 'rollmean'
p <- p + scale_color_manual(values = c("15-Year rollmean" = "#111111", "5-Year rollmean" = "grey"))

p <- p + labs(color = NULL)
# edit axis-visuals
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p <- p + scale_y_continuous(expand = expansion(mult = c(0.1, .1)))
# labs
p <- p + labs(x = element_blank(), y = "Baseflow Index (BFI)")
p <- p + ggtitle("Gürbe, Belp-Mühlimatte: BFI Winter\nSeasonal sums (DJF; 1979-2023)")
# edit general plot-visuals
# p <- p + theme(plot.margin = margin(l = 5, b=5, t = 5))
# p <- p + theme(panel.grid.major.y = element_line(size = 0.25, colour = "grey"),
#                panel.grid.major.x = element_blank())
# p <- p + theme(plot.background = element_rect(fill = "white"),
#                panel.background = element_rect(fill = "white", color = "black"))
p <- p + theme_bw()
p <- p + theme(legend.position = c(0.2, 0.85))
# Display the plot
print(p)
ggsave("01_graphs/baseflow_index_DJF_74_18_Belp.png", width = 3500, height = 3500, units = "px", dpi = 800)




#### Plot baseflow all year 
bfi_summer <- aggregate( 
  x = baseflow_belp$baseflow,
  by = list(lubridate::year(baseflow_belp$date)),
  FUN=sum
)
bfi_summer$runoff <- aggregate( 
  x = baseflow_belp$runoff,
  by = list(lubridate::year(baseflow_belp$date)),
  FUN=sum
)[[2]]
colnames(bfi_summer) <- c("year","baseflow","runoff")
bfi_summer$bfi <- bfi_summer$baseflow/bfi_summer$runoff

bfi_summer$fiveyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=5, align = "center")
bfi_summer$fifteenyearrollmean <- zoo::rollmean(bfi_summer$bfi,na.pad = T,k=15, align = "center")
rm(season)

# Create a ggplot object
p <- ggplot(bfi_summer, aes(x = year))
# bars 
p <- p + geom_bar(aes(y = bfi), fill="antiquewhite3", stat = "identity", show.legend = F) # Add a bar plot for 'values'
p <- p + coord_cartesian(ylim=c(.3,1),xlim=c(1980,2022))
#p <- p + scale_color_manual(values = c(""))
#plot
p <- p + geom_line(aes(y = fiveyearrollmean, group =1, color = "5-Year rollmean"),
                   linewidth = 1, linetype = "solid", alpha = 0.7) # Add a line plot for 'rollmean'
p <- p + geom_line(aes(y = fifteenyearrollmean, group =1, color = "15-Year rollmean"),
                   linewidth = 1.2) # Add a line plot for 'rollmean'
p <- p + scale_color_manual(values = c("15-Year rollmean" = "#111111", "5-Year rollmean" = "darkgrey"))

p <- p + labs(color = NULL)
# edit axis-visuals
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p <- p + scale_y_continuous(expand = expansion(mult = c(0.1, .1)))
# labs
p <- p + labs(x = element_blank(), y = "Baseflow Index (BFI)")
p <- p + ggtitle("Gürbe, Belp-Mühlimatte: Base Flow Index (BFI)\nSeasonal sums (1979-2023)")

p <- p + theme_bw()
p <- p + theme(legend.position = c(0.2, 0.85))
# Display the plot
print(p)
ggsave("01_graphs/baseflow_index_allyear_74_18_Belp.png", width = 3500, height = 3500, units = "px", dpi = 800)




rm(bfi_summer,df_long,p,plot_col, baseflow_belp,bfi_yearly)
rm()
