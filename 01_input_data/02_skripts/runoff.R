## runoff calculations 

runoff_daily_belp_obs <- belp_daily

runoff_decades <- tibble(sixties = rep(NA, (10*365+3)))
runoff_decades$sixties <- runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>=1960&year(runoff_daily_belp_obs$time)<1970]
runoff_decades$seventies <- c(runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>=1970&year(runoff_daily_belp_obs$time)<1980], NA)
runoff_decades$eighties <- runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>=1980&year(runoff_daily_belp_obs$time)<1990]
runoff_decades$nineties <- c(runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>=1990&year(runoff_daily_belp_obs$time)<2000],NA)
runoff_decades$zeroes <- runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>=2000&year(runoff_daily_belp_obs$time)<2010]
runoff_decades$tens <- c(runoff_daily_belp_obs$runoff[year(runoff_daily_belp_obs$time)>=2010&year(runoff_daily_belp_obs$time)<2020],NA)


runoff_decades <- gather(runoff_decades, key = "decade", value="runoff")

# define customcolors 
custom_colors <- c("sixties"="antiquewhite3","seventies"="antiquewhite3","eighties"="antiquewhite3",
                   "nineties"="antiquewhite3","zeroes"="antiquewhite3","tens"="antiquewhite3")

# Define custom legend labels
legend_labels <- c("sixties"="1960-1969","seventies"="1970-1979","eighties"="1980-1989",
                   "nineties"="1990-1999","zeroes"="2000-2009","tens"="2010-2020")

# Specify the order of boxplots on the x-axis
boxplot_order <- c("sixties",   "seventies", "eighties",  "nineties",  "zeroes",    "tens")



ggplot(runoff_decades, aes(x = factor(decade, levels = boxplot_order), y = runoff, fill = decade)) +
  geom_boxplot(color = "black", width = 0.7) +
  
  # Manual color and legend settings
  scale_fill_manual(values = custom_colors, labels = legend_labels) +
  scale_x_discrete(labels = legend_labels) + 
  theme_bw()+ 
  theme(legend.position = "none") +
  scale_y_log10()+
  labs(title = "Runoff Gürbe (daily means) \nBelp-Mühlimatte (BAFU ID: 2159)",
     x = NULL,
     y = "runoff [m3/s]")
ggsave("01_graphs/10_year_runoff_boxplots_log_scale.png", width = 5000, height = 3500, units = "px", dpi = 800)


ggplot(runoff_decades, aes(x = factor(decade, levels = boxplot_order), y = runoff, fill = decade)) +
  geom_boxplot(color = "black", width = 0.7) +
  
  # Manual color and legend settings
  scale_fill_manual(values = custom_colors, labels = legend_labels) +
  scale_x_discrete(labels = legend_labels) + 
  theme_bw()+ 
  ylim(c(0,10))+
  theme(legend.position = "none") +
  labs(title = "Runoff Gürbe (daily means) \nBelp-Mühlimatte (BAFU ID: 2159)",
       x = NULL,
       y = "runoff [m3/s]")
ggsave("01_graphs/10_year_runoff_boxplots_ord_scale.png", width = 5000, height = 3500, units = "px", dpi = 800)

rm(runoff_decades)

### runoff and precipitation graph 
runoff_annual_sums <- apply(runoff_belp_years[,2:(dim(runoff_belp_years)[2])],MARGIN=2,FUN=sum, na.rm=T) 
runoff_annual_sums <- tibble(year = c(1923:2018), 
                             runoff = runoff_annual_sums[1:(length(runoff_annual_sums)-1)])

runoff_precip_annual <- tibble(year=c(1989:2018),runoff=runoff_annual_sums$runoff[runoff_annual_sums$year>1988],
                               precip=rhires_precip_annual$precip[rhires_precip_annual$year>1988&rhires_precip_annual$year<=2018])
runoff_precip_annual$runoff <- scale(runoff_precip_annual$runoff)
runoff_precip_annual$precip <- scale(runoff_precip_annual$precip)


runoff_precip_annual <- gather(data = runoff_precip_annual, key="key",value = "value", -year)

cor_spearman <- cor(method = "spearman", x=runoff_precip_annual$runoff, y=runoff_precip_annual$precip)

# ### plot 
# p_scaled <- ggplot(data = runoff_precip_annual, aes(x=year, y=value,fill=key))+
#   geom_bar(position = position_dodge(0.7), stat = "identity", width = .7, alpha=0.9)+
#   scale_fill_manual(values = c("#0B60B0","#f49e89"))+
#   theme_bw()+
#   labs(title = NULL,
#        x = NULL,
#        y = NULL)+
#   scale_x_continuous(breaks = c(1989, seq(1995,2015,5),2018), expand = expansion(mult = c(0.01, .01)), 
#                      minor_breaks = c(1989:2018))+
#   scale_y_continuous(
#     expand = expansion(mult = c(0.03, .03)),
#     # Features of the first axis
#     name = "-",
#     
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~.*coeff, name="-"))+
#   theme(legend.position =   "none", 
#         axis.title.y.right = element_text(angle = 90, size = 10, color="white"), 
#         axis.title.y.left = element_text(size = 10, color = "white"))
#   #theme(legend.position = c(0.15, 0.1), legend.title = element_blank(), legend.direction = "horizontal")
# 
# rm(boxplot_order,custom_colors,legend_labels,belp_daily)
# 
# ### runoff and precipitation graph 
# 
# obs.yearly <- aggregate(runoff_daily_belp_obs$runoff,by=list(year(runoff_daily_belp_obs$time)),FUN=sum)
# obs.yearly <- merge(obs.yearly,aggregate(rhires$precip,by=list(year(rhires$date)),FUN=sum),
#       by.x = "Group.1",by.y = "Group.1", all.x = T)
# colnames(obs.yearly) <- c("year","runoff","precip")
# # Value used to transform the data

# A few constants
coeff <- 1


p_unscaled <- ggplot(data = obs.yearly[obs.yearly$year<2022&obs.yearly$year>1991,], aes(x=year)) +
  
  geom_bar( aes(y=precip), stat="identity", size=.1, fill="#0B60B0", alpha=.6) +
  geom_line( aes(y=runoff*coeff), size=2, color="#f49e89") +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.03, .03)),
    # Features of the first axis
    name = "annual precip [mm]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="annual runoff [m3/s]"))+
  labs(x=NULL, 
  title = "Annual Runoff Gürbe (Belp-Mühlimatte) and Precipitation (rhiresD)\n(1992-2021)") +
  
  
  scale_x_continuous(breaks = c(1989, seq(1995,2020,5)), 
                     expand = expansion(mult = c(0.01, .01)), 
                     minor_breaks = c(1989:2021))+
  theme_bw()+
  theme(axis.title.y.right = element_text(angle = 90, size = 10), 
        axis.title.y.left = element_text(size = 10),
        legend.position = c(0.15, 0.1), 
        legend.title = element_blank(), 
        legend.direction = "horizontal") 

print(p_unscaled)

ggsave("01_graphs/annual_precip_runoff_89_18.png", width = 5000, height = 3500, units = "px", dpi = 800)
rm(p_unscaled)



# ### plot 
# ggplot(data = runoff_precip_annual, aes(x=year, y=value,fill=key))+
#   geom_bar(position = position_dodge(0.9), stat = "identity", width = .9)+
#   scale_fill_manual(values = c("#0B60B0","#f49e89"))+
#   theme_bw()+
#   labs(title = "Annual Runoff Gürbe Belp-Mühlimatte (1989-2018)\nAnnual Precipitation rhiresD",
#        x = NULL,
#        y = "runoff [m3/s] / precip [mm]")+
#   scale_x_continuous(breaks = c(1989, seq(1995,2015,5),2018), expand = expansion(mult = c(0.01, .01)))+
#   scale_y_continuous(expand = expansion(mult = c(0.01, .01)))+
#   theme(legend.position = c(0.15, 0.21), legend.title = element_blank())




