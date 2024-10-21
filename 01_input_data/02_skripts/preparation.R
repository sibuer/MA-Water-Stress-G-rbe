# packages 
library(data.table)
library(dplyr)
library(stringr)
library(readr)
library(xts)
library(lubridate)
library(zoo)
library(tidyr)
library(ggplot2)
library(remotes)
library(EcoHydRology)
library(RNWMStat)
library(magrittr)
library(lmtest)
library(Evapotranspiration)
library(SPEI)
library(sirad)
library(ggpubr)

# Belp Runoff 1922-2022
belp_daily <- read.table(paste("00_data/runoff_belp_2159/", list.files("00_data/runoff_belp_2159")[1], sep = ""), 
                 skip = 8, dec = ".", sep = ";", header = T, row.names = NULL)[,c(7,9)]
belp_daily$time <- ymd_hms(belp_daily$Zeitstempel) # convert to r time 
belp_daily <- belp_daily[belp_daily$time>ymd("1922/12/31")&belp_daily$time<ymd("2023/01/01"),]
belp_daily$Zeitstempel<- NULL
belp_daily <- data.frame(belp_daily[,2],belp_daily[,1])
colnames(belp_daily) <- c("time","runoff")

# Belp Runoff 2023
belp_daily_2023 <- read.table(paste("00_data/runoff_belp_2159/", list.files("00_data/runoff_belp_2159")[2], sep = ""), 
                                    skip = 9, dec = ".", sep = ";", header = F, row.names = NULL)[,c(7,9)]
belp_daily_2023$V7 <- ymd_hms(belp_daily_2023$V7)
colnames(belp_daily_2023) <- c("time","runoff")

# combine df's 
belp_daily <- rbind(belp_daily,belp_daily_2023)
rm(belp_daily_2023)



# define 20 year Intervals 
periods <- data.frame(
  start = seq(2021, 2100, by = 20),
  end = seq(2021, 2100, by = 20) + 19)  # Add 19 to get the end year of each period
  



  
  
  
  
  
  
  
  
  
 



#### ----------------------------------- 
# )
# # burgistein 
# runoff_daily_burgistein_obs <- read.table(paste("00_data/runoff_burgistein_a102/", list.files("00_data/runoff_burgistein_a102")[2], 
#            sep = ""), sep = "\t", dec = ".", header = T, skip = 3)
# # prepare data 
# runoff_daily_burgistein_obs$time <- dmy(runoff_daily_burgistein_obs$X.Datum) # convert to r time 
# runoff_daily_burgistein_obs 
# runoff_daily_burgistein_obs <- runoff_daily_burgistein_obs[runoff_daily_burgistein_obs$time>ymd("2008/12/31") 
#                                                &runoff_daily_burgistein_obs$time<ymd("2019/01/01"),c(8,2)]
# colnames(runoff_daily_burgistein_obs) <- c("time","runoff")
# 
# 
# 
# ## detect possible systemic errors in the data
# yearly_means <- as.data.frame(apply(runoff_belp_years,FUN=mean,MARGIN = 2, na.rm=T))
# yearly_means <- data.frame(year=c(1923:2018), mean=yearly_means[2:(dim(yearly_means)[1]-1),])
# 
# 
# firstquantile <- as.vector(apply(runoff_belp_years,FUN=quantile,MARGIN = 2, na.rm=T, 0.25))
# firstquantile <- firstquantile[2:(length(firstquantile)-1)] 
# 
# yearly_means$firstquantile <- firstquantile
# rm(firstquantile)
# 
# thirdquantile <- as.vector(apply(runoff_belp_years,FUN=quantile,MARGIN = 2, na.rm=T, 0.75))
# thirdquantile <- thirdquantile[2:(length(thirdquantile)-1)] 
# 
# yearly_means$thirdquantile <- thirdquantile
# rm(thirdquantile)
# 
# 
# med <- as.vector(apply(runoff_belp_years,FUN=median,MARGIN = 2, na.rm=T))
# med <- med[2:(length(med)-1)] 
# yearly_means$med <- med
# rm(med)
# 
# 
# yearly_means$fiftenyearmean <- zoo::rollmean(x=yearly_means$mean,k=15,align = "center", na.pad = T)
# yearly_means$fiftenyearfirstquantile <- zoo::rollmean(x=yearly_means$firstquantile,k=15,align = "center", na.pad = T)
# yearly_means$fiftenyearthirdquantile <- zoo::rollmean(x=yearly_means$thirdquantile,k=15,align = "center", na.pad = T)
# yearly_means$fiftenyearmedian <- zoo::rollmean(x=yearly_means$med ,k=15,align = "center", na.pad = T)
# 
# par(mfrow=c(2,2))
# 
# 
# plot(x=1923:2018,y=yearly_means$fiftenyearmean, type="h", xlim = c(1930,2011), main = "15y-mean", ylab = "[m3/s]")
# plot(x=1923:2018,y=yearly_means$fiftenyearfirstquantile, type="h", xlim = c(1930,2011), main = "15y-0.25quant", ylab = "[m3/s]")
# plot(x=1923:2018,y=yearly_means$fiftenyearmedian, type="h", xlim = c(1930,2011), main = "15y-median", ylab = "[m3/s]")
# plot(x=1923:2018,y=yearly_means$thirdquantile, type="h", xlim = c(1930,2011), main = "15y-0.75qant", ylab = "[m3/s]")
# 
# # chow test 
# rhires %>% spread(data=as.numeric(precip),key=as.string(lubridate::year(date)))
# aggregate(rhires$precip, by=as.list(lubridate::year(rhires$date)), FUN=sum)
# 
# yearly_rhires <- aggregate(rhires$precip, by=list(year = as.factor(lubridate::year(rhires$date))), FUN=sum)
# yearly_rhires$fiftenyearmeansumprecip <- zoo::rollmean(x=yearly_rhires$x,k=15,align = "center", na.pad = T)
# 
# par(mfrow=c(2,1))
# plot(x=1923:2018,y=yearly_means$fiftenyearmean, type="h", xlim = c(1969,2011), main = "15y-mean-mean-runoff", ylab = "[m3/s]")
# plot(x=1961:2020,y=yearly_rhires$fiftenyearmeansumprecip, type="h", xlim = c(1968,2011), 
#      main = "15y-mean-sum-precip", ylab = "[mm]", col ="lightblue")
# 
# yearly_rhires$year <- as.character(yearly_rhires$year) %>% as.numeric(.)
# 
# #1968-2011
# png(units = "mm",filename="01_graphs/15y_mean_comparison_runoff_belp_precipitation_rhires.png", 
#     width = 165, height = 100,res = 150)
# 
# 
# par(mfrow=c(1,1))
# plot(x=1968:2011,y=scale(yearly_means$fiftenyearmean[yearly_means$year>=1968 & yearly_means$year<=2011]), 
#      type="h", xlim = c(1968,2011), main = "15-yearly annual means: precip rhiresD (annual sum); 
#      runoff Belp-Mühlimatte (annual mean)", ylab = "scaled [mm] & [m3/s]",
#      xlab=c(""))
# lines(x=1968:2011,y=scale(yearly_rhires$fiftenyearmeansumprecip[yearly_rhires$year>=1968 & yearly_rhires$year<=2011]), 
#       type="l", col = "blue")
# corr <- cor(scale(yearly_means$fiftenyearmean[yearly_means$year>=1968 & yearly_means$year<=2011]),
#             scale(yearly_rhires$fiftenyearmeansumprecip[yearly_rhires$year>=1968 & yearly_rhires$year<=2011]), method = "spearman")
# 
# text(1973,0.7,paste("spearman-corr. = ", round(corr,2), sep =""), cex = 0.7)
# 
# legend(x=1967,y=2, # Position of the legend
#        legend = c("Precipitation", "Runoff"), # Labels
#        col = c("blue", "black"), # Colors
#        lty = c(1, 1), # Line types
#        cex=0.8) 
# 
# dev.off()
# 
# ### analysis 
# # define timestep(s)
# data_t1 <- runoff_daily_belp_obs[runoff_daily_belp_obs$time>=ymd("1988/01/01") 
#                                  &runoff_daily_belp_obs$time<=ymd("2018/12/31"),]
# # show yearly flow
# mean_daily_belp_t1 <- aggregate(data_t1$runoff, by = list(substring(data_t1$time,6,10)), mean)
# median_daily_belp_t1 <- aggregate(data_t1$runoff, by = list(substring(data_t1$time,6,10)), median)
# 
# colnames(mean_daily_belp_t1) <- c("day","mean_runoff")
# mean_daily_belp_t1$day <- lubridate::as_date(ymd("2000/01/01"):ymd("2000/12/31"))
# 
# colnames(median_daily_belp_t1) <- c("day","median_runoff")
# median_daily_belp_t1$day <- lubridate::as_date(ymd("2000/01/01"):ymd("2000/12/31"))
# 
# ## add quantils
# apply(median_daily_belp_t1,MARGIN = 1, FUN = quantile, .9)
# 
# 
# # write function that adds quantiles, mean and median to table and returns table
# 
# 
# 
# # box plot 
# par(mar = c(5, 4, 4, 2) + 0.1)
# plot(x=median_daily_belp_t1$day,y=median_daily_belp_t1$median_runoff, type = "l", 
#      xlab = "", ylab = "runoff [m3/s]", xaxt = "n", col = "white", 
#      xlim = c(lubridate::as_date(median_daily_belp_t1$day[1]),lubridate::as_date(median_daily_belp_t1$day[366])),
#      ylim = c(0,5), 
#      main = "Median Runoff Belp 1988-2018 (Station ID: 2159) ")
# ticks <- ymd(c("2000-01-01", "2000-02-01", "2000-03-01", "2000-04-01", "2000-05-01", "2000-06-01", "2000-07-01", "2000-08-01",
#   "2000-09-01", "2000-10-01", "2000-11-01", "2000-12-01","2000-12-31"))
# axis(1, at = ticks, labels = format(ticks, "%b"), cex.axis = 0.7)
# abline(v=ticks, col = "lightgrey")
# abline(h=0:5, col = "lightgrey")
# #lines 
# lines(median_daily_belp_t1$day,median_daily_belp_t1$median_runoff)
# 
# 
# 
# # Create the ggplot time series plot
# ggplot(median_daily_belp_t1, aes(x = day, y = median_runoff)) +
#   geom_line() +
#   scale_x_date(
#     date_breaks = "1 month",        # Ticks for each month
#     date_labels = "%b",             # Display month abbreviations
#     expand = c(0, 0),               # Remove extra space between axis and plot
#     limits = c(as_date(median_daily_belp_t1$day[1]), as_date(median_daily_belp_t1$day[366])) # Set the x-axis limits
#   ) +
#   labs(
#     x = "Month",                    # X-axis label
#     y = "runoff",                    # Y-axis label
#     title = "Time Series Plot"      # Plot title
#   ) +
#   theme_minimal()                   # Apply a minimal theme (you can customize this
# 
# 
# ### FLOW DURATION CURVE
# 
# #Sorting discharges in decreasing order
# flow <- sort(mean_daily_belp_t1$median_runoff, decreasing = T)
# 
# 
# #Creating a data frame in which x column is hte percent of ie less than a specific time and
# #y is the correspondent discharge.
# flow_duration_belp_t1 <-data.frame(x=366/length(flow)*1:length(flow),y=flow)
# 
# #Plot
# plot(x = flow_duration_belp_t1$x, y = flow_duration_belp_t1$y, type = "l",
#      ylab="runoff [m3/s]",xlab="Number of days flow is exceeded",main="Flow Duration Curve \n Gürbe, Belp Mühlimatt (1988-2018)")
# grid()
# 
# ggplot(flow_duration_belp_t1, aes(x = x, y = y)) +
#   geom_line() +
#   
#   # DEFINE THEMES
#   theme(panel.border = element_rect(color = "black", fill = NA, size = 1),)+
#   
#   
#   labs(
#     x = "Number of days (annual) flow is exceeded",                    # X-axis label
#     y = "runoff [m3/s]",                    # Y-axis label
#     title = "Time Series Plot"      # Plot title
#   ) 
# 
# 
# 
# #lines(mean_daily_belp_t1$day,mean_daily_belp_t1$mean_runoff)
# 
# # distribution 
# 
# #### RECESSION COEFFICIENT / COMPARISION
# #####
# 
# # format data for ggplot
# runoff_daily_belp_obs$year <- substring(runoff_daily_belp_obs$time,1,4) #%>% as.numeric(.)
# runoff_daily_belp_obs$doy <- runoff_daily_belp_obs$time %>% lubridate::yday(.)
# 
# runoff_belp_years <- runoff_daily_belp_obs[,2:4] %>%
#   pivot_wider(names_from = year, values_from = runoff)
# colnames(runoff_belp_years) <- c("doy",paste("y_",1923:2018,sep=""))
# 
# runoff_belp_years$median_t1 <- median_daily_belp_t1$median_runoff
# #####
# # PLOT 4 years (1995,200,2003,2018)
# #####
# 
# ggplot(runoff_belp_years, aes(x = doy)) +
#   
#   # X-AXIS
#   geom_vline(xintercept = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
#              linetype = "solid", color = "grey", size=0.2) +
#   scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
#                      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
#                      limits = c(0,367), expand = c(0, 0))+
#   
#   # Y-AXIS
#   geom_hline(yintercept = c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30), 
#              linetype = "solid", color = "grey", size=0.2)+
#   scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0,15))+
#   
#   
#   # DATA-LINES 
#   geom_line(aes(y = median_t1, color = "Median"), size = 1) +
#   geom_line(aes(y = y_2018, color = "2018"), size = 0.5) +
#   geom_line(aes(y = y_2003, color = "2003"), size = 0.5) +
#   geom_line(aes(y = y_2000, color = "2000"), size = 0.5) +
#   geom_line(aes(y = y_1995, color = "1995"), size = 0.5) +
#   scale_color_manual(values = c("Median" = "grey", "2018" = "red", "2003" = "red",
#                                 "2000" = "blue", "1995" = "blue")) +
#   labs(color = "Year") +
#   
#   # DEFINE THEMES
#   theme(legend.position = "right", panel.grid.minor = element_blank(), 
#         panel.border = element_rect(color = "black", fill = NA, size = 1), 
#         panel.background = element_rect(fill = NA, colour="white"))+
#   guides(color = guide_legend(title = "Year")) + 
#   
#   # TITLES
#   ylab("Runoff [m3/s]") +
#   ggtitle("Runoff Belp-Mühlimatte (2159); Median (1988-2018)") +
#   theme(axis.title.x = element_blank())  # Remove x-axis title
#   
# 
# 
# 
# #####
# # Plot all years and 2003 and 2018 orange
# #####
# data_t1$year <- substring(data_t1$time,1,4)
# data_t1$doy <- data_t1$time %>% lubridate::yday(.)
# 
# ggplot(data_t1, aes(x = doy, y = runoff, color = year)) +
#   
#   # X-AXIS
#   geom_vline(xintercept = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
#              linetype = "solid", color = "grey", size=0.2) +
#   scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
#                      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
#                      limits = c(0,367), expand = c(0, 0))+
#   
#   # Y-AXIS
#   geom_hline(yintercept = c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30), 
#              linetype = "solid", color = "grey", size=0.2)+
#   scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0,15))+
#   
#   # DATA
#   geom_line(size = 0.5) +
#   labs(color = "Year") +
#   scale_color_manual(values = c("2018" = "orange", "2003" = "orange", "grey")) +
#   theme(legend.position = "right") +
#   ggtitle("Main Title") +
#   ylab("Y-Axis Title") +
#   xlab(NULL)  # Remove x-axis label
# 
# ## further code
