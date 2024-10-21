# (duncan 2019)

library(tidyr)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)




### get longer data
runoff_daily_belp_obs <- belp_daily


#NM7Q 
# define hydrological years 1. April - 31. März 
runoff_daily_belp_obs$hydr_year <- rep(NA,dim(runoff_daily_belp_obs)[1])
for(i in 1923:2023){
  start <- ymd(paste(as.numeric(i),04,01, sep="-")) # def. period
  end <- ymd(paste(as.numeric(i+1),03,31, sep="-")) # def. period
  name <- as.numeric(i) # define name for hydr year 
  runoff_daily_belp_obs$hydr_year[runoff_daily_belp_obs$time>=start&
                                    runoff_daily_belp_obs$time<=end]<-name #enter values
  rm(start,end,name) #rem. temp. variables
}

# define function 
nm7q <- function(x,date){
  df <- data.frame(date,x) #create df to show date of nm7q
  df$sevendaysaverage <- zoo::rollmean(x,k=7,na.pad = T, align = "center") # 7-day-average
  return(list(df[which.min(df$sevendaysaverage),1],
               df[which.min(df$sevendaysaverage),3])) #return 
}

belp_nm7q <- data.frame(date=NA, nm7q=NA, hydr_year=NA)[numeric(0), ] # create empty df
belp_nm7q$date <- lubridate::as_date(belp_nm7q$date) # define date as date-class

# use function <- adjust year here in for loop if neccessary 
for (i in 1974:2023){
  df <- runoff_daily_belp_obs[runoff_daily_belp_obs$hydr_year==i,]
  val <- as.numeric(nm7q(x=df$runoff,date=df$time)[[2]]) # Wert des nm7q
  date <- ymd(nm7q(x=df$runoff,date=df$time)[[1]]) # Datum des nm7q
  row <- c(date,val,i)
  belp_nm7q[nrow(belp_nm7q)+1,] <- row # reihe hinzufügen 
  rm(df, val,date,i,row) # überflüssige variablen löschen 
}



# add 15-year mean 
belp_nm7q$fiftenyearmean <- zoo::rollmean(x=belp_nm7q$nm7q,k=15,align = "center",na.pad = T)
# add 5-year mean 
belp_nm7q$fiveyearmean <- zoo::rollmean(x=belp_nm7q$nm7q,k=5,align = "center",na.pad = T)

# add column with season of nm7q 
get_meteorological_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("DJF")
  } else if (month %in% c(3, 4, 5)) {
    return("MAM")
  } else if (month %in% c(6, 7, 8)) {
    return("JJA")
  } else {
    return("SON")
  }
}

belp_nm7q$season <- sapply(X=belp_nm7q$date,FUN = get_meteorological_season)
belp_nm7q$hydr_year_factor <- as.factor(belp_nm7q$hydr_year)
belp_nm7q$season <- as.factor(belp_nm7q$season)

### PLOT 15-Y ROLLMEAN NM7Q

# Create a ggplot object
ggplot(belp_nm7q, aes(x = hydr_year_factor))+
  geom_bar(aes(y = nm7q, fill=season), stat = "identity")+ # Add a bar plot for 'values'
  scale_fill_manual(values = c("#37abb4", "#7ed2bb", "#f49e89","#d49e4a"), 
                           breaks = c("DJF", "MAM", "JJA","SON"))+

# line
  geom_line(aes(y = fiftenyearmean, group =1, color = "15 y. running mean"),
                   linewidth = 1.2)+ # Add a line plot for 'rollmean'
  scale_color_manual(values = c("15 y. running mean" = "#111111"))+
  labs(color = NULL)+
  # edit axis-visuals
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_discrete(breaks = levels(belp_nm7q$hydr_year_factor)[seq(1, length(levels(belp_nm7q$hydr_year_factor)), by = 2)])+
  scale_y_continuous(expand = expansion(mult = c(0.01, .1)))+
  # labs
  labs(x = "Hydrological Year (April-March)", y = "NM7Q [m3/s]")+
  ggtitle("NM7Q Gürbe Belp-Mühlimatte (1974-2023)")+
  # edit general plot-visuals
  theme(panel.grid.major.y = element_line(size = 0.25, colour = "grey"),
        panel.grid.major.x = element_blank())+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", color = "black"))



ggsave("01_graphs/NM7Q.png",width = 7, height=5)





mean(belp_nm7q$nm7q)
sd(belp_nm7q$nm7q)



# define function 
nm14q <- function(x,date){
  df <- data.frame(date,x) #create df to show date of nm7q
  df$sevendaysaverage <- zoo::rollmean(x,k=14,na.pad = T, align = "center") # 7-day-average
  return(list(df[which.min(df$sevendaysaverage),1],
              df[which.min(df$sevendaysaverage),3])) #return 
}

belp_nm14q <- data.frame(date=NA, nm14q=NA, hydr_year=NA)[numeric(0), ] # create empty df
belp_nm14q$date <- lubridate::as_date(belp_nm14q$date) # define date as date-class

# use function <- adjust year here in for loop if neccessary 
for (i in 1974:2023){
  df <- runoff_daily_belp_obs[runoff_daily_belp_obs$hydr_year==i,]
  val <- as.numeric(nm14q(x=df$runoff,date=df$time)[[2]]) # Wert des nm7q
  date <- ymd(nm14q(x=df$runoff,date=df$time)[[1]]) # Datum des nm7q
  row <- c(date,val,i)
  belp_nm14q[nrow(belp_nm14q)+1,] <- row # reihe hinzufügen 
  rm(df, val,date,i,row) # überflüssige variablen löschen 
}



# add 15-year mean 
belp_nm14q$fiftenyearmean <- zoo::rollmean(x=belp_nm14q$nm14q,k=15,align = "center",na.pad = T)
# add 5-year mean 
belp_nm14q$fiveyearmean <- zoo::rollmean(x=belp_nm14q$nm14q,k=5,align = "center",na.pad = T)

# add column with season of nm7q 
get_meteorological_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("DJF")
  } else if (month %in% c(3, 4, 5)) {
    return("MAM")
  } else if (month %in% c(6, 7, 8)) {
    return("JJA")
  } else {
    return("SON")
  }
}

belp_nm14q$season <- sapply(X=belp_nm14q$date,FUN = get_meteorological_season)
belp_nm14q$hydr_year_factor <- as.factor(belp_nm14q$hydr_year)
belp_nm14q$season <- as.factor(belp_nm14q$season)

### PLOT 15-Y ROLLMEAN NM7Q

# Create a ggplot object
ggplot(belp_nm14q, aes(x = hydr_year_factor))+
  geom_bar(aes(y = nm14q, fill=season), stat = "identity")+ # Add a bar plot for 'values'
  scale_fill_manual(values = c("#37abb4", "#7ed2bb", "#f49e89","#d49e4a"), 
                    breaks = c("DJF", "MAM", "JJA","SON"))+
  
  # line
  geom_line(aes(y = fiftenyearmean, group =1, color = "15-Year rollmean"),
            linewidth = 1.2,color="black")+ # Add a line plot for 'rollmean'
  labs(color = NULL)+
  # edit axis-visuals
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_discrete(breaks = levels(belp_nm7q$hydr_year_factor)[seq(1, length(levels(belp_nm7q$hydr_year_factor)), by = 2)])+
  scale_y_continuous(expand = expansion(mult = c(0.01, .1)))+
  # labs
  labs(x = "Hydrological Year (April-March)", y = "NM14Q [m3/s]")+
  ggtitle("NM14Q Gürbe Belp-Mühlimatte (1974-2023)")+
  # edit general plot-visuals
  theme(panel.grid.major.y = element_line(size = 0.25, colour = "grey"),
        panel.grid.major.x = element_blank())+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", color = "black"))



ggsave("01_graphs/NM14Q.png",width = 7, height=5)



mean(belp_nm14q$nm14q)
#