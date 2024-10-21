# date 
rhires_date <- read.table("00_data/gridded_meteo_data/date_2159_1961_2020.txt")
rhires_date <- paste(rhires_date[,1],rhires_date[,2],rhires_date[,3], sep = "/") %>% 
  dmy()
# add 2021
rhires_date <- c(rhires_date,seq(ymd("2021-01-01"),ymd("2021-12-31"), by="1 day"))

# TabsD Daily Mean Temperatures 
rhires_temp <- read.table("00_data/gridded_meteo_data/tabsD_2159_1961_2021.txt")
rhires_temp <- rhires_temp %>% apply(.,MARGIN=1,FUN = mean)

# SrelD Sonnenscheindauer 
rhires_sunshine <- read.table("00_data/gridded_meteo_data/srelD_2159_1971_2021.txt")
rhires_sunshine <- rhires_sunshine %>% apply(.,MARGIN=1,FUN = mean)

# rhiresD Niederschlag 
rhires_precip <- read.table("00_data/gridded_meteo_data/rhiresD_2159_1961_2021.txt")
rhires_precip <- rhires_precip %>% apply(.,MARGIN=1,FUN = mean)

# create data frame 
rhires <- data.frame(rhires_date,rhires_precip, rhires_temp)
colnames(rhires) <- c("date","precip","temp")
rm(rhires_precip, rhires_date, rhires_temp)


# add sunshine to data frame 
rhires$sun <- rep(NA,dim(rhires)[1])
rhires$sun[rhires$date>=ymd("1971-01-01")] <- rhires_sunshine
rm(rhires_sunshine)


# daten zollikofen  
zollikofen <- read.table("00_data/meteodaten/zollikofen/zollikofen_data.txt", dec = ".", header = T, skip = 1)
zollikofen$time <- zollikofen$time %>% ymd(.)

colnames(zollikofen) <- c("stn", "date", "ET_total", "glob_rad", "ref_E_FAO", "longwave_rad",
                          "t_daily_max", "t_daily_min", "temp", "humidity", "E")

zollikofen$t_daily_min <- zollikofen$t_daily_min %>% as.numeric(.) 


# add meteorological season 
rhires$season <- sapply(X=month(rhires$date),FUN=get_meteorological_season)




get_meteorological_season <- function(month) {
  if (!is.numeric(month) || month < 1 || month > 12) {
    stop("Month must be a numeric value between 1 and 12.")
  }
  
  if (month %in% c(3, 4, 5)) {
    return("MAM")
  } else if (month %in% c(6, 7, 8)) {
    return("JJA")
  } else if (month %in% c(9, 10, 11)) {
    return("SON")
  } else {
    return("DJF")
  }
}


write.csv(rhires,"rhires_data_daily_mean.csv")
