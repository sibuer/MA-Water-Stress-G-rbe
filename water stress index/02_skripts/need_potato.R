library(zoo)
library(dplyr)
library(lubridate)


doy_crop_coeff <- read.csv(file="03_tables/doy_crop_coeff.csv")
potatos <- data.frame(
  doy = c(1:366),
  cc = doy_crop_coeff$Zuckerrüben
)
  


# prepare and combine date & crop coefficient dataframe 
date_df <- data.frame(date=ET0_CTM$date)
date_df$doy <- yday(date_df$date)
date_df <- merge(date_df,potatos,by.x = "doy", by.y = "doy") # merge to get Kc-values
date_df <- date_df[order(date_df$date), ] # re-order dataframe with dates 


# get the ETc dataframe 
ETc_CTM_potato <- data.frame(date=date_df$date,
                      lapply(ET0_CTM[2:dim(ET0_CTM)[2]], function(x) x * date_df$cc))
colnames(ETc_CTM_potato) <- gsub("\\.","_", colnames(ETc_CTM))

# get the rolling mean dataframe for the precip 
k = 7 # Define the window size for the rolling mean

# getting the irrigation demand per day and model 
irrigation_need_CTM_potato <- data.frame(date = ETc_CTM$date)
for(i in 2:ncol(ETc_CTM_potato)){
  irg_name <- colnames(ETc_CTM_potato)[i]
  irg_ETc <- ETc_CTM_potato[,i]
  irg_precip <-  precip_rollmeans[,grep(irg_name,colnames(precip_rollmeans))]
  irg_irr_need <- irg_ETc - irg_precip
  irg_irr_need[irg_irr_need<0] <- 0
  irrigation_need_CTM_potato[[irg_name]] <- irg_irr_need
}

# rem. newly created variables 
irg_vars <- grep("irg", ls(), value = TRUE)
rm(list = irg_vars)



yearly_sums(irrigation_need_CTM_potato, periods = periods)


