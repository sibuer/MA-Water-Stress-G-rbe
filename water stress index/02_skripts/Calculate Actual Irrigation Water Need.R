library(zoo)
crop_coefficents <- read.table(file =  "03_tables/Irrigation/2024-09-11_crop_coefficients_doy_combined.csv",
                               header = T, sep=";",dec = ".")
precipitation <- read.table(file="C:/Users/Simon/OneDrive - Universitaet Bern/03_Masterarbeit/data/daten/BAFUCH2018/pr_catchment_means.csv",
                            header = T, sep = ";", dec = ".")


# prepare and combine date & crop coefficient dataframe 
date_df <- data.frame(date=ET0_CTM$date)
date_df$doy <- yday(date_df$date)
date_df <- merge(date_df,crop_coefficents,by.x = "doy", by.y = "doy") # merge to get Kc-values
date_df <- date_df[order(date_df$date), ] # re-order dataframe with dates 


# get the ETc dataframe 
ETc_CTM <- data.frame(date=date_df$date,
                      lapply(ET0_CTM[2:dim(ET0_CTM)[2]], function(x) x * date_df$kc_overall))
colnames(ETc_CTM) <- gsub("\\.","_", colnames(ETc_CTM))

# get the rolling mean dataframe for the precip 
k = 7 # Define the window size for the rolling mean

# Applying the right-centered rolling mean to each column
precip_rollmeans <- data.frame(lapply(precipitation[2:ncol(precipitation)], 
                                      function(column) rollapplyr(column, k, mean, na.rm = TRUE, fill = NA, align = "right")))

# getting the irrigation demand per day and model 
irrigation_need_CTM <- data.frame(date = ETc_CTM$date)
for(i in 2:ncol(ETc_CTM)){
  irg_name <- colnames(ETc_CTM)[i]
  irg_ETc <- ETc_CTM[,i]
  irg_precip <-  precip_rollmeans[,grep(irg_name,colnames(precip_rollmeans))]
  irg_irr_need <- irg_ETc - irg_precip
  irg_irr_need[irg_irr_need<0] <- 0
  irrigation_need_CTM[[irg_name]] <- irg_irr_need
}

# rem. newly created variables 
irg_vars <- grep("irg", ls(), value = TRUE)
rm(list = irg_vars)

# multiplicate with area to recieve demand in Liters 
irrigation_need_CTM <- data.frame(date=date_df$date,
                                  lapply(irrigation_need_CTM[2:dim(irrigation_need_CTM)[2]], function(x) x * date_df$irrigated_area))

# divide by Catchment size to get demand in l/m2 (== mm catchment-wide) 
size_catchment_m2 <-  116.1 * 10^6
irrigation_need_CTM <- data.frame(date=date_df$date,
                                  lapply(irrigation_need_CTM[2:dim(irrigation_need_CTM)[2]], function(x) x / size_catchment_m2))


periods <-data.frame(start=c(1990,2020,2045),end=c(2019,2049,2074))





