library(zoo)

## drinking water need 
drinking_water <- read.table("03_tables/drinking_water_need.csv",sep = ";",header = T,dec = ".", encoding = "UTF-8")

drinking_water <- drinking_water %>% select(monthly_water_need_mm, Year)

# adjust factor if i.e. say 20% of water can be reused through cleansing in ARA put 80%  
factor_reusable_water <- 1

drinking_water$mon_need_mm_corr <- drinking_water$monthly_water_need_mm * factor_reusable_water 
colnames(drinking_water) <- c("monthly_mm","year","monthly_corr_mm")


### ----------------------------------------------------------------------------
# Industrial Water 
### ----------------------------------------------------------------------------

industrial_water <- read.table("03_tables/industry_water_demand.csv",sep = ";",header = T,dec = ".", encoding = "UTF-8")
industrial_water <- industrial_water %>% select(Year,sec_monthly_water_need_mm,third_monthly_water_need_mm)

industrial_water[industrial_water$sec_monthly_water_need_mm==0|industrial_water$third_monthly_water_need_mm==0,] <- NA

industrial_water$total_monthly_wn_mm <- 
  industrial_water$sec_monthly_water_need_mm + industrial_water$third_monthly_water_need_mm

industrial_water
colnames(industrial_water)[1]<-"year"

### yearly timeseries
water <- data.frame(year=seq(1990,2090,1))
water <- merge(water, drinking_water[,c(2,3)], by="year",all.x = T, all.y = T) 
water <- merge(water, industrial_water[,c(1,4)], by="year",all.x = T, all.y = T) 
colnames(water) <- c("year","drinking","industrial")

water$drinking <- zoo::na.approx(water$drinking,maxgap = 20)
water$industrial <- zoo::na.approx(water$industrial,maxgap = 10)

## - clear 
rm(factor_reusable_water,drinking_water,industrial_water)
