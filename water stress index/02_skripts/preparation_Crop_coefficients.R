## crop coefficients 
library(RColorBrewer)
library(lubridate)

table_fields <- read.csv("00_data/irrigation/crop_coefficients.csv",encoding = "UTF-8",header = T, sep=";",dec = ".")

table_fields <- table_fields %>% filter(Irrigation..yes.no.=="ja") %>% 
  select(-KULTGRUT_K,-Kommentar,-Source,-kont)

## create table with months and doy in middle of month
month_doy <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  #DOY_Start = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335))
  DOY_Start = c(16,  47,  75, 106, 136, 167, 197, 228, 259, 289, 320, 350))

## merge month table with CC table to get year-round CC-values
table_fields <- table_fields %>% 
  merge(., month_doy, by.x = "gp_plant_date", by.y = "Month", all.x = T, all.y = F)

# add CC values 
crop_coeff <- table_fields %>% 
  mutate("zp_kc_ini" = (DOY_Start+gp_ini)) %>% 
  mutate("zp_kc_mid" = (DOY_Start+gp_ini+gp_crop_dev)) %>% 
  mutate("zp_kc_end_mid" = (DOY_Start+gp_ini+gp_crop_dev+gp_mid_season)) %>% 
  mutate("zp_kc_late" = (DOY_Start+gp_ini+gp_crop_dev+gp_mid_season+gp_late))

crop_coeff <- crop_coeff %>%
  select(KULTURT_KU,zp_kc_ini,zp_kc_mid,zp_kc_end_mid,zp_kc_late, kc_ini, kc_mid, kc_end, DOY_Start) 

crop_coeff <- crop_coeff %>% rename(zp_planting = DOY_Start)

# set culture-types as rownames  
rownames(crop_coeff) <- crop_coeff$KULTURT_KU
crop_coeff$KULTURT_KU <- NULL

# steigung gerade berechnen um punkte zu verbinden 
crop_coeff$m_dev <- (crop_coeff$kc_mid - crop_coeff$kc_ini)/(crop_coeff$zp_kc_mid- crop_coeff$zp_kc_ini)
crop_coeff$b_dev <- crop_coeff$kc_mid- crop_coeff$m_dev*crop_coeff$zp_kc_mid

# steigung gerade berechnen um punkte zu verbinden 
crop_coeff$m_late <- (crop_coeff$kc_end - crop_coeff$kc_mid)/(crop_coeff$zp_kc_late- crop_coeff$zp_kc_end_mid)
crop_coeff$b_late <- crop_coeff$kc_end- crop_coeff$m_late*crop_coeff$zp_kc_late


# construct new matrix with doy rows 
doy_crop_coeff <- matrix(data = NA, nrow = (366*2), ncol = dim(crop_coeff)[1])
colnames(doy_crop_coeff) <- rownames(crop_coeff)
doy_crop_coeff <- as.data.frame(doy_crop_coeff) 
# doy_crop_coeff %>% mutate(doy=1:366)


# calculate values per doy over 2 years 
for(i in 1:dim(crop_coeff)[1]){
  name <- rownames(crop_coeff)[i]
  # initial period 
  doy_crop_coeff[crop_coeff$zp_planting[i]:crop_coeff$zp_kc_ini[i],colnames(doy_crop_coeff)==name] <- crop_coeff$kc_ini[i]
  # dev period
  x <- crop_coeff$zp_kc_ini[i]:crop_coeff$zp_kc_mid[i]
  y <- crop_coeff$m_dev[i]*x+crop_coeff$b_dev[i]
  doy_crop_coeff[crop_coeff$zp_kc_ini[i]:crop_coeff$zp_kc_mid[i],colnames(doy_crop_coeff)==name] <- y
  # mid season
  doy_crop_coeff[crop_coeff$zp_kc_mid[i]:crop_coeff$zp_kc_end_mid[i],colnames(doy_crop_coeff)==name] <- crop_coeff$kc_mid[i]
  # late season 
  x <- crop_coeff$zp_kc_end_mid[i]:crop_coeff$zp_kc_late[i]
  y <- crop_coeff$m_late[i]*x+crop_coeff$b_late[i]
  doy_crop_coeff[crop_coeff$zp_kc_end_mid[i]:crop_coeff$zp_kc_late[i],colnames(doy_crop_coeff)==name] <- y
  # rm variables
  rm(x,y,name,i)
}

# distribute over one year 
anpassen <- colnames(doy_crop_coeff[,!is.na(doy_crop_coeff[367,])])

for (i in 1:length(anpassen)){
  new <- doy_crop_coeff[,colnames(doy_crop_coeff)==anpassen[i]]
  new <- new[367:length(new)]
  new <- new[!is.na(new)]
  doy_crop_coeff[1:length(new),colnames(doy_crop_coeff)==anpassen[i]] <- new
  rm(new,i)
}

# only keep year 1 
doy_crop_coeff <- doy_crop_coeff[1:366,]



## plot 
colors <- rainbow(dim(doy_crop_coeff)[2])


png("01_graphs/crop_coefficients_doy.png", width = 1000, height = 1500,res = 164)  
par(mfrow=c(8,2), mar = c(2, 2, 2, 2) + 0.1)

for(i in 1:dim(doy_crop_coeff)[2]){
  name <- colnames(doy_crop_coeff)[i]
  plot(x=1:366,y=doy_crop_coeff[,i], col=colors[i], type = "l", 
       lwd=2, ylim=c(0.2,1.2), xlim=c(0,366),
       main = name)
}
rm(colors,name,anpassen)
dev.off()




### irrigated area (=in growing period) mean crop coefficient
colnames(doy_crop_coeff)
list_area <- table_fields %>% select(KULTURT_KU,area..m.)

doy_combined <- rep(NA,366)
irrigated_area <- rep(NA,366)

# define irrigated area for each day of the year
# crops outside growing period -> no irrigation 
for (i in 1:366){
  
  row <- doy_crop_coeff[i,!is.na(doy_crop_coeff[i,])]
  area <- 0
  for(k in 1:dim(row)[2]){
    name <- colnames(row)[k]
    area_new <- list_area$area..m.[list_area$KULTURT_KU==name]
    row[1,k] <- area_new*row[1,k]
    area <- area + area_new
    
  }
  kc_combined <- sum(row[1,])/area
  doy_combined[i] <- kc_combined
  irrigated_area[i] <- area
  rm(row,kc_combined,i,k,area,area_new)
}

# overview harvest date 
harvest <- data.frame(
  culture = rownames(crop_coeff),
  harvest_date = as.Date(ymd("2021-01-01") + days(crop_coeff$zp_kc_late) - 1))

doy_combined <- data.frame(
  doy=1:366,
  kc_overall = doy_combined,
  irrigated_area = irrigated_area
)
rm(irrigated_area)

## create weekly aggregation 
list_woy <- as.Date(ymd("2012-01-01") + days(doy_combined$doy) - 1) %>% week() %>% 
  as.character() %>% gsub("53","52",.) %>% as.numeric()
woy_combined <- aggregate(x = doy_combined[,2:3], 
          by = list(list_woy),
          FUN = mean)
colnames(woy_combined) <- c("woy","kc_overall","irrigated_area")
rm(list_woy)
woy_combined$irrigated_area_ha <- woy_combined$irrigated_area/10000


## -- plot 
factor_scaling <- 500
ggplot(data = woy_combined, aes(woy,irrigated_area_ha/factor_scaling))+
  geom_hline(yintercept = 1, linetype="dotted", color = "red")+
  geom_area(fill="#249EA0", color = "#005F60",alpha=0.5, linewidth=0.75) +
  geom_line(aes(y=kc_overall), color="#FD5901", linewidth =2)+
  # Add a second axis and specify its features
  scale_y_continuous(sec.axis=sec_axis(transform =~.*factor_scaling,name = "cultivated area [h]"), 
                     name = "av. weighted crop coefficient (Kc)",
                     expand = expansion(mult = c(0.01, .1))) +
  
  scale_x_continuous(breaks = seq(1,52,(52/12))+(52/12)/2,
                     labels = month_doy$Month,
                     expand = expansion(mult = c(0.01, .01)))+
  
  
  # theme & title
  theme_bw()+
  #ggtitle("Gürbe Catchment: Average Crop Coefficient & Irrigated Area\nAggregated to weekly scale")+
  labs(x="")+
  theme(panel.grid.major.x = element_line(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank())+
  annotate("segment", x = 0.5, xend = 53, y = 0, yend = 0, color = "black", size = 1, linetype = "solid") +
  # add text to plot 
  geom_text(aes(x = 15.5, y = 2.5, label = "cultivated area [h]"), color = "#005F60", hjust = -0.1, vjust = -0.5)+
  geom_text(aes(x = 4, y = .6,label = "average weighted crop coeff. (Kc)"), color = "#FD5901", hjust = -0.1, vjust = -0.5)

ggsave("01_graphs/irrigated_area_mean_kc.png")


## write results as csv

write.table(doy_combined, paste("03_tables/Irrigation/", Sys.Date(), "_crop_coefficients_doy_combined.csv",sep="") , sep=";",
            dec=".")

write.table(crop_coeff, paste("03_tables/Irrigation/", "_crop_coefficients.csv",sep="") , sep=";",
            dec=".")

write.table(doy_crop_coeff, paste("03_tables/Irrigation/", "doy_crop_coefficients.csv",sep="") , sep=";",
            dec=".")

rm(crop_coeff,doy_crop_coeff,harvest,list_area,month_doy,table_fields,woy_combined,factor_scaling,kc_overall,name)
