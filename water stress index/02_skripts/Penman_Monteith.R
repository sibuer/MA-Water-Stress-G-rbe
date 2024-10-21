# Penman Monteith 

library(FAO56)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(cowplot)
## define constants 
albedo <- 0.23  # albedo 
gamma <- 0.0652 # psychemtrische Konstante


## -----------------------------------------------------------------------------
## --- calculate ET0 BERN (PM)
deg_dec_lat <- 46.99
phi <- DD2Rad(phi_deg = deg_dec_lat) # latitude [rad]
z <- 553 # height Zollikofen Meteostation 

ET0_BER <- data.frame(date = tas_CTM$day)

for (i in 2:(dim(hurs_BER)[2])){

  name <- colnames(hurs_BER)[i] %>% gsub("_BER","",.)
  
  # read in necessary values (for corresponding climate models)
  pm_tas <- tas_BER[,grep(name, colnames(tas_BER))] %>% as.numeric() # [C°]
  pm_hurs <- hurs_BER[,grep(name, colnames(hurs_BER))] %>% as.numeric()  #[%]
  pm_rsds <- rsds_BER[,grep(name, colnames(rsds_BER))] %>% as.numeric() #[W/m2]
  pm_tasmax <- tasmax_BER[,grep(name, colnames(tasmax_BER))] %>% as.numeric() # [C°] 
  pm_tasmin <- tasmin_BER[,grep(name, colnames(tasmin_BER))] %>% as.numeric()# [C°]
  pm_wind <- wind_BER[,grep(name, colnames(wind_BER))] %>% as.numeric() # [m/s]
  pm_date <- ymd(tas_BER$date) 
  
  # check validity thorugh histograms of input variables
  png(paste("01_graphs/val_check/","hist_input_BER_",name,".png",sep=""))
  par(mfrow=c(2,2))
  hist(pm_tas, col="red")
  legend("topleft", legend=paste("NA's:", sum(is.na(pm_tas))),cex = 0.8)
  hist(pm_hurs,col="green")
  legend("topleft", legend=paste("NA's:", sum(is.na(pm_hurs))),cex = 0.8)
  hist(pm_rsds,col="yellow")
  legend("topright", legend=paste("NA's:", sum(is.na(pm_rsds))),cex = 0.8)
  hist(pm_wind,col="lightblue")
  legend("topright", legend=paste("NA's:", sum(is.na(pm_wind))),cex = 0.8)
  dev.off()
  
  ## -- correct hurs to % 
  # pm_hurs <- pm_hurs
  
  ### ---- Vapour pressure and wind 
  pm_eoT <- FAO56::SatVP(Temp = pm_tas) # Saturation vapour pressure [KPa]
  pm_delta <- FAO56::SlpSVPC(Temp = pm_tas) # slope of saturation vapour pressure curve at air temperature T [kPa °C-1]
  #pm_wind <- abs(pm_wind) # take Betrag-values because negative Values are included 
  pm_e_a <- pm_eoT*pm_hurs/100 # actual vapour pressure
  
  # adjust / calculate further variables 
  pm_rsds <- pm_rsds*0.0864 # convert from W/m2 to [MJ / m2 / day]
  pm_rad_ns <- pm_rsds*(1-albedo) # Net shortwave radiation [MJ / m2 / day]

  
  
  
  pm_d_r <- FAO56::EarSunDis(ET0_BER$date)                # inverse relative earth sun distance 
  pm_SolDec <- FAO56::SolDec(ET0_BER$date)                # solar declination 
  pm_sunHA <- FAO56::SunHA(phi = phi, delta = pm_SolDec)  # sunset hour angle 
  
  # Extraterrestrial radiation for daily periods [MJ/(m2 x day)]
  pm_rad_a <- FAO56::ExRad(d_r = pm_d_r,
                           omega_s = pm_sunHA,
                           phi = phi,
                           delta = pm_SolDec)
  
  # clear Sky solar radiaton 
  pm_rad_so <- FAO56::CSSRad(elev = z, R_a =  pm_rad_a) 
  
  # actual vapour pressure 
  
  
  
  # Net longwave radiation   
  pm_rad_nl <- FAO56::NLRad(T_max = pm_tasmax, 
                            T_min = pm_tasmin, 
                            R_s = pm_rsds, 
                            R_so = pm_rad_so,
                            e_a = pm_e_a)                                
  
  # Net Radiation 
  pm_rad_n <- FAO56::NRad(R_ns = pm_rad_ns, R_nl = pm_rad_nl)
  
  # G (soil heat flux) -> Formel aus Calanca et al. 2019 
  pm_g <- 0.159*pm_rad_n-0.987
  
  
 
  
  
  
  
  # Warnen vor Fehlern
  ifelse(length(pm_tas)==0|length(pm_tasmin)==0|length(pm_tasmax)==0|length(pm_tas)==0,
         yes = print(paste("Achtung Fehler",name, i)),
         no = print(paste("alles ok: ",i)))
  
  
  # calculate ET0 
  pm_ET0 <- FAO56::ETo_FPM(
    Delta = pm_delta, 
    T_mean = pm_tas,
    R_n = pm_rad_n,
    G = pm_g, # soil heat flux 
    gamma = gamma,
    u_2 = pm_wind,
    e_s <- pm_eoT,
    T_min = pm_tasmin,
    T_max = pm_tasmax,
    date = pm_date,
    e_a = pm_e_a
  )
  
  ET0_BER[[name]] <- pm_ET0
  
  # rm variables
  pm_vars <- grep("pm", ls(), value = TRUE)
  rm(list = pm_vars)
}


apply(X = ET0_BER[,2:ncol(ET0_BER)], MARGIN=2,FUN=function(x)mean(x,na.rm=T))*366


#### ---------------------------------------------------------------------------
# calculate for Catchment values 

deg_dec_lat <- 46.841
phi <- DD2Rad(phi_deg = deg_dec_lat) # latitude [rad]
z <- 600 # height -> because agricultural areas lie in valley floor 

ET0_CTM <- data.frame(date = tas_CTM$day)

for (i in 2:(dim(hurs_BER)[2])){
  
  name <- colnames(hurs_BER)[i] %>% gsub("_BER","",.) %>% gsub("_QMstations_1981_2099","",.)
  
  # read in necessary values (for corresponding climate models)
  pm_tas <- tas_CTM[,grep(name, colnames(tas_CTM))] %>% as.numeric() # [C°]
  pm_hurs <- hurs_BER[,grep(name, colnames(hurs_BER))] %>% as.numeric()  #[%]
  pm_rsds <- rsds_BER[,grep(name, colnames(rsds_BER))] %>% as.numeric() #[W/m2]
  pm_tasmax <- tasmax_CTM[,grep(name, colnames(tasmax_BER))] %>% as.numeric() # [C°] 
  pm_tasmin <- tasmin_CTM[,grep(name, colnames(tasmin_BER))] %>% as.numeric()# [C°]
  pm_wind <- wind_BER[,grep(name, colnames(wind_BER))] %>% as.numeric() # [m/s]
  pm_date <- ymd(tas_BER$date) 
  
  # check validity thorugh histograms of input variables
  png(paste("01_graphs/val_check/","hist_input_CTM_",name,".png",sep=""))
  par(mfrow=c(2,2))
  hist(pm_tas, col="red")
  legend("topleft", legend=paste("NA's:", sum(is.na(pm_tas))),cex = 0.8)
  hist(pm_hurs,col="green")
  legend("topleft", legend=paste("NA's:", sum(is.na(pm_hurs))),cex = 0.8)
  hist(pm_rsds,col="yellow")
  legend("topright", legend=paste("NA's:", sum(is.na(pm_rsds))),cex = 0.8)
  hist(pm_wind,col="lightblue")
  legend("topright", legend=paste("NA's:", sum(is.na(pm_wind))),cex = 0.8)
  dev.off()
  
  ## -- correct hurs to % 
  # pm_hurs <- pm_hurs
  
  ### ---- Vapour pressure and wind 
  pm_eoT <- FAO56::SatVP(Temp = pm_tas) # Saturation vapour pressure [KPa]
  pm_delta <- FAO56::SlpSVPC(Temp = pm_tas) # slope of saturation vapour pressure curve at air temperature T [kPa °C-1]
  #pm_wind <- abs(pm_wind) # take Betrag-values because negative Values are included 
  pm_e_a <- pm_eoT*pm_hurs/100 # actual vapour pressure
  
  # adjust / calculate further variables 
  pm_rsds <- pm_rsds*0.0864 # convert from W/m2 to [MJ / m2 / day]
  pm_rad_ns <- pm_rsds*(1-albedo) # Net shortwave radiation [MJ / m2 / day]
  
  
  
  
  pm_d_r <- FAO56::EarSunDis(ET0_BER$date)                # inverse relative earth sun distance 
  pm_SolDec <- FAO56::SolDec(ET0_BER$date)                # solar declination 
  pm_sunHA <- FAO56::SunHA(phi = phi, delta = pm_SolDec)  # sunset hour angle 
  
  # Extraterrestrial radiation for daily periods [MJ/(m2 x day)]
  pm_rad_a <- FAO56::ExRad(d_r = pm_d_r,
                           omega_s = pm_sunHA,
                           phi = phi,
                           delta = pm_SolDec)
  
  # clear Sky solar radiaton 
  pm_rad_so <- FAO56::CSSRad(elev = z, R_a =  pm_rad_a) 
  
  # actual vapour pressure 
  
  
  
  # Net longwave radiation   
  pm_rad_nl <- FAO56::NLRad(T_max = pm_tasmax, 
                            T_min = pm_tasmin, 
                            R_s = pm_rsds, 
                            R_so = pm_rad_so,
                            e_a = pm_e_a)                                
  
  # Net Radiation 
  pm_rad_n <- FAO56::NRad(R_ns = pm_rad_ns, R_nl = pm_rad_nl)
  
  # G (soil heat flux) -> Formel aus Calanca et al. 2019 
  pm_g <- 0.159*pm_rad_n-0.987
  
  
  
  
  
  
  
  # Warnen vor Fehlern
  ifelse(length(pm_tas)==0|length(pm_tasmin)==0|length(pm_tasmax)==0|length(pm_tas)==0,
         yes = print(paste("Achtung Fehler",name, i)),
         no = print(paste("alles ok: ",i)))
  
  
  # calculate ET0 
  pm_ET0 <- FAO56::ETo_FPM(
    Delta = pm_delta, 
    T_mean = pm_tas,
    R_n = pm_rad_n,
    G = pm_g, # soil heat flux 
    gamma = gamma,
    u_2 = pm_wind,
    e_s <- pm_eoT,
    T_min = pm_tasmin,
    T_max = pm_tasmax,
    date = pm_date,
    e_a = pm_e_a
  )
  
  ET0_CTM[[name]] <- pm_ET0
  
  # rm variables
  pm_vars <- grep("pm", ls(), value = TRUE)
  rm(list = pm_vars)
}


apply(X = ET0_CTM[,2:ncol(ET0_CTM)], MARGIN=2,FUN=function(x)mean(x,na.rm=T))*366


## ----------------------------------------------------------------------------


## - Hargreaves 
ET0_CTM_Har <- data.frame(date = tas_CTM$day)

phi <- DD2Rad(phi_deg = deg_dec_lat) # latitude [rad]
for (i in 2:(dim(hurs_BER)[2])){
  
  name <- colnames(hurs_BER)[i] %>% gsub("_BER","",.) %>% gsub("_QMstations_1981_2099","",.)
  
  # read in necessary values (for corresponding climate models)
  har_tas <- tas_CTM[,grep(name, colnames(tas_CTM))] %>% as.numeric() # [C°]
  har_tasmax <- tasmax_CTM[,grep(name, colnames(tasmax_BER))] %>% as.numeric() # [C°] 
  har_tasmin <- tasmin_CTM[,grep(name, colnames(tasmin_BER))] %>% as.numeric()# [C°]
  har_date <- ymd(tas_BER$date) 

  har_d_r <- FAO56::EarSunDis(ET0_BER$date)                # inverse relative earth sun distance 
  har_SolDec <- FAO56::SolDec(ET0_BER$date)                # solar declination 
  har_sunHA <- FAO56::SunHA(phi = phi, delta = har_SolDec)  # sunset hour angle 
  
  # Extraterrestrial radiation for daily periods [MJ/(m2 x day)]
  har_rad_a <- FAO56::ExRad(d_r = har_d_r,
                           omega_s = har_sunHA,
                           phi = phi,
                           delta = har_SolDec)

  # Warnen vor Fehlern
  ifelse(length(har_tas)==0|length(har_tasmin)==0|length(har_tasmax)==0|length(har_tas)==0,
         yes = print(paste("Achtung Fehler",name, i)),
         no = print(paste("alles ok: ",i)))
  
  
  # calculate ET0 
  har_ET0 <- FAO56::ETo_Hrg(T_min = har_tasmin, 
                            T_max = har_tasmax, 
                            R_a = har_rad_a
    
  )
  
  ET0_CTM_Har[[name]] <- har_ET0
  
  # rm variables
  har_vars <- grep("har", ls(), value = TRUE)
  rm(list = har_vars)
}

apply(X = ET0_CTM_Har[,2:ncol(ET0_CTM_Har)], MARGIN=2,FUN=function(x)mean(x,na.rm=T))*366

## ----------------------------------------------------------------------------
##### plot differences ET0 with tas BER-CTM

png("01_graphs/comparision_ET0_PM.png", height = 2000,width = 1500,res = 164)

par(mfrow=c(6,3))
for(i in 1:dim(periods)[1]){

  sub_BER <- ET0_BER  %>% filter(year(date)>=periods[i,1]&year(date)<=periods[i,2])
  sub_CTM <- ET0_CTM  %>% filter(year(date)>=periods[i,1]&year(date)<=periods[i,2])
  
  rcp_list <- c("RCP26","RCP45","RCP85")
  
  for(k in 1:3){
    sub_sub_BER <- sub_BER[,c(1,grep(rcp_list[k], colnames(sub_BER)))]
    sub_sub_CTM <- sub_CTM[,c(1,grep(rcp_list[k], colnames(sub_CTM)))]
    
    name <- paste(periods[i,1],periods[i,2],rcp_list[k],sep="-")
    
    plot(x=c(1:12),y=rep(0,12),col="white", main = name,ylim = c(0,10),
         xlab = "red= Temp Bern; blue = Temp Catchment", ylab = "mean ET0 [mm]")
    for(x in 2:dim(sub_sub_BER)[2]){
      lines(x=c(1:12),
            y = aggregate(sub_sub_BER[,x],list(month(sub_sub_BER[,1])),FUN=mean,na.rm=T)[,2],
            col="red")
    }
    for(x in 2:dim(sub_sub_CTM)[2]){
      lines(x=c(1:12),
            y = aggregate(sub_sub_CTM[,x],list(month(sub_sub_CTM[,1])),FUN=mean,na.rm=T)[,2],
            col="blue")
    }
  cat(k)
  }
  sub_vars <- grep("sub", ls(), value = TRUE)
  rm(list = sub_vars)
}
dev.off()

#### boxplots ------------------------------------------------------------------


irr_value <- vector()
irr_period <- vector()
irr_rcp <- vector()

for(i in 1:3){
  irr_sub <- ET0_CTM %>% filter(year(date) >= periods[i,1] & year(date) <= periods[i,2]) # only for 1 period
  irr_per <- paste(periods[i,1], periods[i,2], sep = "-")
  for(k in 1:3){
    irr_sub_sub <- irr_sub[,grep(rcp_list[k],names(irr_sub))] # only one RCP scenario 
    irr_sub_sub_sums <- apply(irr_sub_sub, MARGIN = 2, FUN = function(x) sum(x,na.rm=T)/30) # get yearly sums
    
    irr_rcp_ <- paste("RCP ",substring(rcp_list[k],4,4),".",substring(rcp_list[k],5,5),sep="")
    
    irr_value <- c(irr_value,irr_sub_sub_sums)
    irr_period <- c(irr_period, rep(irr_per,length(irr_sub_sub)))
    irr_rcp <- c(irr_rcp, rep(irr_rcp_,length(irr_sub_sub)))
    
    # irr_25[length(irr_25)+1] <- quantile(irr_sub_sub_sums,0.25) # 0.25 quantile
    # irr_75[length(irr_75)+1] <- quantile(irr_sub_sub_sums,0.75) # 0.75 quantile 
    # irr_median[length(irr_median)+1] <- median(irr_sub_sub_sums) # median 
  }
}



df_boxplot_ETo <- tibble(
  value = irr_value,
  period = irr_period, 
  rcp = irr_rcp
)


ggplot(data = df_boxplot_ETo, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot()+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Gürbe Catchment: Potential Evapotranspiration (ET0)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white")
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[mm]")

ggsave("01_graphs/boxplots/boxplots_ETo_w_outliers.png")


# List all objects in the workspace that start with "irr_"
objects_with_irr_prefix <- ls(pattern = "^irr_")
rm(list = objects_with_irr_prefix, objects_with_irr_prefix)

### ----------------------------------------------------------------------------
# boxplots ETc 


irr_value <- vector()
irr_period <- vector()
irr_rcp <- vector()

for(i in 1:3){
  irr_sub <- ETc_CTM %>% filter(year(date) >= periods[i,1] & year(date) <= periods[i,2]) # only for 1 period
  irr_per <- paste(periods[i,1], periods[i,2], sep = "-")
  for(k in 1:3){
    irr_sub_sub <- irr_sub[,grep(rcp_list[k],names(irr_sub))] # only one RCP scenario 
    irr_sub_sub_sums <- apply(irr_sub_sub, MARGIN = 2, FUN = function(x) sum(x,na.rm=T)/30) # get yearly sums
    
    irr_rcp_ <- paste("RCP ",substring(rcp_list[k],4,4),".",substring(rcp_list[k],5,5),sep="")
    
    irr_value <- c(irr_value,irr_sub_sub_sums)
    irr_period <- c(irr_period, rep(irr_per,length(irr_sub_sub)))
    irr_rcp <- c(irr_rcp, rep(irr_rcp_,length(irr_sub_sub)))
    
    # irr_25[length(irr_25)+1] <- quantile(irr_sub_sub_sums,0.25) # 0.25 quantile
    # irr_75[length(irr_75)+1] <- quantile(irr_sub_sub_sums,0.75) # 0.75 quantile 
    # irr_median[length(irr_median)+1] <- median(irr_sub_sub_sums) # median 
  }
}



df_boxplot_ETc <- tibble(
  value = irr_value,
  period = irr_period, 
  rcp = irr_rcp
)


ggplot(data = df_boxplot_ETo, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot()+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("Crop Corrected Potential Evapotranspiration (ETc)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white")
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[mm]")

ggsave("01_graphs/boxplots/boxplots_ETc_w_outliers.png")


# List all objects in the workspace that start with "irr_"
objects_with_irr_prefix <- ls(pattern = "^irr_")
rm(list = objects_with_irr_prefix, objects_with_irr_prefix)
# 
# apply(ET0_CTM[,2:ncol(ET0_CTM)],MARGIN=2, FUN = function(x) mean(x,na.rm=T)*366)
# apply(precip_rollmeans[,2:ncol(ET0_CTM)],MARGIN=2, FUN = function(x) mean(x,na.rm=T)*366)




df_boxplot_ETc$type <- rep("ETc", nrow(df_boxplot_ETc))
df_boxplot_ETo$type <- rep("ETo", nrow(df_boxplot_ETo))

df_boxplot <-  rbind(df_boxplot_ETo,df_boxplot_ETc)

df_boxplot$type <- factor(df_boxplot$type, levels = c("ETo", "ETc"))

# Create the combined plot with the adjusted order
ggplot(data = df_boxplot, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0, outlier.shape = NA) +  # Remove outliers
  facet_grid(rcp ~ type, scales = "fixed") +  # Adjust the order of facets
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  ggtitle("Projected ETo & ETc") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey", linewidth = 0.1),
    plot.background = element_rect(color = "transparent", fill = "white"),
    axis.text.x = element_text(angle = 90, size = 7, hjust = 1),
    panel.border = element_rect(color = "black", fill = "transparent")
  ) +
  labs(fill = "RCP-Scenario") +
  xlab("") +
  ylab("[mm]")

# Save the plot
ggsave("01_graphs/boxplots/boxplots_ETc_ETo_combined.png", width = 10.58, height = 7.92)
