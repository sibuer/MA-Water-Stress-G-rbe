
# for(i in 1:3){
#   file <- list.files("00_data/hydro_ch18_runoff/daily")[grepl("2159",list.files("00_data/hydro_ch18_runoff/daily"))][i]
#   file <- paste("00_data/hydro_ch18_runoff/daily/",file,sep="")
#   name <- paste("daily_",substring(file,(nchar(file)-8),nchar(file)-4), sep = "")
#   
#   assign(x = name, value = read.csv(file = file, header = T))
# }

lm.monthly<-list()
for(i in 1:3){
  file <- list.files("00_data/hydro_ch18_runoff/monthly")[grepl("2159",list.files("00_data/hydro_ch18_runoff/monthly"))][i]
  file <- paste("00_data/hydro_ch18_runoff/monthly/",file,sep="")
  name <- paste("monthly_",substring(file,(nchar(file)-8),nchar(file)-4), sep = "")
  lm.monthly[[name]] <-  read.csv(file = file, header = T)
  rm(file)
}

# check data
lm.monthly.sd <- list()
for (i in 1:length(lm.monthly)){
  name <- paste("sd_",names(lm.monthly)[i],sep="")
  lm.monthly.sd[[name]] <- data.frame(lm.monthly[[i]][,1:2],sd = apply(lm.monthly[[i]][,(dim(lm.monthly[[i]])[2]-(dim(lm.monthly[[i]])[2]-2)):dim(lm.monthly[[i]])[2]], 
                                            MARGIN = 1, FUN = sd))
}

# create list for each 20y interval and RCP-scenario 
lm.periods.monthly <- list()
for (i in 1:length(lm.monthly)){
  df <- lm.monthly[[i]]
  for (k in 1:dim(periods)[1]){
    sub <- df[df$Year>=periods[k,1]&df$Year<=periods[k,2],]
    name <- paste(substring(names(lm.monthly[i]),9,14), periods[k,1],periods[k,2], sep = "_")
    lm.periods.monthly[[name]] <- aggregate(x = sub[,3:dim(sub)[2]], by = list(sub$Month), FUN = mean)
    lm.periods.monthly[[name]] <- as.data.frame(lm.periods.monthly[[name]])
  }
  rm(df,sub)
}

# conversion m3/s -> mm  
size_catchment_m2 <- 116.1*1e+6
factor_m3_s_mm_monthly <- 1000*30.4375*24*60*60 #m3 -> liter x days per month x hours x minutes x seconds  
lm.periods.monthly.mm <- list()
for (i in 1:length(lm.periods.monthly)){
  name <- names(lm.periods.monthly[i])
  lm.periods.monthly.mm[[name]] <- data.frame(month = 1:12,(lm.periods.monthly[[i]][,2:ncol(lm.periods.monthly[[i]])]*factor_m3_s_mm_monthly)/size_catchment_m2)
}

# plot 
par(mfrow=c(4,4))
colors <- c(rep("darkgreen",4), rep("orange",4),rep("red",4))
for(i in 1:length(lm.periods.monthly.mm)){
  
  df <- lm.periods.monthly.mm[[i]]
  plot(df[,1], df[,2], type = "l", main = names(lm.periods.monthly.mm[i]), ylab = "[mm]", xlab="month", xaxt="n", ylim = c(15,100), col = "grey")
  axis(1, at = 1:12, labels = 1:12)
  for(k in 3:ncol(df)){
    lines(df[,1],df[,k], col = "grey")
  }
  lines(df[,1],apply(df[2:ncol(df)],MARGIN = 1, FUN=mean), col = colors[i], lwd =2)
  rm(df)
}
colors <- c("darkgreen","orange","red")
for(i in 1:4){
  list_temp <- lm.periods.monthly.mm[grepl(periods[i,1],names(lm.periods.monthly.mm))]
  plot(list_temp[[1]][,1] ,apply(list_temp[[1]][2:ncol(list_temp[[1]])],MARGIN = 1, FUN=mean) , 
         type = "l", main = substring(names(lm.periods.monthly.mm[i]),7,15), ylab = "[mm]", xlab="month", xaxt="n", ylim = c(15,100), col = colors[1], lwd = 2)
  axis(1, at = 1:12, labels = 1:12)
  for(k in 2:length(list_temp)){
    lines(list_temp[[1]][,1] ,apply(list_temp[[k]][2:ncol(list_temp[[k]])],MARGIN = 1, FUN=mean),lwd=2,col=colors[k])
  }
}
rm(i,k,colors,list_temp)




