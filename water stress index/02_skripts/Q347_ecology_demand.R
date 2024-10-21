### Q347 

# create emtpy matrix 
q347 <- matrix(data = NA, nrow = 3, ncol = (ncol(streamflow_belp_mod_mm)-1))
colnames(q347) <- colnames(streamflow_belp_mod_mm[2:ncol(streamflow_belp_mod_mm)])
 
# calc. Q347 and fill matrix 
for(i in 1:3){
  sub_data <- streamflow_belp_mod_m3[year(dates)>=periods[i,1]&year(dates)<=periods[i,2],2:ncol(streamflow_belp_mod_m3)]
  q347[i,] <- lapply(sub_data,function(x) quantile(x,0.05,na.rm=T)) %>% unlist() %>% as.vector()
}

# mean per scenario 
q347_scenario_means <- matrix(data = NA, nrow = 3, ncol = 3)
colnames(q347_scenario_means) <- rcp_list
rownames(q347_scenario_means) <- paste(periods[,1],"-",periods[,2],sep="")

for(i in 1:3){
  q347_scenario_means[,i] <- 
    apply(q347[,grep(rcp_list[i],x = colnames(q347))],MARGIN = 1,function(x) mean(x,na.rm = T))
}

### put in actual values
belp_daily <- read.csv("00_data/runoff_belp_daily_observed.csv") %>% filter(year(time)>=1990&year(time)<=2019) %>% select(c(time,runoff))
q347_scenario_means[1,] <- quantile(belp_daily$runoff,0.05)

# adjustment according to GschG
q347_scenario_means[1,] <- (280+31+31)/1000

q347_scenario_means[2,1] <- (160+(25*4.4))/1000
q347_scenario_means[2,2] <- (160+(22*4.4))/1000
q347_scenario_means[2,3] <- (160+(24*4.4))/1000

q347_scenario_means[3,1] <- (160+(22*4.4))/1000
q347_scenario_means[3,2] <- (160+(18*4.4))/1000
q347_scenario_means[3,3] <- (160+(18*4.4))/1000

## convert to m3/month then to monthly mm water col
q347_monthly_mm <- (q347_scenario_means*60*60*24*30.5)/(116000000)*1000


# save as table 
q347_monthly_mm <- round(q347_monthly_mm,3)
write.csv(q347_monthly_mm,"03_tables/q347_monthly_need_mm.csv")

rm(q347_scenario_means)
