library(dplyr)
library(zoo)
library(tidyr)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(lubridate)


### Prepare dataframe with existing data -------------------------------------

## daten einlesen 
df <- read.table("00_data/bfs_animal_heads.csv", sep = ";", dec = ".", header = T)
# gemeinden auswählen und ausfiltern
gmd <- c(0861, 0922, 0863, 0866, 0867, 0869, 0872,0877, 0936, 0879,0880, 0883,0770,         0889,
         0884, 0943, 0944, 0888, 0886,0948)
df$gmd_bfs <- gsub("\\D", "", df$Gemeinde) %>% as.numeric() 
df$gmd_name <- gsub("[^a-zA-Z]", "",df$Gemeinde)
df$Gemeinde <- NULL
df <- df[df$gmd_bfs%in%gmd,]

# only keep 10-year steps 
# df[,colnames(df)[grepl(("2022|2015|2005"),colnames(df))]] <- NULL

# add factors 
bfs_percentage_area <- read.table("00_data/bfs_percentage_area.csv", sep = ";", dec=".", header = T)
df <- merge(x = df, y=bfs_percentage_area[,c("GMDE","kat_2")], by.x = "gmd_bfs", by.y = "GMDE", all.x = T,  suffixes = "")

# Tables livestock 
list_livestock <- unique(df$Typ)[grepl("Tiere",unique(df$Typ))]

# translation table 
trans <- data.frame(
  de = substring(list_livestock,9),
  eng = c("Other Animals", "Poultry","Pigs", "Goats","Sheep","Equine","Cattle")
)



# create df to fill up 

animal_heads <- data.frame(
  year =c(sort(as.numeric(gsub("\\D"," ",colnames(df)[grepl("X",colnames(df))])),decreasing = F),
          seq(2025,2100,5)),
  type = c(rep("observed",10), rep("extrapolated",16))
)

for(i in 1:length(list_livestock)){
  df_sub <- df[df$Typ==list_livestock[i],]
  
  # apply area percentage factors
  area_percent <- df_sub$kat_2
  
  # only keep columns with observations of livestock numbers 
  df_sub <- df_sub[,colnames(df_sub)[grepl("X",colnames(df_sub))]]
  
  # summary table 
  df_sub <- apply(df_sub,MARGIN = 2,function(x)x*area_percent)
  
  # only keep digits in names 
  colnames(df_sub) <- gsub("\\D","",colnames(df_sub))
  
  
  ## get sums for each livestock type 
  df_sub <- apply(df_sub,MARGIN = 2, function(x)sum(x))
  
  # create df to order 
  df_sub <- data.frame(year=names(df_sub),number=as.numeric(df_sub))
  df_sub <- df_sub[order(df_sub$year),] # order 
  rownames(df_sub)<- 1:10               # adjust rownames 
  
  # define title (from translation table)
  title <- trans[trans$de==substring(list_livestock[i],9),2]
  
  # add to final dataframe 
  animal_heads[title] <- c(df_sub$num,rep(NA,16))
}



#### ---------------------------------------------------------------------------
# Extrapolate to fill up data 


## For Equine, and Pigs: Assume 2020 values to stay persistent 
animal_heads$Equine[is.na(animal_heads$Equine)] <- animal_heads$Equine[animal_heads$year==2020]
animal_heads$Pigs[is.na(animal_heads$Pigs)] <-  animal_heads$Pigs[animal_heads$year==2020]

## For Poultry and "other Animals": Assume mean to stay persistent
animal_heads$Poultry[is.na(animal_heads$Poultry)] <- mean(animal_heads$Poultry,na.rm = T)
animal_heads$`Other Animals`[is.na(animal_heads$`Other Animals`)] <- mean(animal_heads$`Other Animals`,na.rm = T)

# For Sheep, cAttle and Goats: Assume linear change according to observed period 

for(i in c(grep("Sheep",colnames(animal_heads)),grep("Cattle",colnames(animal_heads)),grep("Goats",colnames(animal_heads)))){
  # define coefficients for linear model 
  b <- lm(animal_heads[!is.na(animal_heads[,i]),i]~animal_heads$year[!is.na(animal_heads[,i])])$coefficients[1]
  m <- lm(animal_heads[!is.na(animal_heads[,i]),i]~animal_heads$year[!is.na(animal_heads[,i])])$coefficients[2]
  
  # apply linear model 
  animal_heads[animal_heads$year>=2025&animal_heads$year<=2050,i] <- 
    animal_heads$year[animal_heads$year>=2025&animal_heads$year<=2050]*m+b
}

animal_heads$Sheep[is.na(animal_heads$Sheep)] <-  animal_heads$Sheep[animal_heads$year==2050]
animal_heads$Cattle[is.na(animal_heads$Cattle)] <-  animal_heads$Cattle[animal_heads$year==2050]
animal_heads$Goats[is.na(animal_heads$Goats)] <-  animal_heads$Goats[animal_heads$year==2050]

### plot livestock numbers -----------------------------------------------------

plot_list <- list()

for(i in 3:ncol(animal_heads)){
  
  df_plot <- data.frame(year=animal_heads$year,data=animal_heads[,i], type=animal_heads$type)
  df_plot <- df_plot[df_plot$year<=2080,]
  title <- colnames(animal_heads)[i]
  
  plot_list[[i-2]] <- ggplot(df_plot,aes(x=year,y=data, color = type))+
    geom_point(show.legend = FALSE)+
    geom_smooth(method=lm, se=F,show.legend = FALSE)+
    # title etc
    theme_bw()+
    scale_x_continuous(breaks = round(seq(1980,2100, 20)))+
    labs(title=paste("Nr. of animal heads: ", title),
         x ="", y = "")
}
rm(df_plot,title)

## plot full data 
ggsave("01_graphs/livestock_animal_heads.png",
       plot = grid.arrange(grobs = plot_list, ncol = 3), height = 4, width = 10)
#width = 4500, height = 6000, units = "px", dpi = 600)

## ----------------------------------------------------------------------------
# table water demand livestock 

gve_conversion <- data.frame(animal = colnames(animal_heads[3:9]))
gve_conversion$factor <-  c(0.2,0.01,0.3,0.2,0.2,0.5,0.8)

# gve_conversion$werte_alcamo <- c(0.028,15,25,2.25,4,2.25,2.25) false, needs reordering 
gve_conversion$werte_brunner_gve <- gve_conversion$factor*110

animal_heads_gve <- data.frame(year=animal_heads$year,gve=rep(0,nrow(animal_heads)),type = animal_heads$type)

# according to number of GVE's 
for(i in 3:ncol(animal_heads)){
  animal <- colnames(animal_heads)[i]
  factor <- gve_conversion$factor[gve_conversion$animal==animal]
  print(paste(animal,factor))
  
  animal_heads_gve$gve <- animal_heads_gve$gve + animal_heads[,i]*factor
}


ggplot(animal_heads_gve,aes(x=year,y=gve, color = type))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method=lm, se=F,show.legend = FALSE)+
  # title etc
  theme_bw()+
  scale_x_continuous(breaks = round(seq(1980,2100, 20)))+
  labs(title=paste("Nr. of GVE-Animal Heads"),
       x ="", y = "")

#animal_heads[title] <-  c(df_sub$num, animal_heads$year[animal_heads$year>=2025]*m+b)

# rm variables 
rm(df_sub, df_sub_exp, b, m, i, title)


# ----------------------------------------------------------------------------
# water need p. animal 



# get water need 
animal_heads_gve$water_need_daily_l <- animal_heads_gve$gve*110
animal_heads_gve$water_need_monthly_m3 <- animal_heads_gve$water_need_daily_l*30.5/1000
animal_heads_gve$water_need_monthly_mm <- animal_heads_gve$water_need_monthly_m3*1000/116000000

write.csv(animal_heads_gve, "03_tables/animal_heads_gve.csv", row.names = FALSE)
write.csv(animal_heads, "03_tables/animal_heads.csv", row.names = FALSE)

# keep rows relevant for WSI table 
animal_heads_water <- animal_heads_gve %>% select(c(year,water_need_monthly_mm))
colnames(animal_heads_water) <- c("year","livestock_monthly")



## merge with water table 

water$livestock_monthly <- NULL # delete existing livestock column 

water <- merge(water, animal_heads_water, by="year",all.x = T, all.y = T) 

water$livestock_monthly <- zoo::na.approx(water$livestock_monthly,maxgap = 20)
water <- water %>% filter(year>=1990&year<=2090)

# remove variables 
rm(animal_heads,animal_heads_gve,animal_heads_gve_filtered,animal,factor,animal_heads_water)


