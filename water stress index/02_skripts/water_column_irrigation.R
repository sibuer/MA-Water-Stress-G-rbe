
## Irrigation Columns Annual 

irr_area <- mean(crop_coefficents$irrigated_area)
irr_water_column <- vector()
irr_daily_need <- vector()

for(i in 1:9){
  irr_sum <- WSI[[i]]$irr_median %>% sum()
  irr_daily_need <- irr_sum/366
  irr_water_column[i] <- irr_sum * 116000000 / irr_area
} 

irr_water_column
irr_daily_need

## --> == ca. übereinstimmend mit Brunner 
df_water_column <- tibble(
  period = substring(names(WSI),0,9),
  rcp = substring(names(WSI),11,15),
  value = as.numeric(irr_water_column)
)
write.csv(df_water_column, file="03_tables/irrigation_water_column.csv")

# List all objects in the workspace that start with "irr_"
objects_with_irr_prefix <- ls(pattern = "^irr_")
rm(list = objects_with_irr_prefix, objects_with_irr_prefix)

# put mean for observed periods 
vec_mean <- df_water_column[df_water_column[,1]=="1990_2019",3] %>% as.vector
df_water_column[df_water_column[,1]=="1990_2019",3] <- vec_mean$value %>% mean



ggplot(data = df_water_column)+
  geom_bar(aes(x=rcp,y=value,fill=period), position = "dodge", stat = "identity")+
  scale_fill_brewer(palette =  "Set3")+
  ggtitle("Irrigation Need: Water Column [mm]") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white")
  ) +
  labs(fill="Period")+
  xlab("") +
  ylab("[mm]")+
  annotate("segment", x = 0.5, xend = 3.6, y = 0, yend = 0, color = "black", size = 1, linetype = "solid")

ggsave("01_graphs/barplot_water_colum.png", units = "in", width = 3.97, height = 3.97)










