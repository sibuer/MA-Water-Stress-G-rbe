library(ggplot2)

# hurs
pl_df <- aggregate_year_month_means(hurs_BER)

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~season,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("mean relative humidity (hurs)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black",fill = "transparent"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="black",fill="white", linewidth = 0.01), 
    axis.text.x = element_text(angle = 90, size = 7),
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[%]")  

ggsave("01_graphs/hurs_monthly.png", width = 10,height = 7)



# rsds
pl_df <- aggregate_year_month_means(rsds_BER)

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~season,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("surface downward solar radiation (rsds)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black",fill = "transparent"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="black",fill="white", linewidth = 0.01), 
    axis.text.x = element_text(angle = 90, size = 7),
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[W/m2]")  

ggsave("01_graphs/rsds_seasons.png", width = 10,height = 7)


# rsds
pl_df <- aggregate_year_month_means(wind_BER)

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~season,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("wind speed") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black",fill = "transparent"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="black",fill="white", linewidth = 0.01), 
    axis.text.x = element_text(angle = 90, size = 7),
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[m/s]")  

ggsave("01_graphs/wind_seasons.png", width = 10,height = 7)


# rsds
pl_df <- aggregate_year_month_means(tas_BER)

ggplot(data = pl_df, aes(x=period, y=value, fill=rcp)) + 
  geom_boxplot(coef=0,outliers = F)+
  facet_grid(rcp~season,scales = "fixed")+
  scale_fill_brewer(palette =  "RdYlBu", direction = -1)+
  ggtitle("temperature above surface (tas)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black",fill = "transparent"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="black",fill="white", linewidth = 0.01), 
    axis.text.x = element_text(angle = 90, size = 7),
  ) +
  labs(fill="RCP-Scenario")+
  xlab("") +
  ylab("[C°]")  

ggsave("01_graphs/tas_seasons.png", width = 10,height = 7)

