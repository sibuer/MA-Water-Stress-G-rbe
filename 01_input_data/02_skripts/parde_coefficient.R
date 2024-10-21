### 
library(ggplot2)

df <- belp_daily %>% filter(year(time)>1993) %>% filter(year(time)<2024)
yyyy_mm <- aggregate(df$runoff, list(format(df$time, "%Y-%m")),function(x)mean(x))
mm <- aggregate(df$runoff, list(format(df$time, "%m")),function(x)mean(x))
av <- mean(yyyy_mm$x)


#yyyy_mm$pk <- yyyy_mm$x/av
yyyy_mm$pk <- rep(NA,nrow(yyyy_mm))

for(i in 1994:2023){
  dat <-  yyyy_mm[as.numeric(substring(yyyy_mm$Group.1,1,4))==i,]
  av_j <- mean(dat$x)
  yyyy_mm$pk[as.numeric(substring(yyyy_mm$Group.1,1,4))==i] <- dat$x/av_j
  rm(dat, av_j)
}


mm$pk <- mm$x/av

# quantiles parde-coefficient 
mm$sevenfive <- rep(NA,nrow(mm))
mm$twofive <- rep(NA,nrow(mm))

for(i in 1:12){
  dat <- yyyy_mm$pk[as.numeric(substring(yyyy_mm$Group.1,6,7))==i]
  mm$sevenfive[i] <- quantile(dat,0.75)
  mm$twofive[i] <- quantile(dat,0.25)
  rm(dat)
}

# erklärte Varianz durch Pardekoeffizienten 
1-(sum((yyyy_mm$x -mm$x)^2))/(sum((yyyy_mm$x - av)^2))

# normierter Jahresabfluss
yyyy <- aggregate(df$runoff, list(format(df$time, "%Y")),function(x)mean(x))
yyyy$x/av



#### P-Werte
yyyy$Rj <- rep(NA,nrow(yyyy))

for(i in 1994:2023){
  dat <-  yyyy_mm[as.numeric(substring(yyyy_mm$Group.1,1,4))==i,]
  yyyy$Rj[i-1993] <- sum(abs(av*mm$pk-dat$x))/sum(av*mm$pk)
  rm(dat)
}

## R saisonal (nur saisnale Einflüsse durch verwenden Jahresmittelabfluss)

yyyy$R_saisonal <- rep(NA,nrow(yyyy))

for(i in 1994:2023){
  dat <-  yyyy_mm[as.numeric(substring(yyyy_mm$Group.1,1,4))==i,]
  yyyy$R_saisonal[i-1993] <- sum(abs(mean(dat$x)*mm$pk-dat$x))/sum(mean(dat$x)*mm$pk)
  rm(dat)
}


## R MQ (nur Schwankung der jährlichen Abflüsse)

yyyy$R_mq <- rep(NA,nrow(yyyy))

for(i in 1994:2023){
  dat <-  yyyy_mm[as.numeric(substring(yyyy_mm$Group.1,1,4))==i,]
  yyyy$R_mq[i-1993] <- sum(abs(av*dat$pk-dat$x))/sum(av*dat$pk)
  rm(dat)
}

png(filename = "01_graphs/R_value_plot.png", units = "in", width = 7, height = 5, res = 300)

# first plot
par(mar=c(5, 4, 4, 5))
hcum <- h <- hist(yyyy$Rj, plot=FALSE)
plot(hcum, main="R-Values Gürbe (1994-2023)",xlim = c(0,0.8), axes = F, ylab="", xlab = "R-value", col = "red")
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Frequency",side=2,line=2.5)
box()
# "second" plot
par(new = TRUE)
yline <- cumsum(hcum$counts)/30*100
plot(x=hcum$mids, y=yline, xlim = c(0,0.8), ylim=c(0,100), axes = FALSE, bty = "n", xlab = "", ylab = "", col="blue", pch=19)
lines(x=hcum$mids, y=yline, xlim = c(0,1), col="blue", lwd = 2)
mtext("Cumulative frequency",side=4,line=2.5)
axis(side=4, ylim=c(0,100), las=1)
axis(1,seq(0,0.8,0.1))
dev.off()


####

mm$Group.1 <- factor(mm$Group.1, levels = sprintf("%02d", 1:12), ordered = TRUE)
mm_long <- pivot_longer(mm, cols = c(pk, sevenfive, twofive), names_to = "type", values_to = "value")



library(ggplot2)

# Assuming 'mm_long' is the long format data created earlier

library(ggplot2)

# Assuming 'mm_long' is the long format data and 'mm' is the original data frame with quantiles

ggplot() +
  
  # Plot the shaded area between 'twofive' and 'sevenfive'
  geom_ribbon(data = mm, aes(x = Group.1, ymin = twofive, ymax = sevenfive,group=1), fill = "khaki", alpha = 0.5) +
  # Plot the line for 'pk' with points
  geom_line(data = subset(mm_long, type == "pk"), aes(x = Group.1, y = value,group=1), color = "blue", size = 1) +
  geom_point(data = subset(mm_long, type == "pk"), aes(x = Group.1, y = value), color = "blue", size = 3) +
  
  # Labels and title
  labs(title = "Gürbe Belp (1994-2023)", x = "", y = "Pardé-coefficient") +
  
  # Customize x-axis to show months
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   expand = c(0.01, 0.01)) +
  
  # Set theme for a clean look
  theme_minimal(base_size = 15) +
  
  # Additional theme adjustments
  theme(
    legend.title = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    panel.background = element_rect(colour = "black"),
    panel.grid.major.y  = element_line(size = 0.5, linetype = 'solid', colour = "gray80"),
    axis.ticks = element_line(),
    plot.background = element_rect("white")

  )

ggsave("01_graphs/parde_coeff_regime.png")
