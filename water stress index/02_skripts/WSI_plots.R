library(treemapify)
library(scales)
library(scico)
library(ggplot2)
library(tidyr)
library(gridExtra)

#### Plot ---------------------------------------------------------------------- 


# colors_regime_plot <- c("#fcf6f0", "#ffffb7", "#fecc5c", "#fd8d3c", "#e31a1c")



results_regime_plot <- function(p1=1,p2=4){
  col_vec <- c("#fcf6f0", "#ffffb7", "#FEB24C", "#FC4E2A", "#b10026")
  # define title
  name <- names(WSI)[p2]
  title <- paste(substring(name,1,4),
                 "-",
                 substring(name,6,9),
                 ": ",
                 substring(name,11,13),
                 " ",
                 substring(name,14,14),
                 ".",
                 substring(name,15,15),
                 sep = "")
  
  
  ## create necessary data frames for plot 
  
  # basic data (sf and demand)
  df_plot <- WSI[[p1]] %>% 
    select(month,sfm_median,need_wo_eco) %>% 
    rename(sf_current = sfm_median, wsi_current = need_wo_eco)
  
  df_plot <- cbind(df_plot, WSI[[p2]] %>% 
                     select(sfm_median,need_wo_eco)%>% 
                     rename(sf_future = sfm_median, wsi_future =need_wo_eco))
  
  
  ## pivot longer 
  df_plot <- pivot_longer(df_plot, cols = c(sf_future, wsi_future,sf_current, wsi_current), 
                          
                          values_to = "value", names_to = "names")
  
  df_ws_current <- WSI[[p1]] %>% select(month,WSI_wo_eco)
  df_ws_current$wsi <-  sapply(df_ws_current$WSI_wo_eco, FUN = function(x) wsi_to_text(x))
  df_ws_current$month <- as.factor(df_ws_current$month)
  
  # range of demand
  demand_range_current <- WSI[[p1]] %>% select(month,need_wo_eco_max,need_wo_eco_min)
  
  # range of ecology
  ecology_current <- WSI[[p1]] %>% select(month,ecology,need_wo_eco,need_eco)
  
  
  
  
  
  ## FUTURE
  # range of runoff
  sf_range_future <- WSI[[p2]] %>% select(month,sfm_q25,sfm_q75)
  
  # water stress future # create text WSI-levels with month factor-levels 
  df_ws_future <- WSI[[p2]] %>% select(month,WSI_wo_eco)
  df_ws_future$wsi <-  sapply(df_ws_future$WSI_wo_eco, FUN = function(x) wsi_to_text(x))
  df_ws_future$month <- as.factor(df_ws_future$month)
  
  
  # range of demand
  demand_range_future <- WSI[[p2]] %>% select(month,need_wo_eco_max,need_wo_eco_min)
  
  # range of ecology
  ecology_future <- WSI[[p2]] %>% select(month,ecology,need_wo_eco,need_eco,need_eco_max, need_wo_eco_max)
  
  
  # plot -------------------------------------------------------------------------
  p <- ggplot(data = df_plot) +
    
    # top rectangle
    annotate("rect", xmin = 0, xmax = 13, ymin = 90, ymax = 125, fill = "white") +
    
    # manual y-axis 
    annotate("text", x = 0.2, y = 10, label = "10")+
    annotate("text", x = 0.2, y = 20, label = "20")+
    annotate("text", x = 0.2, y = 30, label = "30")+
    annotate("text", x = 0.2, y = 40, label = "40")+
    annotate("text", x = 0.2, y = 50, label = "50")+
    annotate("text", x = 0.2, y = 60, label = "60")+
    annotate("text", x = 0.2, y = 70, label = "70")+
    annotate("text", x = 0.2, y = 80, label = "80")+
    annotate("text", x = 0.2, y = 100, label = "F", fontface = "bold")+
    annotate("text", x = 0.2, y = 113, label = "P", fontface = "bold")+
    
    annotate("text", x = -0.5, y = 50, label = "[mm]", angle = 90)+
    annotate("text", x = 0.5, y = 120, label = title, hjust=0,vjust=0, fontface = "bold.italic")+
    
    # manual y-grid
    annotate("segment", x = 0.5, xend = 12.5, y = 10, yend = 10, linewidth=.2, colour = "darkgrey")+
    annotate("segment", x = 0.5, xend = 12.5, y = 20, yend = 20, linewidth=.2, colour = "darkgrey")+
    annotate("segment", x = 0.5, xend = 12.5, y = 30, yend = 30, linewidth=.2, colour = "darkgrey")+
    annotate("segment", x = 0.5, xend = 12.5, y = 40, yend = 40, linewidth=.2, colour = "darkgrey")+
    annotate("segment", x = 0.5, xend = 12.5, y = 50, yend = 50, linewidth=.2, colour = "darkgrey")+
    annotate("segment", x = 0.5, xend = 12.5, y = 60, yend = 60, linewidth=.2, colour = "darkgrey")+
    annotate("segment", x = 0.5, xend = 12.5, y = 70, yend = 70, linewidth=.2, colour = "darkgrey")+
    annotate("segment", x = 0.5, xend = 12.5, y = 80, yend = 80, linewidth=.2, colour = "darkgrey")+
    
    
    
    ## ---------------------------------------------------------------------------
  
  
  
  #  current 
  geom_ribbon(data = sf_range_current, 
              aes(x = as.numeric(month), ymin = sfm_q25, ymax = sfm_q75), 
              fill = "white", alpha = 0, colour = "grey", linewidth = 0.2, show.legend = FALSE) +
    
    
    # range future
    geom_ribbon(data = sf_range_future, 
                aes(x = as.numeric(month), ymin = sfm_q25, ymax = sfm_q75), 
                fill = "#0496C7", alpha = 0.2, show.legend = FALSE) +
    
    # range future
    geom_ribbon(data = demand_range_future, 
                aes(x = as.numeric(month), ymin = need_wo_eco_min, ymax = need_wo_eco_max), 
                fill = "white", alpha = 0.2, show.legend = FALSE) +
    
    # range future
    geom_ribbon(data = ecology_future, 
                aes(x = as.numeric(month), ymin = need_wo_eco, ymax = need_eco), 
                fill = "#7CCBA2", alpha = 0.5, show.legend = FALSE) +
    
    # range future uncertainty ecology
    geom_ribbon(data = ecology_future, 
                aes(x = as.numeric(month), ymin = need_wo_eco_max, ymax = need_eco_max), 
                fill = "#7CCBA2", alpha = 0.35, show.legend = FALSE) +
    
    
    # current 
    geom_ribbon(data = demand_range_future, 
                aes(x = as.numeric(month), ymin = need_wo_eco_min, ymax = need_wo_eco_max), 
                fill = "darkgrey", alpha = 0.6,  show.legend = FALSE) +
    
    
    
    # abdecken oben 
    annotate("rect", xmin = 0.5, xmax = 12.5, ymin = 90, ymax = 100, fill = "white",colour="transparent") +
    # plot border 
    annotate("rect", xmin = 0.5, xmax = 12.5, ymin = 0, ymax = 90, fill = "transparent",colour="black") +
    
    
    
    
    # Adding the line
    geom_line(aes(x = as.numeric(month), y = value, 
                  color = names, linetype=names, linewidth = names), show.legend = F) +
    
    # Adding the water stress bar (current)
    geom_tile(data = df_ws_current, 
              aes(x = as.numeric(month), y = 113, fill = wsi), 
              height = 8, width = 1, show.legend = FALSE, colour = "black", linewidth=0.5) +
    # Adding the water stress bar (future)
    geom_tile(data = df_ws_future,
              aes(x = as.numeric(month), y = 100, fill = wsi), 
              height = 8, width = 1, show.legend = FALSE, colour = "black", linewidth=0.5) + 
    
    
    
    # adjust Scales
    scale_x_continuous(breaks = 1:12, 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                       limits = c(-0.5,13),
                       minor_breaks = seq(0.5,12.5,1)) +
    scale_y_continuous(breaks = seq(10, 80, by = 10))+
    scale_fill_manual(values = c("No Stress" = col_vec[1], "Low Stress" = col_vec[2], "Moderate Stress" = col_vec[3], "High Stress" = col_vec[4], "Severe Stress" = col_vec[5])) +
    scale_color_manual(values = c("#023E8A","#023E8A","black","black"))+
    scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
    scale_linewidth_manual(values = c(0.9,0.9,0.9,0.9))+
    
    
    
    # Theme settings
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(0,0,1,0), "cm"),
      axis.text.y = element_blank(),
      panel.background = element_rect(fill = "white", colour = "white"), 
      plot.background = element_rect(fill = "white", colour="white"),
      axis.text.x = element_text(size = 11, family = "sans", colour = "black")
    )
  return(p)
}

### ----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

legend_plot <- function(k){
  col_vec <- c("#fcf6f0", "#ffffb7", "#FEB24C", "#FC4E2A", "#b10026")
  ap_r <- 2
  v <- 0
  top <- 3
  
  name <- names(WSI)[k]
  period <- paste(substring(name,1,4),
                  "-",
                  substring(name,6,9),
                  sep = "")
  
  df_mock <- data.frame(month=c(1:12),value=sample(1:100, 12, replace = TRUE))
  
  ### plot legend ----------------------------------------------------------------
  p <- ggplot(data=df_mock)+
    geom_line(aes(x=month,y=value),show.legend = F)+
    annotate("rect",xmin = 0, xmax = 13, ymin = 0, ymax = 120, fill = "white")+
    
    # Water Stress Index ----------------
  annotate("rect",xmin = v + 0, xmax = v +1, ymin = 40, ymax = 47, fill = col_vec[1],colour="black")+
    annotate("rect",xmin = v +0, xmax = v +1, ymin = 30, ymax = 37, fill = col_vec[2],colour="black")+
    annotate("rect",xmin = v +0, xmax = v +1, ymin = 20, ymax = 27, fill = col_vec[3],colour="black")+
    annotate("rect",xmin = v +0, xmax = v +1, ymin = 10, ymax = 17, fill = col_vec[4],colour="black")+
    annotate("rect",xmin = v +0, xmax = v +1, ymin = 0, ymax =7, fill = col_vec[5],colour="black")+
    # header 
    annotate("text", x = 0, y = 52, label = "Water Stress Index: Need to Availability Ratio", fontface = "bold", hjust=0, vjust=0)+
    # Labels range
    annotate("text", x = v +1.25, y = 2, label = "> 0.8", hjust=0,vjust=0)+
    annotate("text", x = v +1.25, y = 12, label = "0.4 - 0.8", hjust=0, vjust=0)+
    annotate("text", x = v +1.25, y = 22, label = "0.2 - 0.4", hjust=0, vjust=0)+
    annotate("text", x = v +1.25, y = 32, label = "0.1 - 0.2", hjust=0, vjust=0)+
    annotate("text", x = v +1.25, y = 42, label = "< 0.1", hjust=0, vjust=0)+
    # Labels Indications 
    annotate("text", x = v +3.25, y = 2, label = "Severe Stress", hjust=0, vjust=0)+
    annotate("text", x = v +3.25, y = 12, label = "High Stress", hjust=0, vjust=0)+
    annotate("text", x = v +3.25, y = 22, label = "Moderate Stress", hjust=0, vjust=0)+
    annotate("text", x = v +3.25, y = 32, label = "Low Stress", hjust=0, vjust=0)+
    annotate("text", x = v +3.25, y = 42, label = "No Stress", hjust=0, vjust=0)+
    
    
    annotate("segment", x =0, xend = 13, y = top +58.5-1, yend = top +58.5-1, linewidth=.1, colour = "black")+
    annotate("segment", x =6.5, xend = 6.5, y = 0, yend = 47, linewidth=.1, colour = "black")+
    annotate("segment", x = 7, xend = 13, y = 23.5, yend = 23.5, linewidth=.1, colour = "black")+
    
    # P & F Segment ----------------------
  # Legend 
  annotate("text", x = 7, y = ap_r+30, label = "F", fontface = "bold", hjust=0, vjust=0)+
    annotate("text", x = 7, y = ap_r+40, label = "P", fontface = "bold", hjust=0, vjust=0)+
    # LEgend Text 
    annotate("text", x = 7.5, y = ap_r+30, label = "Future Water Stress State", hjust=0, vjust=0)+
    annotate("text", x = 7.5, y = ap_r+40, label = "Current Water Stress State", hjust=0, vjust=0)+
    
    
    
    ## TEXT Oben 
    annotate("text", x = 0, y =top + 112, label = "Freshwater Resource Availability (Modeled Data)", hjust=0, vjust=0, fontface = "bold")+
    # Legend oben 
    annotate("segment", x = 0, xend = 1, y = top +104, yend = top +104, linewidth=1, colour = "#023E8A")+
    annotate("rect",xmin = 0, xmax = 1, ymin = top +91, ymax =top + 97, fill = "#0496C7", alpha=0.2)+
    annotate("rect",xmin = 0, xmax = 1, ymin = top +107, ymax =top + 101, fill = "#0496C7", alpha=0,colour="grey",linewidth=0.2)+
    annotate("segment", x = 0, xend = 1, y =top + 94, yend =top + 94, linewidth=1, colour = "#023E8A", linetype = "dashed")+
    annotate("segment", x = 0, xend = 1, y =top + 74, yend =top + 74, linewidth=1, colour = "black")+
    annotate("segment", x = 0, xend = 1, y =top + 64, yend =top + 64, linewidth=1, colour = "black", linetype = "dashed")+
    annotate("rect",xmin = 0, xmax = 1, ymin = top +61, ymax =top + 67, fill = "darkgrey", alpha=0.5)+
    
    annotate("rect",xmin = 7, xmax = 8, ymin = 10, ymax = 17, fill = "#7CCBA2", alpha=0.5)+
    annotate("text", x = 8.5,y  = 12, label = "Residual Water", hjust=0, vjust=0)+
    annotate("text", x = 8.5,y  = 2, label = "Requirement (Q347)", hjust=0, vjust=0)+
    
    
    annotate("text", x = 1.5, y = top +102, label = "Mean Monthly Streamflow & Variability Range (1990-2019)", hjust=0, vjust=0)+
    annotate("text", x = 1.5, y = top +92, label = paste("Mean Monthly Streamflow & Variability Range (", period,")", sep = ""), hjust=0, vjust=0)+
    
    annotate("text", x = 0, y = top +82, label = "Mean Total Water Need", hjust=0, vjust=0, fontface = "bold")+
    
    annotate("text", x = 1.5, y = top +72, label = "Current State (1990-2019)", hjust=0, vjust=0)+
    annotate("text", x = 1.5, y = top +62, label = paste("Future Trend & Variability Range (",period,")",sep=""), hjust=0, vjust=0)+
    
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(), 
      axis.title = element_blank(),
      panel.background = element_rect(fill="white")
    )
  return(p)
}



# creata arrangement 
list_plots_regime <- list()

list_plots_regime[[1]] <- results_regime_plot(p1=1,p2=4)
list_plots_regime[[2]] <- results_regime_plot(p1=1,p2=5)
list_plots_regime[[3]] <- results_regime_plot(p1=1,p2=6)
list_plots_regime[[4]] <-  legend_plot(k=4)

arranged_plot <- grid.arrange(grobs = list_plots_regime, ncol = 2)  
ggsave("01_graphs/regime_plot/wsi_regime_2020-2049.png",plot = arranged_plot, units = "in", width = 10.58, height = 7.92)

# creata arrangement 
list_plots_regime <- list()

list_plots_regime[[1]] <- results_regime_plot(p1=1,p2=7)
list_plots_regime[[2]] <- results_regime_plot(p1=1,p2=8)
list_plots_regime[[3]] <- results_regime_plot(p1=1,p2=9)
list_plots_regime[[4]] <-  legend_plot(k=7)

arranged_plot <- grid.arrange(grobs = list_plots_regime, ncol = 2)  
ggsave("01_graphs/regime_plot/wsi_regime_2045-2074.png",plot = arranged_plot, units = "in", width = 10.58, height = 7.92)





# #### --------------------------------------------------------------------------
# ##    TREEMAP 
# 
# 
# df_plot <- WSI[[1]] %>% select(irr_median,drinking,industrial,ecology,livestock_monthly)
# 
# 
# df_plot <-  pivot_longer(df_plot, cols = c(irr_median,drinking,industrial,ecology,livestock_monthly), values_to = "value", names_to = "names")
# 
# df_plot <- df_plot %>%  group_by(names) %>%
#   summarise(sum_value = sum(value))
# 
# df_plot[df_plot[,1]=="ecology",1] <- "Ecology"
# df_plot[df_plot[,1]=="drinking",1] <- "Drinking Water"
# df_plot[df_plot[,1]=="industrial",1] <- "Industry"
# df_plot[df_plot[,1]=="irr_median",1] <- "Irrigation"
# df_plot[df_plot[,1]=="livestock_monthly",1] <- "Livestock"
# 
# 
# 
# 
# total <- scales::comma(sum(df_plot$sum_value)*16000000/1000, big.mark = "'")
#  
# cv <- scico(n=5,palette = "batlow",begin = 0.2,end = 0.6)
# 
# custom_colors <- c("Ecology" = "#7CCBA2", 
#                    "Drinking Water" = "#045275", 
#                    "Industry" = "#00718B", 
#                    "Irrigation" = "#46AEA0", 
#                    "Livestock" = "#089099")
# 
# # Create the treemap
# ggplot(df_plot, aes(area = sum_value, fill = names, label = names)) +
#   geom_treemap(colour = "black") +
#   geom_treemap_text(
#     aes(label = paste(names)), # Custom label format
#     colour = "white",          # Text color
#     place = "centre",          # Position of the text
#     grow = T,               # Grow text to fill the area
#     size = 9,                 # Text size
#     fontface = "bold",         # Font face
#     family = "sans"            # Font family
#   ) +
#   scale_fill_manual(values = custom_colors) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     panel.background = element_rect(fill = "white", colour = "white"),
#     plot.background = element_rect(fill = "white", colour = "white")
#   )+
#   theme(legend.position = "none") +
#   ggtitle(paste("Yearly Water Need; Total: ca. ", 
#                 total, " [m3]",sep = ""))  # Add title
# 
# ggsave("01_graphs/treemap_plot.png", units = "in", width = 3.97, height = 3.97)
# rm(df_plot,cv, custom_colors,total,k)
# 
# 
# ### Grouped Barplot 3 Scearios -------------------------------------------------
# #### 2045-2074
# 
# k <- 7
# 
# df_plot <- WSI[[k]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>% 
#   pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly), 
#                          values_to = "value", names_to = "names") %>% 
#   group_by(names) %>%
#   summarise(sum_value = sum(value)) %>% 
#   mutate(rcp = paste("RCP ",
#                      substring(names(WSI)[k],14,14),
#                      ".",
#                      substring(names(WSI)[k],15,15),
#                      sep=""))
# df_plot_1 <- WSI[[k+1]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>% 
#   pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly), 
#                values_to = "value", names_to = "names") %>% 
#   group_by(names) %>%
#   summarise(sum_value = sum(value)) %>% 
#   mutate(rcp = paste("RCP ",
#                      substring(names(WSI)[k+1],14,14),
#                      ".",
#                      substring(names(WSI)[k+1],15,15),
#                      sep=""))
# 
# df_plot_2 <- WSI[[k+2]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>% 
#   pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly), 
#                values_to = "value", names_to = "names") %>% 
#   group_by(names) %>%
#   summarise(sum_value = sum(value)) %>% 
#   mutate(rcp = paste("RCP ",
#                      substring(names(WSI)[k+2],14,14),
#                      ".",
#                      substring(names(WSI)[k+2],15,15),
#                      sep=""))
# 
# df_plot <- rbind(df_plot,df_plot_1,df_plot_2)
# rm(df_plot_1,df_plot_2,k)
# 
# # df_plot$sum_value * 116000000/1000
# 
# df_plot[df_plot[,1]=="drinking",1] <- "Drinking Water"
# df_plot[df_plot[,1]=="industial",1] <- "Industry"
# df_plot[df_plot[,1]=="irr_median",1] <- "Irrigation"
# df_plot[df_plot[,1]=="livestock_monthly",1] <- "Livestock"
# 
# ggplot(df_plot, aes(fill=names, y=sum_value, x=rcp))+
#   geom_bar(position = "stack",stat="identity", width=0.65, colour = "black", linewidth =0.1) +    
#   scale_fill_brewer(palette =  "YlGnBu")+
#   ggtitle("Annual Water Need 2045-2074") +
#   theme_minimal() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
#     panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
#     plot.background = element_rect(color="transparent",fill="white")
#   ) +
#   labs(fill="Category")+
#   xlab("") +
#   ylab("[mm]")+
#   annotate("segment", x = 0.5, xend = 3.6, y = 0, yend = 0, color = "black", size = 1.5, linetype = "solid")
# 
# ggsave("01_graphs/threebars_plot_2045_2074.png", units = "in", width = 3.97, height = 3.97)


k <- 1

df_plot <- WSI[[k]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>%
  pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly),
                         values_to = "value", names_to = "names") %>%
  group_by(names) %>%
  summarise(sum_value = sum(value)) %>%
  mutate(rcp = paste("RCP ",
                     substring(names(WSI)[k],14,14),
                     ".",
                     substring(names(WSI)[k],15,15),
                     sep=""))
df_plot_1 <- WSI[[k+1]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>%
  pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly),
               values_to = "value", names_to = "names") %>%
  group_by(names) %>%
  summarise(sum_value = sum(value)) %>%
  mutate(rcp = paste("RCP ",
                     substring(names(WSI)[k+1],14,14),
                     ".",
                     substring(names(WSI)[k+1],15,15),
                     sep=""))

df_plot_2 <- WSI[[k+2]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>%
  pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly),
               values_to = "value", names_to = "names") %>%
  group_by(names) %>%
  summarise(sum_value = sum(value)) %>%
  mutate(rcp = paste("RCP ",
                     substring(names(WSI)[k+2],14,14),
                     ".",
                     substring(names(WSI)[k+2],15,15),
                     sep=""))

df_plot <- rbind(df_plot,df_plot_1,df_plot_2)
rm(df_plot_1,df_plot_2,k)

# df_plot$sum_value * 116000000/1000

df_plot[df_plot[,1]=="drinking",1] <- "Drinking Water"
df_plot[df_plot[,1]=="industial",1] <- "Industry"
df_plot[df_plot[,1]=="irr_median",1] <- "Irrigation"
df_plot[df_plot[,1]=="livestock_monthly",1] <- "Livestock"

ggplot(df_plot, aes(fill=names, y=sum_value, x=rcp))+
  geom_bar(position = "stack",stat="identity", width=0.65, colour = "black", linewidth =0.1) +
  scale_fill_brewer(palette =  "YlGnBu")+
  ggtitle("Annual Water Need 1990-2019") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
    plot.background = element_rect(color="transparent",fill="white")
  ) +
  labs(fill="Category")+
  xlab("") +
  ylab("[mm]")+
  annotate("segment", x = 0.5, xend = 3.6, y = 0, yend = 0, color = "black", size = 1.5, linetype = "solid")

ggsave("01_graphs/threebars_plot_1990_2019.png", units = "in", width = 3.97, height = 3.97)





# ### Grouped Barplot 3 Scearios -------------------------------------------------
# #### 2020-2049
# 
# k <- 4
# 
# df_plot <- WSI[[k]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>% 
#   pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly), 
#                values_to = "value", names_to = "names") %>% 
#   group_by(names) %>%
#   summarise(sum_value = sum(value)) %>% 
#   mutate(rcp = paste("RCP ",
#                      substring(names(WSI)[k],14,14),
#                      ".",
#                      substring(names(WSI)[k],15,15),
#                      sep=""))
# df_plot_1 <- WSI[[k+1]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>% 
#   pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly), 
#                values_to = "value", names_to = "names") %>% 
#   group_by(names) %>%
#   summarise(sum_value = sum(value)) %>% 
#   mutate(rcp = paste("RCP ",
#                      substring(names(WSI)[k+1],14,14),
#                      ".",
#                      substring(names(WSI)[k+1],15,15),
#                      sep=""))
# 
# df_plot_2 <- WSI[[k+2]] %>% select(irr_median,drinking,industrial,livestock_monthly) %>% 
#   pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly), 
#                values_to = "value", names_to = "names") %>% 
#   group_by(names) %>%
#   summarise(sum_value = sum(value)) %>% 
#   mutate(rcp = paste("RCP ",
#                      substring(names(WSI)[k+2],14,14),
#                      ".",
#                      substring(names(WSI)[k+2],15,15),
#                      sep=""))
# 
# df_plot <- rbind(df_plot,df_plot_1,df_plot_2)
# rm(df_plot_1,df_plot_2,k)
# 
# # df_plot$sum_value * 116000000/1000
# 
# df_plot[df_plot[,1]=="drinking",1] <- "Drinking Water"
# df_plot[df_plot[,1]=="industial",1] <- "Industry"
# df_plot[df_plot[,1]=="irr_median",1] <- "Irrigation"
# df_plot[df_plot[,1]=="livestock_monthly",1] <- "Livestock"
# 
# ggplot(df_plot, aes(fill=names, y=sum_value, x=rcp))+
#   geom_bar(position = "stack",stat="identity", width=0.65, colour = "black", linewidth =0.1) +    
#   scale_fill_brewer(palette =  "YlGnBu")+
#   ggtitle("Annual Water Need 2045-2074") +
#   theme_minimal() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
#     panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
#     plot.background = element_rect(color="transparent",fill="white")
#   ) +
#   labs(fill="Category")+
#   xlab("") +
#   ylab("[mm]")+
#   annotate("segment", x = 0.5, xend = 3.6, y = 0, yend = 0, color = "black", size = 1.5, linetype = "solid")
# 
# ggsave("01_graphs/threebars_plot_2020_2049.png", units = "in", width = 3.97, height = 3.97)
# 
# 
# 
# 
# 
# 
# 
# ### ----------------------------------------------------------------------------
# k<- 1
# 
# # aChtung: not actual values, only relative values 
# df_brunner <- data.frame(names = c("Ecology", "Hydropower", "Irrigation", "Drinking Water", "Industry", "Livestock", "Tourism", "Snow Production"), 
#                          value = c(326658, 247312, 41679,40606,49335, 2830, 621, 331), 
#                          source = rep("Switzerland\n(Brunner et al. 2018)", 8))
# 
# df_plot <- WSI[[k]] %>% select(irr_median,drinking,industrial,livestock_monthly, ecology) %>% 
#   pivot_longer(cols = c(irr_median,drinking,industrial,livestock_monthly, ecology), values_to = "value", names_to = "names") %>% 
#   group_by(names) %>%
#   summarise(value = sum(value)) %>% 
#   mutate(source="Gürbe Ctm.")
# 
# df_plot <- rbind(df_plot, c("Hydropower",0,"Gürbe Ctm."))
# df_plot <- rbind(df_plot, c("Tourism",0,"Gürbe Ctm."))
# df_plot <- rbind(df_plot, c("Snow Production",0,"Gürbe Ctm."))
# 
# df_plot[df_plot[,1]=="ecology",1] <- "Ecology"
# df_plot[df_plot[,1]=="drinking",1] <- "Drinking Water"
# df_plot[df_plot[,1]=="industrial",1] <- "Industry"
# df_plot[df_plot[,1]=="irr_median",1] <- "Irrigation"
# df_plot[df_plot[,1]=="livestock_monthly",1] <- "Livestock"
# 
# df_plot <- rbind(df_plot,df_brunner)
# df_plot$value <- df_plot$value %>% as.numeric
# 
# ## plot ------------
# 
# ggplot(df_plot, aes(fill=names, y=value, x=source))+
#   geom_bar(position = "fill",stat="identity", width=0.65, colour = "black", linewidth =0.1)  +  
#   scale_fill_brewer(palette =  "Accent")+
#   ggtitle("Comparison Relative Water Need") +
#   theme_minimal() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
#     panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
#     plot.background = element_rect(color="transparent",fill="white")
#   ) +
#   labs(fill="Category")+
#   xlab("") +
#   ylab("[%]")+
#   annotate("segment", x = 0.5, xend = 2.6, y = 0, yend = 0, color = "black", size = 1, linetype = "solid")
# 
# ggsave("01_graphs/twobars_comp_plot_CH_with_all.png", units = "in", width = 3.97, height = 3.97)
# 
# 
# 
# 
# 
# ### ----------------------------------------------------------------------------
# 
# df_plot <-  df_plot %>% filter(!grepl("Ecology", names))
# 
# ## plot ------------
# 
# ggplot(df_plot, aes(fill=names, y=value, x=source))+
#   geom_bar(position = "fill",stat="identity", width=0.65, colour = "black", linewidth =0.1)  +  
#   scale_fill_brewer(palette =  "Accent")+
#   ggtitle("Comparison Relative Water Need") +
#   theme_minimal() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
#     panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
#     plot.background = element_rect(color="transparent",fill="white")
#   ) +
#   labs(fill="Category")+
#   xlab("") +
#   ylab("[%]")+
#   annotate("segment", x = 0.5, xend = 2.6, y = 0, yend = 0, color = "black", size = 1, linetype = "solid")
# 
# ggsave("01_graphs/twobars_comp_plot_CH_wo_ecology.png", units = "in", width = 3.97, height = 3.97)
# 
# ## -----------------------------------------------------------------------------
# df_plot <-  df_plot %>% filter(!grepl("Snow Production", names))
# df_plot <-  df_plot %>% filter(!grepl("Tourism", names))
# 
# ## plot ------------
# 
# ggplot(df_plot, aes(fill=names, y=value, x=source))+
#   geom_bar(position = "fill",stat="identity", width=0.65, colour = "black", linewidth =0.1)  +  
#   scale_fill_brewer(palette =  "Accent")+
#   ggtitle("Comparison Relative Water Need") +
#   theme_minimal() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
#     panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
#     plot.background = element_rect(color="transparent",fill="white")
#   ) +
#   labs(fill="Category")+
#   xlab("") +
#   ylab("[%]")+
#   annotate("segment", x = 0.5, xend = 2.6, y = 0, yend = 0, color = "black", size = 1, linetype = "solid")
# 
# ggsave("01_graphs/twobars_comp_plot_CH_wo_ecology_and_sp_tour.png", units = "in", width = 3.97, height = 3.97)
# 
# ## ----------------------------------------------------------------------------
# 
# df_plot <-  df_plot %>% filter(!grepl("Hydropower", names))
# 
# ## plot ------------
# 
# ggplot(df_plot, aes(fill=names, y=value, x=source))+
#   geom_bar(position = "fill",stat="identity", width=0.65, colour = "black", linewidth =0.1)  +  
#   scale_fill_brewer(palette =  "Accent")+
#   ggtitle("Comparison Relative Water Need") +
#   theme_minimal() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "grey",linewidth = 0.25),
#     panel.grid.minor.y = element_line(color = "grey",linewidth = 0.1),
#     plot.background = element_rect(color="transparent",fill="white")
#   ) +
#   labs(fill="Category")+
#   xlab("") +
#   ylab("[%]")+
#   annotate("segment", x = 0.5, xend = 2.6, y = 0, yend = 0, color = "black", size = 1, linetype = "solid")
# 
# ggsave("01_graphs/twobars_comp_plot_CH_only_four.png", units = "in", width = 3.97, height = 3.97)
# 
# rm(df_brunner,df_plot, legend_plot, list_plots_regime,arranged_plot)
# 
# ## ----------------------------------------------------------------------------
