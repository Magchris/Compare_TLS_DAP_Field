# load the required packages  
library(ggplot2)
library(vegan)
library(FactoMineR)
library(cowplot)
library(ggpubr)
library(stringr)
library(RColorBrewer)

# create file path to base directory 
dir <- getwd()

# Prepare data ####
df.all <- read.csv(paste0(dir, "/data/Allmetrics.csv")) |>
  # remove the undesired metrics
  dplyr::select(!c("X", "plot_id", "zskew", "zkurt")) |>
  # filter out tea plantation plots
  filter(LandUse %in% c("Agroforest", "Forest")) |>
  mutate(LandUse = factor(LandUse, levels=c("Forest","Agroforest")))

#####################################################
# PCA for UAV-photogrammetry Metrics

# select required columns
pcols <- c("zmax", "CR", "rumple", "zq75", "zq50", "zq25",
           "zentropy", "LAI", "FHD", "gapFrac", "zsd")
pcols.het <- c("rumple", "CR") # heterogeneity
pcols.ver <- c("zq75", "zq50", "zq25", "zmax", "zentropy", "zsd") # vertical structure
pcols.hor <- c("gapFrac") # gap metrics
pcols.vden <- c("LAI", "FHD") # vegetation density metrics

# subset the selected variables 
uavcols <- df.all[, pcols]

# run the PCA
uav.pca <- vegan::rda(uavcols, scale = TRUE)


#####################################################
# PCA for TLS Metrics

# select required columns
scols <- c("ENL", "SSCI", "can.open", "UCI", "TopH", "MeanFrac")
scols.het <- c("SSCI", "UCI") # heterogeneity
scols.ver <- c("TopH", "ENL") # Vertical structure
scols.hor <- c("can.open", "gapArea") # Gao
scols.vden <- c("MeanFrac", "UCI") # Vegetation density

# subset the selected variables 
tlscols <- df.all[, scols] # no missing values

# run the PCA
tls.pca <- vegan::rda(tlscols, scale = TRUE)

#####################################################
# PCA for Ground data

gcols <- c("sdDBH", "meanDBH", "sdH", "meanH", 
           "maxH", "BA", "nStems", "sdBA") 
gcols.het <- c("sdDBH", "sdBA") # Heterogeneity
gcols.ver <- c("sdH", "meanH", "maxH") # Vertical Structure
gcols.vden <- c("BA", "nStems", "meanDBH") # Vegetation density


# subset the ground metrics
fieldcols <- df.all[, gcols]   # no missing values

# run the PCA
grd.pca <- vegan::rda(fieldcols, scale = TRUE)


## Procrustes
# Cross-platform metrics Prcrustes Rotation

# UAV - TLS
proc.uavtls <- vegan::procrustes(uav.pca, tls.pca, scale = TRUE)
# TLS - Ground
proc.tlsgrd <- vegan::procrustes(tls.pca, grd.pca, scale = TRUE)
# UAV - Ground
proc.uavgrd <- vegan::procrustes(uav.pca, grd.pca, scale = TRUE)


## Test the significance of the procrustes
## UAV - TLS
(protest.uavtls <- vegan::protest(tls.pca, uav.pca, scores = "sites", permutations = 999))
## TLS - Ground
(protest.tlsgrd <- vegan::protest(tls.pca, grd.pca, scores = "sites", permutations = 999))
## UAV - Ground
(protest.uavgrd <- vegan::protest(uav.pca, grd.pca, scores = "sites", permutations = 999))



## Visualization
# Procrustes plot for UAV and TLS metrics

# set color paletter
display.brewer.pal(n = 9, name = "Greens")

# set up data table for plotting the procrustes rotation
df.uavtls <- data.frame(rda1 = proc.uavtls$Yrot[,1], rda2 = proc.uavtls$Yrot[,2], 
                        xrda1 = proc.uavtls$X[, 1], xrda2 = proc.uavtls$X[,2],
                        plots = rownames(proc.uavtls$Yrot)) 

# Assign names to the land use 
df.uavtls$landuse <- df.all$LandUse

## plotting 
(procplot1 <- df.uavtls %>%
    ggplot(aes(rda1, rda2)) + 
    geom_point(color = "#74C476", size = 0.5) +
    geom_point(aes(xrda1, xrda2), color = "#00441B", size = 1.2) + 
    geom_segment(aes(rda1, rda2, xend = xrda1, yend = xrda2), colour = "lightgrey", 
                 arrow = arrow(type = "closed", length = unit(0.08, "cm")), alpha = 0.7) + 
    annotate("text", x = min(df.uavtls$xrda1) + 0.3, y = max(df.uavtls$xrda2) - 0.5, 
             label = paste("italic(m) ^ 2 == ", round(protest.uavtls$ss, 2)), parse = TRUE, size = 2) + 
    annotate("text", x = min(df.uavtls$xrda1) + 0.3, y = max(df.uavtls$xrda2) - 0.7, 
             label = paste("p = ", round(protest.uavtls$signif, 2)), size = 2) +
    annotate("text", x = min(df.uavtls$xrda1) + 0.3, y = max(df.uavtls$xrda2) - 0.9, 
             label = paste("R = ", round(protest.uavtls$t0, 2)), size = 2) +
    xlab(expression(bold("Dimension 1"))) + 
    ylab(expression(bold("Dimension 2"))) + 
    theme_bw() +
    labs(title = "TLS - DAP") +
    theme(legend.position = "none", 
          title = element_text(size = 6, face = "bold"),
          text = element_text(size = 6),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6, face = "bold"))
)


## Procrustes plot for UAV and Ground metrics
# set up data table for plotting the procrustes rotation
df.uavgrd <- data.frame(rda1 = proc.uavgrd$Yrot[,1], rda2 = proc.uavgrd$Yrot[,2], 
                        xrda1 = proc.uavgrd$X[, 1], xrda2 = proc.uavgrd$X[,2],
                        plots = rownames(proc.uavgrd$Yrot)) 

# Assign names to the land use 
df.uavgrd$landuse <- df.all$LandUse


## plotting
(procplot2 <- df.uavgrd %>%
    ggplot(aes(rda1, rda2)) + 
    geom_point(color = "#74C476", size = 0.5) +
    geom_point(aes(xrda1, xrda2), color = "#00441B", size = 1.2) + 
    geom_segment(aes(rda1, rda2, xend = xrda1, yend = xrda2), colour = "lightgrey", 
                 arrow = arrow(type = "closed", length = unit(0.08, "cm")), alpha = 0.7) + 
    annotate("text", x = min(df.uavgrd$xrda1) + 0.3, y = max(df.uavgrd$xrda2) - 0.5, 
             label = paste("italic(m) ^ 2 == ", round(protest.uavgrd$ss, 2)), parse = TRUE, size = 2) + 
    annotate("text", x = min(df.uavgrd$xrda1) + 0.3, y = max(df.uavgrd$xrda2) - 0.7,
             label = paste("p = ", round(protest.uavgrd$signif, 2)), size = 2) +
    annotate("text", x = min(df.uavgrd$xrda1) + 0.3, y = max(df.uavgrd$xrda2) - 0.9, 
             label = paste("R = ", round(protest.uavgrd$t0, 2)), size = 2) +
    xlab(expression(bold("Dimension 1"))) + 
    ylab(expression(bold("Dimension 2"))) + 
    theme_bw() +
    labs(title = "DAP - Field") +
    theme(legend.position = "none", 
          title = element_text(size = 6, face = "bold"),
          text = element_text(size = 6),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6, face = "bold"))
)


## Procrustes plot for TLS and Ground metrics

# set up data table for plotting the procrustes rotation
df.tlsgrd <- data.frame(rda1 = proc.tlsgrd$Yrot[,1], rda2 = proc.tlsgrd$Yrot[,2], 
                        xrda1 = proc.tlsgrd$X[, 1], xrda2 = proc.tlsgrd$X[,2],
                        plots = rownames(proc.tlsgrd$Yrot)) 

# Assign names to the land use 
df.tlsgrd$landuse <- df.all$LandUse

## plotting
(procplot3 <- df.tlsgrd %>%
    ggplot(aes(rda1, rda2)) + 
    geom_point(color = "#74C476", size = 0.5) +
    geom_point(aes(xrda1, xrda2), color = "#00441B", size = 1.2) + 
    geom_segment(aes(rda1, rda2, xend = xrda1, yend = xrda2), colour = "lightgrey", 
                 arrow = arrow(type = "closed", length = unit(0.08, "cm")), alpha = 0.7) + 
    annotate("text", x = min(df.uavgrd$xrda1) + 0.3, y = max(df.uavgrd$xrda2) - 0.5, 
             label = paste("italic(m) ^ 2 == ", round(protest.tlsgrd$ss, 2)), parse = TRUE, size = 2) + 
    annotate("text", x = min(df.uavgrd$xrda1) + 0.3, y = max(df.uavgrd$xrda2) - 0.7,
             label = paste("p = ", round(protest.tlsgrd$signif, 2)), size = 2) +
    annotate("text", x = min(df.uavgrd$xrda1) + 0.3, y = max(df.uavgrd$xrda2) - 0.9, 
             label = paste("R = ", round(protest.tlsgrd$t0, 2)), size = 2) +
    xlab(expression(bold("Dimension 1"))) + 
    ylab(expression(bold("Dimension 2"))) + 
    theme_bw() +
    labs(title = "TLS - Field") +
    theme(legend.position = "none", 
          title = element_text(size = 6, face = "bold"),
          text = element_text(size = 6),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6, face = "bold"))
)
## Change the parameters for the vertical and heterogeneity metrics and rerun the plotting 

## Create grid to save graph
(procplot.hor <- plot_grid(procplot1, procplot2, procplot3, labels = "AUTO", ncol = 3, label_size = 10))
(procplot.ver <- plot_grid(procplot1, procplot2, procplot3, labels = c("E", "F", "G"), ncol = 3, label_size = 10))
(procplot.het <- plot_grid(procplot1, procplot2, procplot3, labels = c("H", "I", "J"), ncol = 3, label_size = 10))


# Create plot titles for the rows of the metrics
title1 <- ggdraw() + 
  draw_label("Horizontal Metrics", fontface = 'bold', size = 8) +
  theme()# plot.margin = margin(0,0,3,7))
title2 <- ggdraw() + 
  draw_label("Vertical Metrics", fontface = 'bold', size = 8) +
  theme() # plot.margin = margin(0,0,3,7))
title3 <- ggdraw() + 
  draw_label("Heterogeneity Metrics", fontface = 'bold', size = 8) +
  theme() # plot.margin = margin(0,0,3,7))

# Merge the plot grid
(procplot <- plot_grid(title1, procplot.hor, title2, procplot.ver, title3, procplot.het, 
                       labels = "", nrow = 6, label_size = 10, 
                       rel_heights = c(0.05, 0.6, 0.05, 0.6, 0.05, 0.6)))


####### Export the plot as image 
ggsave(paste0(dir, "/output/images/procplot.jpg"), procplot, width = 20, height = 24, unit = "cm")

