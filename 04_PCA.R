##---------------------------------------------------------------------------------------------------------------------
## Principal Component Analysis
## Title: Code Script for assessing the compatibility of Single-Scan Terrestrial LiDAR with Digital Aerial Photogrammetry and Field Inventory"
## Author: Magnus Onyiriagwu Supervised by: Clara Zemp
##---------------------------------------------------------------------------------------------------------------------

## load libraries 
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2) 
library(MetBrewer)

# create file path to base directory 
dir <- getwd()

## import dataset 
df.all <- read.csv(paste0(dir, "/data/Allmetrics.csv")) |>
  # filter out tea plantation plots
  filter(LandUse %in% c("Agroforest", "Forest")) |>
  # remove the undesired metrics
  dplyr::select(!c("X", "plot_id", "scan_id", "zskew", "zkurt")) |>
  mutate_if(is.integer, as.numeric)

## For DAP
df.uav <- df.all |> dplyr::select(all_of(c("zsd", "zq75", "zq50", "zq25", "zmean", "zmax", "zentropy", 
                         "rumple", "LAI", "gapFrac", "FHD", "CR", "LandUse")))

## For TLS
df.tls <- df.all |> dplyr::select(all_of(c("SSCI", "MeanFrac", "ENL", "UCI", "can.open", "TopH", "LandUse")))

## For Ground
df.field <- df.all |> dplyr::select(all_of(c("maxH", "sdDBH", "nStems", "BA", "sdH", "meanH", "meanDBH", "sdBA", "LandUse")))


#####################################################
# Global PCA
ss.pca <- PCA(scale(df.all[, -which(names(df.all) == "LandUse")]), graph = F)

# Scree plot
fviz_eig(ss.pca, addlabels=T,  
         barfill="grey", barcolor ="brown",
         ncp = 8, labelsize = 2) + 
  ylim(0, 60) + 
  theme_classic() + labs(title = "", x = "Principal Components (PC)", y = "Explained variance [%]") + 
  theme(title = element_text(size = 6, face = "bold"),
        text = element_text(size = 4),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6))

# Biplot
picBiplot <- fviz_pca_biplot(ss.pca, fill.ind = df.all$LandUse, col.ind = "white", 
                            # habillage = factor(df.all$type),
                             geom.ind = "point", palette = met.brewer("VanGogh3", 2), 
                             label ="var", col.var = "black", labelsize = 3, repel=TRUE,
                             arrowsize = 0.1, arrowshape = "closed", 
                             pointshape = 21, pointsize = 2, alpha.ci = 0.5, 
                             addEllipses = F) +
  labs(title = "Global PCA", 
       x = "PC1 [44.6%]", y = "PC2 21.1%]",
       fill = "Land Use") + 
  theme(legend.position = "right", 
    legend.key.size = unit(0.05, "cm"),
    legend.key.width = unit(0.05,"cm"),
    legend.text=element_text(size=6),
    legend.title=element_text(size=6),
    title = element_text(size = 8, face = "bold"),
    text = element_text(size = 3, face = "bold"),
    axis.text = element_text(size = 8, face = "bold"), 
    axis.title = element_text(size = 8, face = "bold")) + 
  xlim(-6,6) + ylim(-6,6) 
picBiplot        

# Merge plots into a single plot 
(fig1 <- plot_grid(picBiplot, 
                   labels = c("A"), label_size = 8))


############################################################
## PCA TLS 
which(names(df.tls) == "LandUse")
tls.pca <- PCA(scale(df.tls[, -which(names(df.tls) == "LandUse")]), graph = F)


# Scree plot
fviz_eig(tls.pca, addlabels=T,  
         barfill="grey", barcolor ="brown",
         ncp = 8, labelsize = 2) + 
  ylim(0, 80) + 
  theme_classic() + labs(title = "", x = "Principal Components (PC)", y = "Explained variance [%]") + 
  theme(title = element_text(size = 6, face = "bold"),
        text = element_text(size = 4),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6))

# Biplot
(tlsBiplot <- fviz_pca_biplot(tls.pca, fill.ind = df.tls$LandUse, col.ind = "white", 
                              # habillage = factor(df.all$type),
                              geom.ind = "point", palette = met.brewer("VanGogh3", length(unique(df.tls$LandUse))), 
                              label ="var", col.var = "black", labelsize = 3, repel=TRUE,
                              arrowsize = 0.1, arrowshape = "closed", 
                              pointshape = 21, pointsize = 2, alpha.ci = 0.5, 
                              addEllipses = F) +
    labs(title = "TLS",
         x = "PC1 [55.4%]", y = "PC2 [26.9%]",
         fill = "Land Use") + 
    theme(legend.position = "none", 
          legend.key.size = unit(0.05, "cm"),
          legend.key.width = unit(0.05,"cm"),
          legend.text=element_text(size=6),
          legend.title=element_text(size=6),
          title = element_text(size = 8, face = "bold"),
          text = element_text(size = 3, face = "bold"),
          axis.text = element_text(size = 8, face = "bold"), 
          axis.title = element_text(size = 8, face = "bold")) + 
    xlim(-4,4) + ylim(-4,4))



#####################################################

## PCA DAP
which(names(df.uav) == "LandUse")
uav.pca <- PCA(scale(df.uav[, -which(names(df.uav) == "LandUse")]), graph = F)


# Scree plot
fviz_eig(uav.pca, addlabels=T,  
         barfill="grey", barcolor ="brown",
         ncp = 8, labelsize = 2) + 
  ylim(0, 80) + 
  theme_classic() + labs(title = "", x = "Principal Components (PC)", y = "Explained variance [%]") + 
  theme(title = element_text(size = 6, face = "bold"),
        text = element_text(size = 4),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6))

# Biplot
(uavBiplot <- fviz_pca_biplot(uav.pca, fill.ind = df.uav$LandUse, col.ind = "white", 
                              # habillage = factor(df.all$type),
                              geom.ind = "point", palette = met.brewer("VanGogh3", length(unique(df.uav$LandUse))), 
                              label ="var", col.var = "black", labelsize = 3, repel=TRUE,
                              arrowsize = 0.1, arrowshape = "closed", 
                              pointshape = 21, pointsize = 2, alpha.ci = 0.5, 
                              addEllipses = F) +
    labs(title = "DAP",
         x = "PC1 [68.8%]", y = "PC2 [17.4%]",
         fill = "Land Use") + 
    theme(legend.position = "none", 
          legend.key.size = unit(0.05, "cm"),
          legend.key.width = unit(0.05,"cm"),
          legend.text=element_text(size=6),
          legend.title=element_text(size=6),
          title = element_text(size = 8, face = "bold"),
          text = element_text(size = 3, face = "bold"),
          axis.text = element_text(size = 8, face = "bold"), 
          axis.title = element_text(size = 8, face = "bold")) + 
    xlim(-4,4) + ylim(-4,4))


#####################################################

## PCA Field
which(names(df.field) == "LandUse")
field.pca <- PCA(scale(df.field[, -which(names(df.field) == "LandUse")]), graph = F)

# Scree plot
fviz_eig(field.pca, addlabels=T,  
         barfill="grey", barcolor ="brown",
         ncp = 8, labelsize = 2) + 
  ylim(0, 80) + 
  theme_classic() + labs(title = "", x = "Principal Components (PC)", y = "Explained variance [%]") + 
  theme(title = element_text(size = 6, face = "bold"),
        text = element_text(size = 4),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6))

# Biplot
(fieldBiplot <- fviz_pca_biplot(field.pca, fill.ind = df.field$LandUse, col.ind = "white", 
                                # habillage = factor(df.all$type),
                                geom.ind = "point", palette = met.brewer("VanGogh3", length(unique(df.field$LandUse))), 
                                label ="var", col.var = "black", labelsize = 3, repel=TRUE,
                                arrowsize = 0.1, arrowshape = "closed", 
                                pointshape = 21, pointsize = 2, alpha.ci = 0.5, 
                                addEllipses = F) +
    labs(title = "Field",
         x = "PC1 [47.1%]", y = "PC2 [26.8%]",
         fill = "Land Use") + 
    theme(legend.position = "none", 
          legend.key.size = unit(0.05, "cm"),
          legend.key.width = unit(0.05,"cm"),
          legend.text=element_text(size=6),
          legend.title=element_text(size=6),
          title = element_text(size = 8, face = "bold"),
          text = element_text(size = 3, face = "bold"),
          axis.text = element_text(size = 8, face = "bold"), 
          axis.title = element_text(size = 8, face = "bold")) + 
    xlim(-4,4) + ylim(-4,4))


#####################################################
## Merge plots into a single plot 
(fig2 <- plot_grid(tlsBiplot, uavBiplot, fieldBiplot, 
                   ncol = 3, labels = c("B", "C", "D"), label_size = 8))
(figall <- plot_grid(fig1, fig2,  
                     nrow = 2, label_size = 8))

## Export as png
save_plot(figall, filename = paste0(dir, "/scripts/output/images/allpcacomb.png"), 
          dpi = 600)


