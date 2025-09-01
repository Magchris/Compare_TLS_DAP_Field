##---------------------------------------------------------------------------------------------------------------------
## Merge TLS, DAP, & Field metrics
## Title: Code Script for assessing the compatibility of Single-Scan Terrestrial LiDAR with Digital Aerial Photogrammetry and Field Inventory"
## Author: Magnus Onyiriagwu Supervised by: Clara Zemp
##---------------------------------------------------------------------------------------------------------------------

## load the required packages
library(dplyr)

## create path to directory. Set working directory to the root folder containing the codes
dir <- getwd()

## import the plot information
scanps <- read.csv(paste0(dir, "/data/scan.csv"))[, c("plot_id", "plot", "type")]

## import TLS metrics
tlsmet <- read.csv(paste0(dir, "/data/TLSmetrics.csv")) |>
  dplyr::select(all_of(c("canopy.height", "enl", "ssci", "canopy.openness", "mean.frac", "uci", "plot_id"))) |>
  setNames(c("TopH", "ENL", "SSCI", "can.open", "MeanFrac", "UCI", "scan_id"))

## Add plot meta information to TLS data
tlsmet <- merge(tlsmet, scanps, by.x = "scan_id", by.y = "plot_id")

## import DAP metrics
uavmet <- read.csv(paste0(dir, "/data/UAVmetrics.csv"), header = T) |>
  dplyr::select(!c("X"))

## import field metrics 
fieldmet <- read.csv(paste0(dir, "/data/FieldMetrics.csv")) |>
  dplyr::select(all_of(c("G_nstems", "G_BA", "G_sdBA", "G_meanBA", "G_sdH", "G_maxH", "G_meanH", "G_meanDBH", "G_sdDBH", "plot_id"))) |>
  setNames(c("nStems", "BA", "sdBA", "meanBA", "sdH", "maxH", "meanH", "meanDBH", "sdDBH", "plot_id"))

## Merge all data set
allmetrics <- merge(uavmet, fieldmet, by = "plot_id")
allmetrics <- merge(allmetrics, tlsmet, by.x = "plot_id", by.y = "plot") 

## Export dataframe
write.csv(allmetrics, paste0(dir, "/data/Allmetrics.csv"))

