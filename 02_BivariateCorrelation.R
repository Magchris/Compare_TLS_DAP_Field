# Load the required libraries
require(ggplot2)
require(dplyr)
library(cowplot)
library(MetBrewer)

## create path to directory. Set working directory to the root folder containing the codes
dir <- getwd()

## import dataset 
df.all <- read.csv(paste0(dir, "/data/Allmetrics.csv")) |>
  # filter out tea plantation plots
  filter(LandUse %in% c("Agroforest", "Forest")) |>
  # remove the undesired metrics
  dplyr::select(!c("X", "plot_id", "scan_id", "zskew", "zkurt", "LandUse")) |>
  mutate_if(is.integer, as.numeric)

##################################################################

# Initialize empty dataframe to store correlation values and p-values
cor_results <- data.frame(Var1 = character(),
                          Var2 = character(),
                          Correlation = numeric(),
                          p_value = numeric(),
                          stringsAsFactors = FALSE)

# Calculate correlation and p-value for each pair
for(i in 1:ncol(df.all)) {
  for(j in 1:ncol(df.all)) {
    if(i != j) { # Avoid self-correlation
      test <- cor.test(df.all[[i]], df.all[[j]], method = "spearman")
      cor_results <- rbind(cor_results, data.frame( 
        Var1 = colnames(df.all)[i],
        Var2 = colnames(df.all)[j],
        Correlation = test$estimate,
        p_value = test$p.value
      ))
    }
  }
}

# Filter for significant correlations (e.g., p < 0.05)
cor_results <- cor_results %>% filter(p_value < 0.05)

# Define labels for metric types to filter DAP metrics vs. TLS/Field metrics
cor_results$Metric_Type <- ifelse(grepl("zsd|zq75|zq50|zq25|zmean|zmax|zentropy|rumple|LAI|gapFrac|FHD|CR", cor_results$Var2), "DAP", 
                                  ifelse(grepl("SSCI|MeanFrac|ENL|can.open|TopH|UCI|maxH|sdDBH|nStems|BA|sdBA|sdH|meanH|meanDBH|meanBA", cor_results$Var2), "Field/TLS", " "))
cor_results$Metric_Type_y <- ifelse(grepl("zsd|zq75|zq50|zq25|zmean|zmax|zentropy|rumple|LAI|gapFrac|FHD|CR", cor_results$Var1), "DAP", 
                                    ifelse(grepl("SSCI|MeanFrac|ENL|can.open|TopH|UCI|maxH|sdDBH|nStems|BA|sdBA|sdH|meanH|meanDBH|meanBA", cor_results$Var1), "Field/TLS", " "))

# Keep only relevant correlations between DAP (rows) and TLS/Field (columns)
cor_filtered <- cor_results %>% 
  filter(Metric_Type == "Field/TLS" & Metric_Type_y == "DAP")

# Create a custom order for the TLS and field metrics for grouping
cor_filtered$Var2 <- factor(cor_filtered$Var2, levels = c("SSCI", "MeanFrac", "ENL", "can.open", "TopH", "UCI",
                                                          "maxH", "sdDBH", "nStems", "BA", "sdBA", "sdH", "meanH", "meanDBH"))

# Create heatmap with significant correlation coefficients displayed
col <- met.brewer("VanGogh3", 3)

(corplot <- ggplot(cor_filtered, aes(Var2, Var1, fill = Correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "darkgrey", high = "#1f5b25", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Corr") +
    geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) + # Display only significant coefficients
    theme_classic() +
    labs(x = "TLS and Field Metrics", y = "DAP Metrics") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
          axis.title = element_text(face = "bold", size = 8), 
          axis.text = element_text(face = "bold", size = 8), 
          legend.text = element_text(size = 6), 
          legend.title = element_text(size = 6, face = "bold"), 
          legend.key.size = unit(0.4, "cm")) +
    geom_vline(xintercept = 5.5, colour = "black", lty = 2, lwd = 0.4)) 

## save correlation heatmap
save_plot(corplot, filename = paste0(dir, "/scripts/output/images/corplot.pdf"), dpi = 600)


##################################################################
# Estimate the proportion of correlating pairs 
require(Hmisc)

# Subset the variables into three groups
uav_vars <- c("zsd", "zq75", "zq50", "zq25", "zmean", "zmax", "zentropy", 
              "rumple", "LAI", "gapFrac", "FHD", "CR")
tls_vars <- c("SSCI", "ENL", "can.open", "TopH", "UCI")
field_vars <- c("maxH", "sdDBH", "nStems", "BA", "sdBA", "sdH", "meanH", "meanDBH")

# Ensure these variables are present in your dataset
uav_data <- df.all %>% dplyr::select(all_of(uav_vars))
tls_data <- df.all %>% dplyr::select(all_of(tls_vars))
field_data <- df.all %>% dplyr::select(all_of(field_vars))

# Compute Spearman correlation matrices and p-values
uav_tls_cor <- rcorr(as.matrix(cbind(uav_data, tls_data)), type = "spearman")
uav_field_cor <- rcorr(as.matrix(cbind(uav_data, field_data)), type = "spearman")
tls_field_cor <- rcorr(as.matrix(cbind(tls_data, field_data)), type = "spearman")


#############
# Identify significant correlations (p-value < 0.05)
uav_tls_matrix <- uav_tls_cor$P < 0.05
uav_field_matrix <- uav_field_cor$P < 0.05
tls_field_matrix <- tls_field_cor$P < 0.05

# Replace NA values (diagonal) with FALSE, since we don't consider self-correlations
uav_tls_matrix[is.na(uav_tls_matrix)] <- FALSE
uav_field_matrix[is.na(uav_field_matrix)] <- FALSE
tls_field_matrix[is.na(tls_field_matrix)] <- FALSE

# Calculate the proportion of significant correlations
uav_tls_significant <- sum(uav_tls_matrix) / sum(!is.na(uav_tls_cor$P))
uav_field_significant <- sum(uav_field_matrix) / sum(!is.na(uav_field_cor$P))
tls_field_significant <- sum(tls_field_matrix) / sum(!is.na(tls_field_cor$P))

# Print the result
print(paste("Proportion of significant correlations between UAV and TLS: ", uav_tls_significant))
print(paste("Proportion of significant correlations between UAV and Field: ", uav_field_significant))
print(paste("Proportion of significant correlations between TLS and Field: ", tls_field_significant))

