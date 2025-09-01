---
Title: "Compatibility of Single-Scan Terrestrial LiDAR with Digital Aerial Photogrammetry and Field Inventory"
Author: "Magnus Onyiriagwu"
---

The R-scripts contains the codes used in performing the statistics in the above mentioned paper. Details on the variable extraction are well documented in Mohan et al., 2021 for the digital aerial photogrammetry (DAP) data and Ehbrecht et al., 2017 for the single-scan terrestrial laser scanning (TLS) metrics. 

# Script 01_CombineMetrics: The derived metrics from the single-scan TLS, DTM-independent DAP and field inventory are merged into a data table. 

# Script 02_BivariateCorrelation: Computes the Spearman correlation of all pairs of the three dataset and generates a correlation heatmap. The percentage of significant correlations were also extracted from the correlation matrix

# Script 03_Procrustes: Computes Procrustes correlation for the sets of analogous and equivalent metrics between the DAP-TLS, DAP-Field, and TLS-Field structural variables. Each data source were grouped into four structural aspects, vertical structure, horizontal structure, vegetation density, and structural heterogeneity, and compared with similar aspects from the other two data sources. 

# Script 04_PCA: Extrapolates the principal component and variable loading of all the structural variables

# Scrip 05_Classification: Performs random forest classification on the combined set of the variables. 


