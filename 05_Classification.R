#### Title: Random Forest Classification 

## Call the required libraries
library(party)
library(dplyr)
library(MetBrewer)
library(ggpubr)
library(cowplot)
library(party)
library(tidyr)

# create path to base directory 
dir <- getwd()

## import dataset 
df.all <- read.csv(paste0(dir, "/data/Allmetrics.csv")) |>
  # filter out tea plantation plots
  filter(LandUse %in% c("Agroforest", "Forest")) |>
  # remove the undesired metrics
  dplyr::select(!c("X", "plot_id", "scan_id", "zskew", "zkurt")) |>
  mutate_if(is.character, factor)

## For uav
df.uav <- df.all |> dplyr::select(all_of(c("zsd", "zq75", "zq50", "zq25", "zmean", "zmax", "zentropy", 
                                           "rumple", "LAI", "gapFrac", "FHD", "CR", "LandUse")))

## For TLS
df.tls <- df.all |> dplyr::select(all_of(c("SSCI", "MeanFrac", "ENL", "UCI", "can.open", "TopH", "LandUse")))

## For Ground
df.field <- df.all |> dplyr::select(all_of(c("maxH", "sdDBH", "nStems", "BA", "sdH", "meanH", "meanDBH", "sdBA", "LandUse")))

#########################################################################
#------------- variable importance ranking using conditional permutation
set.seed(440875)
imp <-  varimp(cforest(LandUse ~., data=df.all, control = cforest_unbiased(ntree = 50)), 
               conditional = TRUE)
df.imp <- data.frame(imp)
df.imp$varnames <- rownames(df.imp)  
rownames(df.imp) <- NULL
names(df.imp)[1] <- 'IncMSE'

# Assign names to the different variable sources
df.imp$varclass <- ifelse(grepl("zsd|zq75|zq50|zq25|zmean|zmax|zentropy|rumple|LAI|gapFrac|FHD|CR", df.imp$varnames), "DAP", 
                          ifelse(grepl("SSCI|ENL|MeanFrac|can.open|TopH|UCI", df.imp$varnames), "TLS", 
                                 ifelse(grepl("maxH|sdDBH|nStems|BA|sdBA|sdH|meanH|meanDBH|meanBA", df.imp$varnames), "Field", " ")))

df.imp$strclass <- ifelse(grepl("zsd|zq50|zq25|zmean|zentropy|ENL|sdH|meanH|FHD", df.imp$varnames), "Vertical (Internal)", 
                          ifelse(grepl("zmax|TopH|maxH|zq75", df.imp$varnames), "Vertical (Top)", 
                                 ifelse(grepl("FHD|LAI|MeanFrac|nStems|BA|meanDBH", df.imp$varnames), "Vegetation Density", 
                                        ifelse(grepl("rumple", df.imp$varnames), "Heterogeneity (Top)", 
                                               ifelse(grepl("UCI|CR|SSCI|sdDBH|sdBA", df.imp$varnames), "Heterogeneity (Internal)", 
                                                      ifelse(grepl("can.open|gapFrac", df.imp$varnames), "Horizontal Structure",  " "))))))

####################################################################
## TLS Classification
set.seed(440875)
imp.tls <-  varimp(cforest(LandUse ~., data=df.tls, control = cforest_unbiased(ntree = 50)), 
               conditional = TRUE)
df.imptls <- data.frame(imp.tls)
df.imptls$varnames <- rownames(df.imptls)  
rownames(df.imptls) <- NULL
names(df.imptls)[1] <- 'IncMSE'

# Assign names to the different variable sources
df.imptls$strclass <- ifelse(grepl("zsd|zq50|zq25|zmean|zentropy|ENL|sdH|meanH|FHD", df.imptls$varnames), "Vertical (Internal)", 
                          ifelse(grepl("zmax|TopH|maxH|zq75", df.imptls$varnames), "Vertical (Top)", 
                                 ifelse(grepl("FHD|LAI|MeanFrac|nStems|BA|meanDBH", df.imptls$varnames), "Vegetation Density", 
                                        ifelse(grepl("rumple", df.imptls$varnames), "Heterogeneity (Top)", 
                                               ifelse(grepl("UCI|CR|SSCI|sdDBH|sdBA", df.imptls$varnames), "Heterogeneity (Internal)", 
                                                      ifelse(grepl("can.open|gapFrac", df.imptls$varnames), "Horizontal Structure",  " "))))))



####################################################################
## UAV Classification
set.seed(440875)
imp.uav <-  varimp(cforest(LandUse ~., data=df.uav, control = cforest_unbiased(ntree = 50)), 
                   conditional = TRUE)
df.impuav <- data.frame(imp.uav)
df.impuav$varnames <- rownames(df.impuav)  
rownames(df.impuav) <- NULL
names(df.impuav)[1] <- 'IncMSE'

# Assign names to the different variable sources
df.impuav$strclass <- ifelse(grepl("zsd|zq50|zq25|zmean|zentropy|ENL|sdH|meanH|FHD", df.impuav$varnames), "Vertical (Internal)", 
                             ifelse(grepl("zmax|TopH|maxH|zq75", df.impuav$varnames), "Vertical (Top)", 
                                    ifelse(grepl("FHD|LAI|MeanFrac|nStems|BA|meanDBH", df.impuav$varnames), "Vegetation Density", 
                                           ifelse(grepl("rumple", df.impuav$varnames), "Heterogeneity (Top)", 
                                                  ifelse(grepl("UCI|CR|SSCI|sdDBH|sdBA", df.impuav$varnames), "Heterogeneity (Internal)", 
                                                         ifelse(grepl("can.open|gapFrac", df.impuav$varnames), "Horizontal Structure",  " "))))))


####################################################################
## Field Classification
set.seed(440875)
imp.field <-  varimp(cforest(LandUse ~., data=df.field, control = cforest_unbiased(ntree = 50)), 
                   conditional = TRUE)
df.impfield <- data.frame(imp.field)
df.impfield$varnames <- rownames(df.impfield)  
rownames(df.impfield) <- NULL
names(df.impfield)[1] <- 'IncMSE'

# Assign names to the different variable sources
df.impfield$strclass <- ifelse(grepl("zsd|zq50|zq25|zmean|zentropy|ENL|sdH|meanH|FHD", df.impfield$varnames), "Vertical (Internal)", 
                             ifelse(grepl("zmax|TopH|maxH|zq75", df.impfield$varnames), "Vertical (Top)", 
                                    ifelse(grepl("FHD|LAI|MeanFrac|nStems|BA|meanDBH", df.impfield$varnames), "Vegetation Density", 
                                           ifelse(grepl("rumple", df.impfield$varnames), "Heterogeneity (Top)", 
                                                  ifelse(grepl("UCI|CR|SSCI|sdDBH|sdBA", df.impfield$varnames), "Heterogeneity (Internal)", 
                                                         ifelse(grepl("can.open|gapFrac", df.impfield$varnames), "Horizontal Structure",  " "))))))



####################################################################
## TLS Plotting 
(ptls <- ggdotchart(df.imptls, x = "varnames", y = "IncMSE",
                   color = "strclass",
                   palette = rev(met.brewer("Nattier", length(unique(df.imp$strclass)))), 
                   sorting = "descending",  
                   add = "segments",
                   add.params = list(color = "lightgray", size = 1), 
                   dot.size = 2,                                 
                   #label = round(df.imp$IncMSE,2), 
                   xlab = "",
                   ylab = "MDA",
                   font.label = list(color = "white", size = 1, vjust = 0.5),
                   rotate = TRUE,
                   ggtheme = theme_pubclean()) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  annotate("text", x = 2.5, y = 0.07, label =  c(expression(bold("OA = 0.78"))), size = 2.5) +
  labs(color = "Str. Class", title = "TLS") + 
  theme(legend.position = "none",
        legend.key.size = unit(1, units = "cm"),
        legend.spacing = unit(0.1, units = "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 8, face = "bold"),
        text = element_text(size = 6),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(vjust = 1)) +
    guides(color = guide_legend(override.aes = list(size = 4))))  # Adjust legend key size)


## DAP Plotting
(puav <- ggdotchart(df.impuav, x = "varnames", y = "IncMSE",
                    color = "strclass",
                    palette = rev(met.brewer("Austria", length(unique(df.imp$strclass)))), 
                    sorting = "descending",  
                    add = "segments",
                    add.params = list(color = "lightgray", size = 1), 
                    dot.size = 2,                                 
                    #label = round(df.imp$IncMSE,2), 
                    xlab = "",
                    ylab = "MDA",
                    font.label = list(color = "white", size = 1, vjust = 0.5),
                    rotate = TRUE,
                    ggtheme = theme_pubclean()) +
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
    annotate("text", x = 2.5, y = 0.1, label =  c(expression(bold("OA = 0.89"))), size = 2.5) +
    labs(color = "Str. Class", title = "DAP") + 
    theme(legend.position = "none",
          legend.key.size = unit(1, units = "cm"),
          legend.spacing = unit(0.1, units = "cm"),
          legend.title = element_text(size = 14, face = "bold"), 
          legend.text = element_text(size = 12, face = "bold"), 
          title = element_text(size = 8, face = "bold"),
          text = element_text(size = 6),
          axis.title = element_text(size = 7),
          axis.text = element_text(size = 7, face = "bold"), 
          axis.text.x = element_text(vjust = 1)) +
    guides(color = guide_legend(override.aes = list(size = 4))))  # Adjust legend key size)


## Field
(pfield <- ggdotchart(df.impfield, x = "varnames", y = "IncMSE",
                    color = "strclass",
                    palette = rev(met.brewer("Nattier", length(unique(df.imp$strclass)))), 
                    sorting = "descending",  
                    add = "segments",
                    add.params = list(color = "lightgray", size = 1), 
                    dot.size = 2,                                 
                    #label = round(df.imp$IncMSE,2), 
                    xlab = "",
                    ylab = "MDA",
                    font.label = list(color = "white", size = 1, vjust = 0.5),
                    rotate = TRUE,
                    ggtheme = theme_pubclean()) +
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
    annotate("text", x = 2.5, y = 0.1, label =  c(expression(bold("OA = 0.70"))), size = 2.5) +
    labs(color = "Str. Class", title = "Field") + 
    theme(legend.position = "none",
          legend.key.size = unit(1, units = "cm"),
          legend.spacing = unit(0.1, units = "cm"),
          legend.title = element_text(size = 14, face = "bold"), 
          legend.text = element_text(size = 12, face = "bold"), 
          title = element_text(size = 8, face = "bold"),
          text = element_text(size = 6),
          axis.title = element_text(size = 7),
          axis.text = element_text(size = 7, face = "bold"), 
          axis.text.x = element_text(vjust = 1)) +
    guides(color = guide_legend(override.aes = list(size = 4))))  # Adjust legend key size)


## Extract the legend
(pSCleg <- get_legend(puav))

## Merge the panels
(pc <- plot_grid(ptls, pfield, nrow = 2, labels = c("B", "C"), label_size = 8))
(pcomb <- plot_grid(puav, pc, ncol = 2, labels = c("A", " "), label_size = 8))

## Export graph as image
save_plot(pcomb, filename = paste0(dir, "/data/output/RFvarimp.png"),
          base_height = 4, base_width = 4, dpi = 600)
save_plot(pcomb, filename = paste0(dir, "/data/output/RFvarimp.pdf"),
          base_height = 4, base_width = 4, dpi = 600)

## Export legend
save_plot(pSCleg, filename = paste0(dir, "/data/output/RFvarimp_legend.png"))

###############################################################################
## Plot Accuracy Parameters
## Combine training and test datasets for the four models variables into a list
df.list <- list(df.all, df.tls, df.uav, df.field)

## Random forest classification using conditional permutation 
set.seed(290875)
rf.list <- lapply(df.list, function(group){
  model = cforest(LandUse ~., data=group, control = cforest_unbiased(ntree = 50))
  return(model)
})

## Calculate the OOB error rate
rf.oob <- sapply(df.list, function(group){
  model = predict(cforest(LandUse ~., data=group, control = cforest_unbiased(ntree = 50)), OOB = TRUE)
  oob = mean(model != group$LandUse)
  return(oob)
})

## Generate confusion table
rf.tab <- lapply(df.list, function(group){
  model = table(predict(cforest(LandUse ~., data=group, control = cforest_unbiased(ntree = 50)), OOB = TRUE), 
                group$LandUse)
  return(model)
})

## Producer accuracy 
prod.acc <- sapply(rf.tab, function(i){
  prod = diag(i / rowSums(i))
})


## User accuracy
user.acc <- sapply(rf.tab, function(i){
  prod = diag(i / colSums(i))
})

## Overall Accuracy 
overall.acc <- sapply(rf.tab, function(i) {
  sum(diag(i)) / sum(i) # Sum of diagonal entries / Total sum of the matrix
})


# Define your data
Acc.tab <- data.frame(
  Method = c("ALL", "TLS", "DAP", "Field"),
  Producer_Agroforest = prod.acc[1,],
  Producer_Forest = prod.acc[2,],
  User_Agroforest = user.acc[1,],
  User_Forest = user.acc[2,],
  Overall_Accuracy = overall.acc,
  OOB_Error_Rate = rf.oob
)

# Transform to long format
Acc.long <- Acc.tab %>%
  pivot_longer(
    cols = -Method,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Category = case_when(
      grepl("Producer", Metric) ~ "Producer Accuracy",
      grepl("User", Metric) ~ "User Accuracy",
      Metric == "Overall_Accuracy" ~ "Overall Accuracy",
      Metric == "OOB_Error_Rate" ~ "OOB Error Rate",
      TRUE ~ "Other"
    )
  )

acc.long <- Acc.long |>
  group_by(Method, Category) |>
  reframe(Value = mean(Value))


# Plot the grouped bar chart
(acc.plot <- ggplot(Acc.long, aes(x = Method, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "", y = "Percentage (%)", fill = "Metric") + 
  theme_minimal() +
  scale_fill_manual(values = met.brewer("VanGogh3", 4)) +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right") 
)


# Export plot 
require(cowplot)
plt <- plot_grid(acc.plot)
save_plot(paste0(dir, "/output/images/acc.plot.png"), acc.plot)





