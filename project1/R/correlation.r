library(corrplot)
library(caret)

cardata <- readRDS(file = "data/cardata.Rds")

cardata$maint <- unclass(cardata$maint)
cardata$buying <- unclass(cardata$buying)
cardata$doors <- unclass(cardata$doors)
cardata$persons <- unclass(cardata$persons)
cardata$lug_boot <- unclass(cardata$lug_boot)
cardata$safety <- unclass(cardata$safety)
cardata$accept <- unclass(cardata$accept)

correlations <- cor(cardata)
dim(correlations)

# Visualization
correlations[1:7, 1:7]

corrplot(correlations, order = "hclust")

# Removing highly correlated predictors
highcorr <- findCorrelation(correlations, cutoff = .7)

filtered_seg_data <- cardata[, -highcorr]

head(filtered_seg_data)
