library(corrplot)
library(caret)



# Code from Max Kuhn Applied Predicitve Modeling
cardata <- readRDS(file = "data/cardata.Rds")

cardata$maint <- unclass(cardata$maint)
cardata$doors <- unclass(cardata$doors)
cardata$persons <- unclass(cardata$persons)
cardata$lug_boot <- unclass(cardata$lug_boot)
cardata$safety <- unclass(cardata$safety)
cardata$accept <- unclass(cardata$accept)

correlations <- cor(cardata[c("doors", "persons", "lug_boot", "accept", "safety", "maint")])
dim(correlations)

# Visualization
head(correlations)

corrplot(correlations, order = "hclust")

# Removing highly correlated predictors
highcorr <- findCorrelation(correlations, cutoff = .7)

filtered_seg_data <- cardata[, -highcorr]

head(filtered_seg_data)
