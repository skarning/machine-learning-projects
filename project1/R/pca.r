library(ggfortify)
library(C50)

cardata <- readRDS(file = "data/cardata.Rds")
vars <- c("PC1", "PC2", "PC3", "PC4", "PC5")

# numerize categories

head(cardata)
cardata$maint <- unclass(cardata$maint)

cardata$doors <- unclass(cardata$doors)
cardata$persons <- unclass(cardata$persons)
cardata$lug_boot <- unclass(cardata$lug_boot)
cardata$safety <- unclass(cardata$safety)
cardata$accept <- unclass(cardata$accept)

# randomize data
set.seed(500)   #sets seed to 500 for recreation
rows <- sample(nrow(cardata))
r_cardata <- cardata[rows, ]
r_cardata <- cardata[c("doors", "maint", "lug_boot", "persons", "accept", "safety")]



# applies pca
cardata.pca <- prcomp(r_cardata, center = TRUE, scale. = TRUE )


# visualization of data
summary(cardata.pca)
autoplot(cardata.pca, data = cardata, colour = "buying", loadings = TRUE)

cardata.fit <- C5.0(cardata.pca$x[,vars], cardata$buying)

summary(cardata.fit)
