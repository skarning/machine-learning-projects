library(ggfortify)

cardata <- readRDS(file = "data/cardata.Rds")

# numerize categories
cardata$maint <- unclass(cardata$maint)
cardata$buying <- unclass(cardata$buying)
cardata$doors <- unclass(cardata$doors)
cardata$persons <- unclass(cardata$persons)
cardata$lug_boot <- unclass(cardata$lug_boot)
cardata$safety <- unclass(cardata$safety)
cardata$accept <- unclass(cardata$accept)

# randomize data
set.seed(500)   #sets seed to 500 for recreation
rows <- sample(nrow(cardata))
r_cardata <- cardata[rows, ]
head(r_cardata)


# applies pca
cardata.pca <- prcomp(r_cardata, center = TRUE, scale. = TRUE)


# visualization of data
summary(cardata.pca)
autoplot(cardata.pca,data = cardata)
