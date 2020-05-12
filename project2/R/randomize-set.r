cardata <- read.table("data/car.data", header=FALSE, sep=",")

set.seed(500)   #sets seed to 500 for recreation
rows <- sample(nrow(cardata))
cardata <- cardata[rows, ]

levels(cardata$V3) <- c(levels(cardata$V3), "5")
cardata$V3[cardata$V3 == '5more'] <- '5'

levels(cardata$V4) <- c(levels(cardata$V4), "5")
cardata$V4[cardata$V4 == 'more'] <- '5'

print(cardata$V4[3])

head(cardata)

write.csv(cardata,"data/random.data", row.names=FALSE)
