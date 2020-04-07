cardata <- read.table("data/car.data", header=FALSE, sep=",")

set.seed(500)   #sets seed to 500 for recreation
rows <- sample(nrow(cardata))
cardata <- cardata[rows, ]

write.csv(cardata,"data/random.data", row.names=FALSE)
