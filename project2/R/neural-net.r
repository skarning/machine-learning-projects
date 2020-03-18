library(caret)
library(plyr)
require(neuralnet)
require(ROCR)

# Import data
cardata <- readRDS(file = "data/cardata.Rds")

# Randomize data
# https://medium.com/@tyagi.sudarshini/how-to-shuffle-a-dataframe-in-r-by-rows-e7971cd7949e
set.seed(500)   #sets seed to 500 for recreation
rows <- sample(nrow(cardata))
randomCardata <- cardata[rows, ]

#https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
#Split dataset
split <- sort(sample(nrow(randomCardata), nrow(randomCardata)*.75))
train <- randomCardata[split,]
test <- randomCardata[-split,]

# One hot encoding
#https://amunategui.github.io/dummyVar-Walkthrough/
oneHotEncode <- function(input) {
    dummyfied <- dummyVars("~ .", data=input)
    dummyfiedData <- predict(dummyfied, newdata = randomCardata)
    encodedData <- data.frame(dummyfiedData)
    return(encodedData)
}

encodedTrain <- oneHotEncode(train)
encodedTest <- oneHotEncode(test)
encodedFull <- oneHotEncode(randomCardata)

# Neuralnet prediction
# Neural network fitting from: https://www.datacamp.com/community/tutorials/neural-network-models-r
neuralNetModel <- neuralnet("buying.high + buying.low + buying.med + buying.vhigh ~ .", data=encodedTrain, hidden=c(5,3), linear.output = FALSE)
result.mat <- neuralNetModel$result.matrix

accuracy <- function(encodedData, res, from, to) {
    class <- max.col(encodedData[,from:to])
    predictedClass <- max.col(res)
    accuracy <- mean(class == predictedClass)
    return(accuracy)
}

predictionTable <- function(model, encodedData) {
    return(compute(model, encodedData)$net.result)
}

# Cross validation
# https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/
crossValidate <- function(k, partition, encodedData, from, to) {
    acurracies <- NULL
    for (i in 1:k) {
        split <- sample(1:nrow(encodedData), round(partition*nrow(encodedData)))
        trainPartition <- encodedData[split, ]
        testPartition <- encodedData[-split, ]                                         #hidden is the number of hidden layers for specifiyin node use vector c(5,2) ex.
        currentModel <- neuralnet("buying.high + buying.low + buying.med + buying.vhigh ~ .", data=trainPartition, hidden=5, act.fct = "tanh", linear.output = FALSE)

        dataPrediction <- predictionTable(currentModel, testPartition)
        acurracies[i] <- accuracy(testPartition, dataPrediction, from, to)
    }
    return(mean(acurracies))
}


# Training data
trainDataPrediction <- predictionTable(neuralNetModel, encodedTrain)
trainDataAccuracy <- accuracy(encodedTrain, trainDataPrediction, 1, 4)

# Test data
testDataPrediction <- predictionTable(neuralNetModel, encodedTest)
testDataAccuracy <- accuracy(encodedTest, testDataPrediction, 1, 4)

# Cross validation
# crossValidationAccuracy <- crossValidate(10, 0.90, encodedFull, 1, 4)

print(trainDataAccuracy)
print(testDataAccuracy)
# print(crossValidationAccuracy)
