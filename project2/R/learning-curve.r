library(caret)
library(neuralnet)
library(ggplot2)


# softmax activation function
# https://en.wikipedia.org/wiki/Softmax_function#Neural_networks
mySoftMax <- function(value) exp(value)/sum(exp(value))


learningCurve <- function(input, res) {
    jump <- floor(nrow(input)/res)
    endIndex <- 0

    results <- NULL
    trainingResults <- NULL
    endIndexes <- NULL
    for (i in 1:res) {
        # limit the data at the end-index
        endIndex <- endIndex + jump
        limitedData <- input[0:endIndex, ]
        # https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
        # Split dataset
        split <- sort(sample(nrow(limitedData), nrow(limitedData)*.80))
        train <- limitedData[split,]
        test <- limitedData[-split,]

        # Neuralnet prediction
        # Neural network fitting from: https://www.datacamp.com/community/tutorials/neural-network-models-r
        neuralNetModel <- neuralnet("buying.high + buying.low + buying.med + buying.vhigh ~ .", data=train, hidden=4, act.fct = "logistic", linear.output = FALSE)

        # Accuracy on training set
        trainingDataPrediction <- neuralNetModel$net.result[[1]]
        trainingAccuracy <- accuracy(train, trainingDataPrediction, 1, 4)
        trainingResults[i] <- trainingAccuracy

        # Accuracy on validation set
        dataPrediction <- predictionTable(neuralNetModel, test)
        accuracy <- accuracy(test, dataPrediction, 1, 4)
        results[i] <- accuracy
        # Size of test data, used for y-axis
        endIndexes[i] <- endIndex
    }
    return(data.frame(validationAccuracy = results, trainingAccuracy = trainingResults, experience = endIndexes))
}


# One hot encoding
# https://amunategui.github.io/dummyVar-Walkthrough/
oneHotEncode <- function(input) {
    dummyfied <- dummyVars("~ .", data=input)
    dummyfiedData <- predict(dummyfied, newdata = randomCardata)
    encodedData <- data.frame(dummyfiedData)
    return(encodedData)
}

accuracy <- function(encodedData, res, from, to) {
    class <- max.col(encodedData[,from:to])
    predictedClass <- max.col(res)
    accuracy <- mean(class == predictedClass)
    return(accuracy)
}

predictionTable <- function(model, encodedData) {
    return(compute(model, encodedData)$net.result)
}

# Import data
cardata <- readRDS(file = "data/cardata.Rds")

# Randomize data
# https://medium.com/@tyagi.sudarshini/how-to-shuffle-a-dataframe-in-r-by-rows-e7971cd7949e
set.seed(500)   #sets seed to 500 for recreation
rows <- sample(nrow(cardata))
randomCardata <- cardata[rows, ]
encodedRandomCardata <- oneHotEncode(randomCardata)


results <- learningCurve(encodedRandomCardata, 50)

print(results)

# Plot the learning curve
ggplot(data=results, mapping=aes(x=experience)) + geom_line(aes(y = validationAccuracy, color = "validation")) + geom_line(aes(y = trainingAccuracy, color = "training")) + scale_color_manual(name = "Sets", values = c("validation" = "blue", "training" = "red"))
