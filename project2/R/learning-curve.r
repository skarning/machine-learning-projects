library(caret)
require(neuralnet)

learningCurve <- function(input, res) {
    jump <- floor(nrow(input)/res)
    endIndex <- 0

    results <- NULL
    endIndexes <- NULL
    for (i in 1:res) {
        # limit the data at the end-index
        endIndex <- endIndex + jump
        limitedData <- input[0:endIndex, ]
        # https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
        # Split dataset
        split <- sort(sample(nrow(limitedData), nrow(limitedData)*.75))
        train <- limitedData[split,]
        test <- limitedData[-split,]

        # Neuralnet prediction
        # Neural network fitting from: https://www.datacamp.com/community/tutorials/neural-network-models-r
        neuralNetModel <- neuralnet("buying.high + buying.low + buying.med + buying.vhigh ~ .", data=train, hidden=3, linear.output = FALSE)
        result.mat <- neuralNetModel$result.matrix

        dataPrediction <- predictionTable(neuralNetModel, test)
        accuracy <- accuracy(test, dataPrediction, 1, 4)
        results[i] <- accuracy
        endIndexes[i] <- endIndex
    }
    return(data.frame(accuracy = results, experience = endIndexes))
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


results <- learningCurve(encodedRandomCardata, 10)

ggplot(data=results, mapping=aes(x=experience, y=accuracy)) + geom_line() + labs(title="Learning Curve") + theme_classic()
