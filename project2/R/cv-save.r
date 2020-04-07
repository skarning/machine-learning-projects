crossValidate <- function(k, partition, encodedData, from, to) {
    acurracies <- NULL
    for (i in 1:k) {
        split <- sample(1:nrow(encodedData), round(partition*nrow(encodedData)))
        trainPartition <- encodedData[split, ]
        testPartition <- encodedData[-split, ]
        currentModel <- neuralnet("buying.high + buying.low + buying.med + buying.vhigh ~ .", data=trainPartition, stepmax = 1e+06, hidden=3, act.fct = "logistic", linear.output = FALSE)

#        dataPrediction <- predictionTable(currentModel, testPartition)
 #       acurracies[i] <- accuracy(testPartition, dataPrediction, from, to)
    }
    return(0.5)
}
