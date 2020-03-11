library(caret)
library(plyr)
require(neuralnet)

#Import data
cardata <- readRDS(file = "data/cardata.Rds")

#https://medium.com/@tyagi.sudarshini/how-to-shuffle-a-dataframe-in-r-by-rows-e7971cd7949e
# randomize data
set.seed(500)   #sets seed to 500 for recreation
rows <- sample(nrow(cardata))
r_cardata <- cardata[rows, ]


#https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
#Split dataset
dt <- sort(sample(nrow(r_cardata), nrow(r_cardata)*.75))
train <- r_cardata[dt,]
test <- r_cardata[-dt,]


#https://amunategui.github.io/dummyVar-Walkthrough/
# One hot encoding
one_hot <- dummyVars("~ .", data=train)
one_hot_mat <- predict(one_hot, newdata = r_cardata)
one_hot_data <- data.frame(one_hot_mat)

one_hot <- dummyVars("~ .", data=test)
one_hot_mat <- predict(one_hot, r_cardata)
one_hot_test <- data.frame(one_hot_mat)

# One hot encoding the entire dataset for cross validation
one_hot_cross <- dummyVars("~ .", data=r_cardata)
one_hot_mat_cross <- predict(one_hot_cross, r_cardata)
one_hot_cross_var <- data.frame(one_hot_mat_cross)

# https://discuss.analyticsvidhya.com/t/how-to-count-the-missing-value-in-r/2949/9
# Find the number of missing values
missing_values <- sum(is.na(train))


# Neuralnet prediction
# Neural network fitting from: https://www.datacamp.com/community/tutorials/neural-network-models-r
nn=neuralnet("buying.high + buying.low + buying.med + buying.vhigh ~ .", data=one_hot_data, hidden=3, act.fct="logistic", linear.output = FALSE)

plot(nn)

result.mat <- nn$result.matrix


# Training data
result <- compute(nn, one_hot_data)
ex_res <- result$net.result

# Calculate the accuracy
act_dat <- max.col(one_hot_data[,1:4])
res_ex_2 <- max.col(ex_res)
mean(res_ex_2 == act_dat)


# Test data
test_result <- compute(nn, one_hot_test)
test_net_result <- result$net.result

# Calculate the accuracy
act_test_dat <- max.col(one_hot_test[,1:4])
res_test_dat <- max.col(test_net_result)
mean(act_test_dat == res_test_dat)

# Cross validation
# Code taken from:
# https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/

k <- 10

outs <- NULL

proportion <- 0.95

for (i in 1:k) {
    index <- sample(1:nrow(one_hot_cross_var), round(proportion*nrow(one_hot_cross_var)))

    train_cv <- one_hot_cross_var[index, ]
    test_cv <- one_hot_cross_var[-index, ]
    nn=neuralnet("buying.high + buying.low + buying.med + buying.vhigh ~ .", data=one_hot_cross_var, hidden=3, act.fct="logistic", linear.output = FALSE)
    
    # Compute predictions
    pr.nn <- compute(nn, test_cv)
    # Extract results
    pr.nn_ <- pr.nn$net.result
    # Accuracy (test set)
    original_values <- max.col(test_cv[, 1:4])
    pr.nn_2 <- max.col(pr.nn_)
    outs[i] <- mean(pr.nn_2 == original_values)
}

mean(outs)

