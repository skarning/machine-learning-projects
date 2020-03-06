library(caret)
library(plyr)
require(neuralnet)

#Import data
cardata <- readRDS(file = "data/cardata.Rds")

# randomize data
set.seed(500)   #sets seed to 500 for recreation
rows <- sample(nrow(cardata))
r_cardata <- cardata[rows, ]


#Split dataset
dt <- sort(sample(nrow(r_cardata), nrow(r_cardata)*.75))
train <- r_cardata[dt,]
test <- r_cardata[-dt,]

head(train)

# One hot encoding
one_hot <- dummyVars("~ .", data=train)
one_hot_mat <- predict(one_hot, newdata = r_cardata)
one_hot_data <- data.frame(one_hot_mat)

one_hot <- dummyVars("~ .", data=test)
one_hot_mat <- predict(one_hot, r_cardata)
one_hot_test <- data.frame(one_hot_mat)

head(one_hot_test)


# Find the number of missing values
missing_values <- sum(is.na(train))


# Neuralnet prediction
# Neural network fitting from: https://www.datacamp.com/community/tutorials/neural-network-models-r
nn=neuralnet("buying.high + buying.low + buying.med + buying.vhigh ~ .", data=one_hot_data, hidden=3, act.fct="logistic", linear.output = FALSE)

plot(nn)

result.mat <- nn$result.matrix


result <- compute(nn, one_hot_data)
ex_res <- result$net.result

# Calculate the accuracy
act_dat <- max.col(one_hot_data[,1:4])
res_ex_2 <- max.col(ex_res)
mean(res_ex_2 == act_dat)
