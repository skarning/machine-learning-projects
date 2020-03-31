library(h2o)

h2o.init(nthread=-1, max_mem_size="4G")
h2o.removeAll()

cardata <- h2o.importFile("data/random.data")

colnames(cardata)[1] = "buying"
colnames(cardata)[2] = "maint"
colnames(cardata)[3] = "doors"
colnames(cardata)[4] = "persons"
colnames(cardata)[5] = "lug_boot"
colnames(cardata)[6] = "safety"
colnames(cardata)[7] = "accept"

# Split the dataset into training, validation and testing
# http://docs.h2o.ai/h2o-tutorials/latest-stable/tutorials/deeplearning/index.html
partition <- h2o.splitFrame(cardata, c(0.8,0.1), seed=500)
training  <- h2o.assign(partition[[1]], "training.hex")
validation  <- h2o.assign(partition[[2]], "validation.hex")
testing   <- h2o.assign(partition[[3]], "testing.hex")

prediction <- "buying"
predictors <- setdiff(names(cardata), prediction)

# Hyperparameters
hyperParameters <- list(
    hidden=list(c(10,10,10), c(100,100)),
    input_dropout_ratio=c(0, 0.5),
    rate_annealing=c(0.01,0.04)
)

grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id="1",
    training_frame=training, 
    validation_frame=validation,
    x=predictors,
    y=prediction,
    categorical_encoding="AUTO",
    epochs=100000,
    stopping_rounds=3,
    stopping_metric="misclassification",
    stopping_tolerance=0.04,
    hyper_params=hyperParameters
)

grid
