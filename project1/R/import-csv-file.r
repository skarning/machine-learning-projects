mydata <- read.table("data/car.data", header = TRUE, sep = ",")

colnames(mydata)[1] = "buying"
colnames(mydata)[2] = "maint"
colnames(mydata)[3] = "doors"
colnames(mydata)[4] = "persons"
colnames(mydata)[5] = "lug_boot"
colnames(mydata)[6] = "safety"
colnames(mydata)[7] = "accept"

saveRDS(mydata, file = "data/cardata.Rds")

