### Basetable creation and splitting ###

set.seed(123)

###### Read Data ######

data <- read.csv("./../data/1_bronze/train.csv")

##### drop manual selected outliers ########
data <- data[!(data$id=="221269" | data$id=="34112" | data$id=="151343" | data$id=="219268"),]


##### split training and val #########

dt = sample(nrow(data), nrow(data)*.8)
train <- data[dt,]
val <- data[-dt,]

####### Selecting features ##########

train_X <- subset(train, select = -c(default))
train_y <- train$default

val_X <- subset(val, select = -c(default))
val_y <- val$default


#######  Write to file  ######

train <- cbind(train_X, train_y)
val <- cbind(val_X, val_y)

names(train)[names(train) == "train_y"] <- "default"
names(val)[names(val) == "val_y"] <- "default"

write.csv(train,"./../data/2_silver/features_train.csv", row.names = FALSE)
write.csv(val,"./../data/2_silver/features_val.csv", row.names = FALSE)
