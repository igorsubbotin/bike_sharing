library(corrplot)
source('dataloader.R')
train <- loadTrain()
train$datetime <- as.POSIXlt(train$datetime)
train$hour <- as.integer(strftime(train$datetime, format="%H"))

c <- cor(train[,c("hour","season","holiday","workingday","weather","temp","atemp",
                  "humidity","windspeed","casual","registered","count")])
#as.matrix(c)
corrplot(c, method="number")
