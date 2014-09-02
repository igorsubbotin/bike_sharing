set.seed(1)
source('dataloader.R')
source('metrics.R')

prepare <- function(d) {
  d$datetime <- as.POSIXlt(d$datetime, "%Y-%m-%d %H:%M:%S", tz = "GMT")
  d$hour <- as.integer(strftime(d$datetime, format="%H"))
  d$season <- as.factor(d$season)
  #d$weather <- as.factor(d$weather)  
  #d <- d[,-which(names(d) %in% c("datetime","holiday","workingday"))]
  #preProcess <- 
  #  preProcess(d[,c("hour","season","weather","temp","atemp","humidity","windspeed")],method="center","scale")
  #d[,c("hour","season","weather","temp","atemp","humidity","windspeed")] <- 
  #  predict(object = preProcess, newdata = d[,c("hour","season","weather","temp","atemp","humidity","windspeed")])
  d
}

library(caret)

# training
#training <- train[,-which(names(train) %in% c("casual","registered"))]
training <- loadTrain()
training <- prepare(training)
modFitCasual <- train(casual~hour+workingday+humidity+temp+weather,data=training,method="rf",
                trControl=trainControl(method="repeatedcv",number=10,repeats=3,allowParallel=TRUE,verboseIter=TRUE))
varImp(modFitCasual)
modFitRegistered <- train(registered~hour+workingday+humidity+temp+weather,data=training,method="rf",
                          trControl=trainControl(method="repeatedcv",number=10,repeats=3,allowParallel=TRUE,verboseIter=TRUE))
#varImp(modFitRegistered)
training$casualPred <- predict(modFitCasual,newdata=training)
training[training$casualPred<0,]$casualPred <- 0
training$registeredPred <- predict(modFitRegistered,newdata=training)
training[training$registeredPred<0,]$registeredPred <- 0
training$countPred <- training$casualPred + training$registeredPred
training[training$countPred<0,]$countPred <- 0

rmseCasual <- rmse(training$casualPred,training$casual)
rmseRegistered <- rmse(training$registeredPred,training$registered)
rmseCount <- rmse(training$countPred,training$count)
rmsleCasual <- rmsle(training$casualPred,training$casual)
rmsleRegistered <- rmsle(training$registeredPred,training$registered)
rmsleCount <- rmsle(training$countPred,training$count)

# testing
test <- loadTest()
#test$datetime <- as.character(test$datetime)
#test$datetime <- as.POSIXlt(test$datetime, "%Y-%m-%d %H:%M:%S", tz = "GMT")
testing <- prepare(test)
test$casual <- predict(modFitCasual,newdata=testing)
test[test$casual<0,]$casual <- 0
test$registered <- predict(modFitRegistered,newdata=testing)
test[test$registered<0,]$registered <- 0
test$count <- test$casual + test$registered
test[test$count<0,]$count <- 0
write.csv(test[,c("datetime", "count")],file="submission.csv",row.names=F,col.names=T,quote=FALSE)