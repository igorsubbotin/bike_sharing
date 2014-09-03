saveVariables <- T
seed <- 1
set.seed(seed)
source('dataloader.R')
source('sunset.R')

prepare <- function(d) {
  d$datetime <- as.POSIXlt(d$datetime,"%Y-%m-%d %H:%M:%S",tz = "GMT")
  d$hour <- as.integer(strftime(d$datetime,format="%H"))
  d$wday <- d$datetime$wday
  #d$mday <- d$datetime$mday
  d$mon <- d$datetime$mon
  #d$yday <- d$datetime$yday
  d$year <- d$datetime$year
  d$isdst <- d$datetime$isdst
  d$season <- as.factor(d$season)
  d$weather <- as.factor(d$weather) 
  #sundata <- suncalc(d$yday)
  #d$sunrise <- sundata$sunrise
  #d$sunset <- sundata$sunset
  #d <- d[,-which(names(d) %in% c("datetime","holiday","workingday"))]
  d
}

library(caret)
library(plyr)
library(Metrics)

# training
training <- loadTrain()
training <- prepare(training)
#preProcess <- preProcess(training[,c("hour","temp","humidity")],method="center","scale","pca")
#training[,c("hour","temp","humidity")] <- predict(object=preProcess,newdata=training[,c("hour","temp","humidity")])

# ml variables
cvMethod <- "cv"
trControl <- trainControl(method=cvMethod,allowParallel=TRUE,verboseIter=TRUE)
method <- "rf"
casualFormulaStr <- "casual~hour+humidity+temp+weather+season+atemp+windspeed+wday+mon+year+isdst"
casualFormula <- casual~hour+humidity+temp+weather+season+atemp+windspeed+wday+mon+year+isdst
registeredFormulaStr <- "registered~hour+humidity+temp+weather+season+atemp+windspeed+wday+mon+year+isdst"
registeredFormula <- registered~hour+humidity+temp+weather+season+atemp+windspeed+wday+mon+year+isdst

# working day
trainingWday <- subset(training,workingday==1)
modFitCasualWday <- train(casualFormula,data=trainingWday,method=method,trControl=trControl)
modFitRegisteredWday <- train(registeredFormula,data=trainingWday,method=method,trControl=trControl)
trainingWday$casualPred <- predict(modFitCasualWday,newdata=trainingWday)
trainingWday$registeredPred <- predict(modFitRegisteredWday,newdata=trainingWday)
trainingWday$countPred <- trainingWday$casualPred + trainingWday$registeredPred
rmseCasualWday <- rmse(trainingWday$casualPred,trainingWday$casual)
rmseRegisteredWday <- rmse(trainingWday$registeredPred,trainingWday$registered)
rmseCountWday <- rmse(trainingWday$countPred,trainingWday$count)
rmsleCasualWday <- rmsle(trainingWday$casualPred,trainingWday$casual)
rmsleRegisteredWday <- rmsle(trainingWday$registeredPred,trainingWday$registered)
rmsleCountWday <- rmsle(trainingWday$countPred,trainingWday$count)

# holiday
trainingHday <- subset(training,workingday==0)
modFitCasualHday <- train(casualFormula,data=trainingHday,method=method,trControl=trControl)
modFitRegisteredHday <- train(registeredFormula,data=trainingHday,method=method,trControl=trControl)
trainingHday$casualPred <- predict(modFitCasualHday,newdata=trainingHday)
trainingHday$registeredPred <- predict(modFitRegisteredHday,newdata=trainingHday)
trainingHday$countPred <- trainingHday$casualPred + trainingHday$registeredPred
rmseCasualHday <- rmse(trainingHday$casualPred,trainingHday$casual)
rmseRegisteredHday <- rmse(trainingHday$registeredPred,trainingHday$registered)
rmseCountHday <- rmse(trainingHday$countPred,trainingHday$count)
rmsleCasualHday <- rmsle(trainingHday$casualPred,trainingHday$casual)
rmsleRegisteredHday <- rmsle(trainingHday$registeredPred,trainingHday$registered)
rmsleCountHday <- rmsle(trainingHday$countPred,trainingHday$count)

# score on full training set
trainingCheck <- rbind(trainingWday, trainingHday)
trainingCheck <- arrange(trainingCheck, datetime)
rmseCasual <- rmse(trainingCheck$casualPred,trainingCheck$casual)
rmseRegistered <- rmse(trainingCheck$registeredPred,trainingCheck$registered)
rmseCount <- rmse(trainingCheck$countPred,trainingCheck$count)
rmsleCasual <- rmsle(trainingCheck$casualPred,trainingCheck$casual)
rmsleRegistered <- rmsle(trainingCheck$registeredPred,trainingCheck$registered)
rmsleCount <- rmsle(trainingCheck$countPred,trainingCheck$count)

# save variables
if (saveVariables) {
  varNameVector <- c("seed","cvMethod","method","casualFormula","registeredFormula",
                     "rmseCasualWday","rmseRegisteredWday","rmseCountWday","rmsleCasualWday","rmsleRegisteredWday","rmsleCountWday",
                     "rmseCasualHday","rmseRegisteredHday","rmseCountHday","rmsleCasualHday","rmsleRegisteredHday","rmsleCountHday",
                     "rmseCasual","rmseRegistered","rmseCount","rmsleCasual","rmsleRegistered","rmsleCount")
  varValueVector <- c(seed,cvMethod,method,casualFormulaStr,registeredFormulaStr,
                      rmseCasualWday,rmseRegisteredWday,rmseCountWday,rmsleCasualWday,rmsleRegisteredWday,rmsleCountWday,
                      rmseCasualHday,rmseRegisteredHday,rmseCountHday,rmsleCasualHday,rmsleRegisteredHday,rmsleCountHday,
                      rmseCasual,rmseRegistered,rmseCount,rmsleCasual,rmsleRegistered,rmsleCount)
  varData <- data.frame(name=varNameVector,value=varValueVector)
  varFileName <- gsub(":", "_", paste(Sys.time(),"csv",sep="."))
  varFileName <- paste("results",varFileName,sep="/")
  write.csv(varData,file=varFileName,row.names=F,col.names=T,quote=F)
}

# compare results


# testing
test <- loadTest()
testing <- prepare(test)

# working day
testingWday <- subset(testing, workingday==1)
testingWday$casual <- predict(modFitCasualWday,newdata=testingWday)
testingWday$registered <- predict(modFitRegisteredWday,newdata=testingWday)
testingWday$count <- testingWday$casual + testingWday$registered

# holiday
testingHday <- subset(testing, workingday==0)
testingHday$casual <- predict(modFitCasualHday,newdata=testingHday)
testingHday$registered <- predict(modFitRegisteredHday,newdata=testingHday)
testingHday$count <- testingHday$casual + testingHday$registered

# combine results
testingResult <- rbind(testingWday, testingHday)
testingResult <- arrange(testingResult, datetime)
testingResult[testingResult$count<0,]$count <- 0
write.csv(testingResult[,c("datetime", "count")],file="submission.csv",row.names=F,col.names=T,quote=FALSE)