train <- read.csv('train.csv',header=TRUE)

prepare <- function(d) {
  d$datetime <- as.POSIXlt(d$datetime)
  #d$hour <- as.factor(strftime(d$datetime, format="%H"))
  #d$season <- as.factor(d$season)
  #d$weather <- as.factor(d$weather)  
  d <- d[,-which(names(d) %in% c("datetime","holiday","workingday"))]
  preProcess <- 
    preProcess(d[,c("temp", "atemp", "humidity", "windspeed")], method = "center", "scale", "knnImpute")
  d[,c("temp", "atemp", "humidity", "windspeed")] <- 
    predict(object = preProcess, newdata = d[,c("temp", "atemp", "humidity", "windspeed")])
  d
}

library(caret)
library(corrplot)
set.seed(1)

# analysis
c <- cor(train[,c("season","holiday","workingday","weather","temp","atemp","humidity","windspeed","casual","registered","count")])
#as.matrix(c)
corrplot(c, method="circle")

# training
#training <- train[,-which(names(train) %in% c("casual","registered"))]
training <- train
training <- prepare(training)
modFit <- train(casual+registered~.,data=training,method="rf",
                trControl=trainControl(method="repeatedcv",number=10,repeats=10,allowParallel=TRUE,verboseIter=TRUE))

# testing
dataTest <- read.csv('test.csv',header=TRUE)
testing <- prepare(dataTest)
dataTest$count <- as.integer(predict(modFit,newdata=testing))
write.csv(dataTest[,c("datetime", "count")],file="submission.csv",row.names=F,col.names=T,quote=FALSE)