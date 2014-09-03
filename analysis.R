library(corrplot)
library(caret)

source('dataloader.R')
train <- loadTrain()
train$datetime <- as.POSIXlt(train$datetime)
train$hour <- as.integer(strftime(train$datetime, format="%H"))
train$wday <- train$datetime$wday
train$mday <- train$datetime$mday
train$mon <- train$datetime$mon
train$yday <- train$datetime$yday
train$year <- train$datetime$year
train$isdst <- train$datetime$isdst
#train$season <- as.factor(train$season)

c <- cor(train[,c("hour","season","holiday","workingday","weather","temp","atemp","wday",
                  "humidity","windspeed","casual","registered","count")])
corrplot(c, method="number")
trellis.par.set(theme = col.whitebg(), warn = FALSE)
featurePlot(x=train$temp,y=train$casual,plot="scatter",type = c("p", "smooth"))
featurePlot(train$hour,train$casual,plot="scatter",type = c("p", "smooth"))
featurePlot(train$humidity,train$casual,plot="scatter",type = c("p", "smooth"))

# working days
trainWday <- subset(train, workingday==1)
c <- cor(trainWday[,c("hour","season","weather","temp","atemp","humidity","wday","windspeed","casual","registered","count")])
corrplot(c, method="number")
trainHoliday <- subset(train, workingday==0)
c <- cor(trainHoliday[,c("hour","season","weather","temp","atemp","humidity","wday","windspeed","casual","registered","count")])
corrplot(c, method="number")

dim(trainWday[trainWday$holiday==1,])