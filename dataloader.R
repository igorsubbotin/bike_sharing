loadTrain <- function() {
  train <- read.csv('train.csv',header=TRUE)
  train
}
loadTest <- function() {
  test <- read.csv('test.csv',header=TRUE)
  test
}

