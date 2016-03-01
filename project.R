
library(caret)

set.seed(1234)

data <- read.csv("./Documents/pml-training.csv", stringsAsFactors=FALSE)

inTrain <- with(data, createDataPartition(y=classe, p=0.7, list=FALSE))
training <- data[inTrain,]
testing <- data[-inTrain,]

# Consider only numeric predictors
classe <- training$classe
tclean <- training[, sapply(training, is.numeric)] 
tclean <- tclean[, colSums(is.na(tclean)) == 0] 
tclean$classe <- classe

#Train a random forest, preprocessing with PCA
m <- train(classe ~ ., method="rf", preProcess=c("pca"), data=tclean)

