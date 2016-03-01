

if(file.exists("pml-training.csv") == FALSE) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                  destfile="pml-training.csv")
}

if(file.exists("pml-testing.csv") == FALSE) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                  destfile="pml-testing.csv")

}

library(caret)

set.seed(1234)

data <- read.csv("./pml-training.csv", stringsAsFactors=FALSE)

inTrain <- createDataPartition(y=data$classe, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

# Prepare the data for prediction

#Get rid of the first four columns, which dont contain measurement data
tclean <- subset(training, select=seq(5, ncol(training)))

# Consider only numeric predictors with no NA values
classe <- tclean$classe
tclean <- tclean[, sapply(tclean, is.numeric)]
tclean <- tclean[, colSums(is.na(tclean)) == 0]
tclean$classe <- as.factor(classe)


#Set up k-fold cross validation
tControl <- trainControl(method="cv", number=3)

#Train a naive Bayes model, preprocessing with PCA, using K-Fold cross validation
m <- train(classe ~ ., method="rf", preProcess=c("center", "scale", "pca"),
           trainControl=tControl, data=tclean)

#Verify error rate on test set
pTesting <- predict(m, newdata=testing[,-160])
c <- data.frame(actual=testing$classe, predicted=pTesting)
errorRate <- nrow(c[c$actual != c$predicted,])/nrow(c)

#Try predictions on testing data
testData <- read.csv("./pml-testing.csv", stringsAsFactors=FALSE)
pFinal <- predict(m, newdata=testData)
