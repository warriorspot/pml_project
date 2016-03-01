## ----fetch files, echo=FALSE---------------------------------------------

if(file.exists("pml-training.csv") == FALSE) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                  destfile="pml-training.csv")
}

if(file.exists("pml-testing.csv") == FALSE) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                  destfile="pml-testing.csv")

}


## ----create data partitions, echo=TRUE-----------------------------------

library(caret)

set.seed(1234)
data <- read.csv("./pml-training.csv", stringsAsFactors=FALSE)

inTrain <- createDataPartition(y=data$classe, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

# Out of sample error rate
outOfSampleError <- function(m) {
    pTesting <- predict(m, newdata=testing[,-160])
    c <- data.frame(actual=testing$classe, predicted=pTesting)
    errorRate <- nrow(c[c$actual != c$predicted,])/nrow(c)
}


## ----clean data, echo=TRUE-----------------------------------------------
#Get rid of the first four columns, which dont contain measurement data
tclean <- subset(training, select=seq(5, ncol(training)))

# Consider only numeric predictors with no NA values
classe <- tclean$classe
tclean <- tclean[, sapply(tclean, is.numeric)]
tclean <- tclean[, colSums(is.na(tclean)) == 0]

preMethod <- c("center", "scale", "pca")
preObj <- preProcess(tclean, method=preMethod)
print(preObj)

tclean$classe <- as.factor(classe)
## ----build the model, cache=TRUE, echo=TRUE------------------------------

tControl <- trainControl(method="cv", number=3)

mLda <- train(classe ~ ., method="lda", preProcess=preMethod,
           trainControl=tControl, data=tclean)
print(mLda)
errorRate <- outOfSampleError(mLda)
print(errorRate)

#mRf <- train(classe ~ ., method="rf", preProcess=c("center", "scale", "pca"),
#           trainControl=tControl, data=tclean)
#print(mRf)
#errorRate <- outOfSampleError(mRf)
#print(errorRate)

## ----do final predictions, echo=TRUE-------------------------------------
#Try predictions on testing data
#testData <- read.csv("./pml-testing.csv", stringsAsFactors=FALSE)
#pFinal <- predict(mRf, newdata=testData)
#print(data.frame(pFinal))



