

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

# Prepare the data for prediction

#Get rid of the first four columns, which dont contain measurement data
tclean <- subset(data, select=seq(5, ncol(data)))

# Consider only numeric predictors with no NA values
classe <- data$classe
tclean <- data[, sapply(data, is.numeric)] 
tclean <- tclean[, colSums(is.na(tclean)) == 0] 
tclean$classe <- classe

#Set up k-fold cross validation
tControl <- trainControl(method="cv", number=10)

#Train a naive Bayes model, preprocessing with PCA, using K-Fold cross validation
m <- train(classe ~ ., method="nb", preProcess=c("pca"), trainControl=tControl, data=tclean)


