# Include needed libraries

library (rpart)
library(ggplot2)
library (caret)
library(rpart.plot) 



download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "Data/pml-training.csv")
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "Data/pml-testing.csv")
setwd("C:/Sherbeeny/Workspace/R/DataScience-Coursera/ML-Project")
# Load the data from downloaded CSV files
pml_train <- read.csv('Data/pml-training.csv', na.strings=c("NA","#DIV/0!", ""))
pml_test <- read.csv('Data/pml-testing.csv' , na.strings=c("NA","#DIV/0!", ""))
head(pml_train)
dim(pml_train)
dim(pml_test)
names(pml_train)


# Data Munging

#remove features that don't have many missing values but have one unique value 
#(i.e. zero variance predictors) or have few unique values relative to the number of samples and the 
#ratio of frequency of the most common value to the frequency of second most common value is large.
bad_Columns <- nearZeroVar(pml_train, saveMetrics = TRUE)
bad_Columns$nzv
pml_train <- pml_train[, bad_Columns$nzv==FALSE]
#Same for Testing dataset
pml_test <- pml_test[, bad_Columns$nzv==FALSE]
dim(pml_train)


#Similarly the user and time information should not have any effect on 
#whether barbell lifts are performed correctly or not.

unnecessary_cols <- c("user_name", "raw_timestamp_part_1",
                    "raw_timestamp_part_2", "cvtd_timestamp")
for (col in unnecessary_cols) {
  pml_train[, col] <- NULL
  pml_test[, col] <- NULL
}
dim(pml_train)


# Delete columns with all missing values
pml_train<-pml_train[,colSums(is.na(pml_train)) == 0]
pml_test <-pml_test[,colSums(is.na(pml_test)) == 0]
pml_train$X <- NULL
pml_test$X <- NULL
dim(pml_train)

#Build the model

set.seed(234334)
inTrain <- createDataPartition(y=pml_train$classe, p=0.75, list=FALSE)
training <- pml_train[inTrain, ] 
validation <- pml_train[-inTrain, ]
dim(training)
dim(validation)
dim(pml_test)


# Decesion tree
modelfit1 <- rpart(classe ~ ., data=training, method="class")
valid_predict1 <- predict(modelfit1, validation, type = "class")

# Test results on our subTesting data set:
confusionMatrix(valid_predict1, validation$classe)

# Test another model
modelcontrol <- trainControl(method="cv", number=3, verboseIter=FALSE)
modelfit2 <- train(classe ~ ., data=training, method="rf",
                          trControl=modelcontrol)
modelfit2$finalModel
# prediction on Test dataset
valid_predict2 <- predict(modelfit2, newdata=validation)
confusionMatrix(valid_predict2, validation$classe)

# fine tuning 
ImportantVar <- train(classe ~ ., data = training, method = "rf")
ImportantVarObj <- varImp(ImportantVar)
# Top 40 plot
plot(ImportantVarObj, main = "Importance of Top 40 Variables", top = 15)



predictfinal <- predict(model2, testingset, type="class")
predictfinal
