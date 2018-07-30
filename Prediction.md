---
title: 'Week 4: Practical Machine Learning'
author: "J. Voltz"
date: "July 27, 2018"
output: html_document
---



## Data 
Download and read the testing and training sets.
```{r}
urlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTest  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
```
# Load the data and corresponding packages to complete the project
```{r}
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(RGtk2)
library(rattle)
library(randomForest)

Train <- read.csv (url(urlTrain))
Test  <- read.csv(url(urlTest))
```
## Cleaning and Exploring the Data
We need to remove the features that are not in the testing set. We do so by deleting the first seven features as they are non-numeric and related to the time sereies columns NA's.  The testing set is not time dependent thus the NA inputs (var, mean, and SD) are irrelevant.
```{r}
str(Train)
str(Test)

#clean data for analysis
features <- names(Test[,colSums(is.na(Test))==0]) [8:59]

#Use features in the testing data
Train <- Train [,c(features, "classe")]
Test  <- Test [,c(features, "problem_id")]

dim(Train)
dim(Test)
```

## Data Partitioning
We will divide our data into a training set and a testing set based upon the lessons from Course 8.  a 60:40 ratio should be able to estimate out the sample error of the predictor.
```{r}
#Partition the data
set.seed(129)

p_Train <- createDataPartition (Train$classe, p =0.6, list = FALSE)

trainset <- Train [p_Train, ]
testset <- Train [-p_Train, ]

dim(trainset)
dim(testset)
```

## Modeling
We will use several different models to predict the outcomes.

#Decision Tree (DT)
```{r}
DTmodel <- rpart(classe ~., data = trainset, method = "class")
rattle::fancyRpartPlot (DTmodel)
#Prediction with (DT)
set.seed(129)

outcome <- predict(DTmodel, testset, type = "class")
confusionMatrix (outcome, testset$classe)
```
#Random Forest (RF)
```{r}
set.seed(129)
RFmodel <- randomForest(classe ~., data = trainset, ntree = 1000)

#Execute prediction with RF model
outcome <- predict(RFmodel, testset, type = "Class")
confusionMatrix (outcome, testset$classe)
```
## Testing Data (csv) Prediciton Comparisons
```{r}
#DT prediciton on test data
DToutcomeTest <- predict(DTmodel, Test, type = "class")
DToutcomeTest

#RF prediction on test data
RFoutcome <- predict(RFmodel, Test, type = "class")
RFoutcome
```

## Conclusion
The Random Forest model is extremely accurate, >99%.  The Decision Tree model has a relatively high error rate (~20%).
Therefore we should utilize the RF model as test cases validated its accuracy.

#Submission file
```{r}
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(RFoutcome)
```




