# Practical Machine Learning - Prediction Assignment Writeup

This document describe the analysis done for the prediction assignment of the practical machine learning course.

The first part is the declaration of the package which will be used. Note : to be reproductible, I also set the seed value.

```{r}
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
set.seed(12345)
```
```{r}
## Warning: package 'caret' was built under R version 3.3.3
## Loading required package: lattice
## Warning: package 'lattice' was built under R version 3.3.3
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 3.3.3
## Warning: package 'randomForest' was built under R version 3.3.3
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## The following object is masked from 'package:ggplot2':
## 
##     margin
## Warning: package 'rpart.plot' was built under R version 3.3.3
## Warning: package 'rattle' was built under R version 3.3.3
## Rattle: A free graphical interface for data mining with R.
## Version 4.1.0 Copyright (c) 2006-2015 Togaware Pty Ltd.
## Type 'rattle()' to shake, rattle, and roll your data
```
The first step is to load the csv file data to dataframe. After that perform basic exploratory analysis and clean the data. 
Two important things to be taken care of are : 1) Some numeric data have been imported as factor because of the presence of some characters ("#DIV/0!") 2) Some columns have a really low completion rate (a lot of missing data)

To manage the first issue we need to import data ignoring "#DIV/0!" values :
```{r}
training<-read.csv("training.csv",na.strings = c("NA","#DIV/0!",""))
testing<-read.csv("testing.csv",na.strings = c("NA","#DIV/0!",""))
```
To manage the second issue we will select as feature only the column with a 100% completion rate:
```{r}
training<-training[,colSums(is.na(training))==0]
testing<-testing[,colSums(is.na(testing))==0]
```
Also removing irrelevant variables : 

```{r}
training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]
```
We have now a dataframe "features which contains all the workable features. So the first step is to split the training dataset in two part : the first for sub-training and the second for sub-testing.

```{r}
inTrain <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
subTraining<-training[inTrain,]
subTesting<-training[-inTrain,]
```
After partitioning the data we will now apply prediction models: Decision Tree and Random Forest. And will deicide which performed better using Confusion Matrix:

```{r}
model1<-rpart(classe~.,data=subTraining,method = "class")
fancyRpartPlot(model1)
subTesting <- subTesting[complete.cases(subTesting),]
prediction1<-predict(model1,subTesting,type = "class",na.action = na.pass)
confusionMatrix(prediction1, subTesting$classe)
```
```{r}
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1260  156   33   40   23
##          B   52  555   73   52   52
##          C   24  136  575   83   95
##          D   40   33  150  513   89
##          E   19   69   24  116  642
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7229          
##                  95% CI : (0.7101, 0.7354)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6486          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9032   0.5848   0.6725   0.6381   0.7125
## Specificity            0.9282   0.9421   0.9165   0.9239   0.9430
## Pos Pred Value         0.8333   0.7079   0.6298   0.6218   0.7379
## Neg Pred Value         0.9602   0.9044   0.9298   0.9287   0.9358
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2569   0.1132   0.1173   0.1046   0.1309
## Detection Prevalence   0.3083   0.1599   0.1862   0.1682   0.1774
## Balanced Accuracy      0.9157   0.7635   0.7945   0.7810   0.8278
```
```{r}
model2<-randomForest(classe~.,data=subTesting,method="class")
prediction2<-predict(model2,subTesting,type="class")
confusionMatrix(prediction2,subTesting$classe)
```
```{r}
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    0    0    0    0
##          B    0  949    0    0    0
##          C    0    0  855    0    0
##          D    0    0    0  804    0
##          E    0    0    0    0  901
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9992, 1)
##     No Information Rate : 0.2845     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```
From the confusion matrix, Random Forest algorithm performed better than Decision Trees. Accuracy for Random Forest model was 1 (95% CI: (0.9992, 1)) compared to Decision Tree model with  0.6407 (95% CI: (0.6271, 0.6541)). The Random Forests model is choosen. The expected out-of-sample error is estimated at 0.005, or 0.5%.





