#Installing packages, loading libraries, and setting the seed for **reproduceability** : 

library(caret)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
set.seed(12345)

#Getting and Cleaning Data
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#Load data to memory.
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

#Delete columns with all missing values
training<-training[,colSums(is.na(training))==0]
testing<-testing[,colSums(is.na(testing))==0]

#Delete variables that are irrelevant to our project: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7)
training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]

#Partitioning training dataset
#Partioning Training data set into two data sets, 75% for subTraining, 25%% for subTesting:
inTrain <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
subTraining<-training[inTrain,]
subTesting<-training[-inTrain,]
dim(subTraining)
dim(subTesting)

##The variable "classe" contains 5 levels: A, B, C, D and E. A plot of the outcome variable will allow us to see the frequency of each levels in the subTraining data set and # compare one another.
freq_table <- table(subTraining$classe)
barplot(freq_table,col = 'green')

#Prediction Model 1 : Decision Tree
model_dt<-rpart(classe~.,data=subTraining,method = "class")

#To view the decision tree
fancyRpartPlot(model_dt,cex=0.4)

#Predicting: 

prediction_cv_dt<-predict(model_dt,subTesting,type = "class")

#Test result using ConfusionMatrix:

confusionMatrix(prediction_cv_dt, as.factor(subTesting$classe))

#Prediction Model 2: Random Forest 

model_rf<-randomForest(classe~.,data=subTesting,method="class")

#Predicting;

prediction_cv_rf<-predict(model_rf,subTesting,type="class")

# Test result using ConfusionMatrix:

confusionMatrix(prediction_cv_rf,as.factor(subTesting$classe))

#Random Forest algorithm performed better than Decision Trees. 
#Accuracy for Random Forest model was 1 (95% CI: (0.9992, 1)) compared to Decision Tree model with  0.6407 (95% CI: (0.6271, 0.6541)). 
#The Random Forests model is choosen. The expected out-of-sample error is estimated at 0.005, or 0.5%.

#Final outcome based on Prediction Model RF i.e. Random Forest applied on Testing Dataset : 
  
finalPrediction<-predict(model2,testing,type="class")
finalPrediction


