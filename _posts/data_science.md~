---
title: "Practical Machine Learning PA"
author: "Jakub Winter aka Rawrqs on GitHub"
date: "Friday, August 22, 2014"
output: html_document
---
##Summary

In this project, the goal was to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.
The goal of your project was to predict the manner in which they did the exercise. This is the "classe" variable in the training set.
For this purpose i have used the random forest technique while using two approaches. After using the first approach for predicting test cases from coursera i have  classified correctly all of the cases and the accuracy for my testing set was over 99%.

##Cleaning the data
###Data selection

The data contains 160 columns and and 19622 rows.Some of the columns and rows were summaries of other rows and columns, thus i have removed those. I found out that it was enough to remove all rows with the new_windows argument set to "no". I have also manually choosen variables of interest (which i searched manually and passed to variable choose) which are presented below. After doing the abovementioned there were no more NA's in data.


```r
data <- read.csv("C:/Users/Kuba/Desktop/pml-training.csv", header=TRUE, sep=",", na.strings= c("NA", ""))
choose <- c(2, 8:11, 37:49, 60:68, 84:86, 102, 113:124, 140, 151:159, 160)
#removing all rows that are summaries
data.2 <- data[data$new_window == "no",]
#creating only data with parameters of interest
data.3 <- data[, choose]
colnames(data.3)
```

```
 [1] "user_name"            "roll_belt"            "pitch_belt"          
 [4] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
 [7] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
[10] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
[13] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
[16] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
[19] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
[22] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
[25] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
[28] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
[31] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
[34] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
[37] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
[40] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
[43] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
[46] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
[49] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
[52] "magnet_forearm_y"     "magnet_forearm_z"     "classe"              
```

After doing the abovementioned there were no more NA's in data.


```r
any(is.na(data.3)==TRUE)
```

```
[1] FALSE
```

##Random forest
I have choosen to use random forest for my modelling, therefore there was no need for creating part of data for cross validation. I decided to split the data into training and testing set by the ratio of 0.7 training set, and 0.3 test set using default sampling options from the createDataPartition in the caret package. I expect the out of sample error to be rather small, as the rf does not overfitt and has some nice algorith foraveraging the predictors.


```r
library(caret)

#?createDataPartition
inTrain <- createDataPartition(data.3$classe, p = 0.7, list = FALSE)
training = data.3[inTrain,]
testing = data.3[-inTrain,]
```

I have decided to do two approaches:

* First - to fit a model for every participant with a dependend variable name using all possible information to learn the model
* Secound - to fit a model for every participant separately, which would catch thier specific characteristics of human body 

Later on I will use the only First model, but I will also shortly compare results from both approaches. 


```r
library(doParallel)
registerDoParallel(4)
library(e1071)
fit <- train(classe ~., data = training, method = "rf")
```

```
## Warning: closing unused connection 6 (<-Rawr-PC:11526)
## Warning: closing unused connection 5 (<-Rawr-PC:11526)
## Warning: closing unused connection 4 (<-Rawr-PC:11526)
## Warning: closing unused connection 3 (<-Rawr-PC:11526)
```

```r
fit.adelmo <- train(classe ~., data = subset(training, user_name == "adelmo")[,-1], method = "rf")
fit.carlitos <- train(classe ~., data = subset(training, user_name == "carlitos")[,-1], method = "rf")
fit.charles <- train(classe ~., data = subset(training, user_name == "charles")[,-1], method = "rf")
fit.eurico <- train(classe ~., data = subset(training, user_name == "eurico")[,-1], method = "rf")
fit.jeremy <- train(classe ~., data = subset(training, user_name == "jeremy")[,-1], method = "rf")
fit.pedro <- train(classe ~., data = subset(training, user_name == "pedro")[,-1], method = "rf")
```

## Variable importance

The plot below presents normalized variable importance for the first apprach. We can see that roll_belt was the most important variable while classyfing to the group. We can see that user_name may not be found in the top 20 variables


```r
plot(varImp(fit), top = 20)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

## Confusion Matrix for approach 1


```r
confusionMatrix(predict(fit,newdata=testing[,-ncol(testing)]),testing$classe)
```

```
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1673    7    0    0    0
         B    0 1130    4    0    0
         C    0    2 1015    5    0
         D    0    0    7  959    6
         E    1    0    0    0 1076

Overall Statistics
                                        
               Accuracy : 0.995         
                 95% CI : (0.992, 0.996)
    No Information Rate : 0.284         
    P-Value [Acc > NIR] : <2e-16        
                                        
                  Kappa : 0.993         
 Mcnemar's Test P-Value : NA            

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity             0.999    0.992    0.989    0.995    0.994
Specificity             0.998    0.999    0.999    0.997    1.000
Pos Pred Value          0.996    0.996    0.993    0.987    0.999
Neg Pred Value          1.000    0.998    0.998    0.999    0.999
Prevalence              0.284    0.194    0.174    0.164    0.184
Detection Rate          0.284    0.192    0.172    0.163    0.183
Detection Prevalence    0.285    0.193    0.174    0.165    0.183
Balanced Accuracy       0.999    0.996    0.994    0.996    0.997
```

The accuracy is pretty satisfiying which is 0.9946. We can also see that most incorrectly classified cases were of type D.
Let's now compare this accuracy to the accuracy of secound approach.



```r
w.1 <- nrow(subset(testing, user_name == "adelmo"))
o.1 <- confusionMatrix(predict(fit.adelmo,newdata=subset(testing[,-ncol(testing)], user_name == "adelmo")[,-1]),subset(testing, user_name == "adelmo")$classe)[[3]][1]

o.2 <- confusionMatrix(predict(fit.carlitos,newdata=subset(testing[,-ncol(testing)], user_name == "carlitos")[,-1]),subset(testing, user_name == "carlitos")$classe)[[3]][1]
w.2 <- nrow(subset(testing, user_name == "carlitos"))

o.3 <- confusionMatrix(predict(fit.charles,newdata=subset(testing[,-ncol(testing)], user_name == "charles")[,-1]),subset(testing, user_name == "charles")$classe)[[3]][1]
w.3 <- nrow(subset(testing, user_name == "charles"))

o.4 <- confusionMatrix(predict(fit.eurico,newdata=subset(testing[,-ncol(testing)], user_name == "eurico")[,-1]),subset(testing, user_name == "eurico")$classe)[[3]][1]
w.4 <- nrow(subset(testing, user_name == "eurico"))

o.5 <- confusionMatrix(predict(fit.jeremy,newdata=subset(testing[,-ncol(testing)], user_name == "jeremy")[,-1]),subset(testing, user_name == "jeremy")$classe)[[3]][1]
w.5 <- nrow(subset(testing, user_name == "jeremy"))

o.6 <- confusionMatrix(predict(fit.pedro,newdata=subset(testing[,-ncol(testing)], user_name == "pedro")[,-1]),subset(testing, user_name == "pedro")$classe)[[3]][1]
w.6 <- nrow(subset(testing, user_name == "pedro"))
```

After calculating accuracy statistic and weighting it by the number of testing cases for each user i have obtained a total accuracy for the secound approach of 0.9942. It is slightly but not noticably better. I decided to use the first approach for the coursera-testing-set

##Predicting the testing data set from Coursera

The secound part of assignment assumes predicting  set of 20 observations. The predictions from my first model are as follows:


```r
data.answers <- read.csv("C:/Users/Kuba/Desktop/pml-testing.csv", header=TRUE, sep=",", na.strings= c("NA", ""))

answers <- predict(fit,data.answers)
print(answers)
```

```
 [1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
```
##Anwers for the secound part of assigment
 Below is a code that generates anwers for the secound part of the assignment.


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
```
