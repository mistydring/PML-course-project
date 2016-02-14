#written by MRR on 2/11/2016
#set working directory to folder where data are stored
setwd("C:/Users/Misty Ring-Ramirez/Dropbox/Coursera Courses/Practical Machine Learning Course Project")

#load libraries I anticipate needing
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(ggplot2)
#set random seed so results are reproducible
set.seed(223344)

#load training and testing data
train <- read.csv("pml-training.csv", na.strings=c("NA",""), header=TRUE)
test <- read.csv("pml-testing.csv", na.strings=c("NA",""), header=TRUE)

#partition training dataset into a 60/40 split
inTrain <- createDataPartition(y=train$classe, p=0.6, list=FALSE)
myTraining <- train[inTrain, ]; myTesting <- train[-inTrain, ]

#remove variables with near zero variance
nzv <- nearZeroVar(myTraining, saveMetrics=TRUE)
myTraining <- myTraining[,nzv$nzv==FALSE]

nzv<- nearZeroVar(myTesting,saveMetrics=TRUE)
myTesting <- myTesting[,nzv$nzv==FALSE]

#describe dimensions of training and testing dataset
dim(myTraining); dim(myTesting)
summary(myTraining)

#keep the variables I intend to use as features (and the outcome, of course)
myTraining <- myTraining[c("roll_belt", "pitch_belt", "yaw_belt", "total_accel_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x", "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "total_accel_dumbbell", "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z", "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", "roll_forearm", "pitch_forearm", "yaw_forearm", "total_accel_forearm", "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x", "accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z", "classe")]
myTesting <- myTesting[c("roll_belt", "pitch_belt", "yaw_belt", "total_accel_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x", "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "total_accel_dumbbell", "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z", "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", "roll_forearm", "pitch_forearm", "yaw_forearm", "total_accel_forearm", "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x", "accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z", "classe")]

#prediction with decision trees using rpart
modFitrp <- rpart(myTraining$classe ~., data=myTraining, method="class")
fancyRpartPlot(modFitrp)

predictions_rp <- predict(modFitrp, myTesting, type = "class")
c_matrix_rp <- confusionMatrix(predictions_rp, myTesting$classe)
c_matrix_rp

#prediction with random forests
set.seed(223344)
modFitrf <- randomForest(classe ~ ., data=myTraining)
predictions_rf <- predict(modFitrf, myTesting, type = "class")
c_matrix_rf <- confusionMatrix(predictions_rf, myTesting$classe)
c_matrix_rf

plot(modFitrf)

#predicting results on test data (finally!)
predictions_rf <- predict(modFitrf, myTesting, type = "class")
predictions_rf

#write the results to a text file
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
