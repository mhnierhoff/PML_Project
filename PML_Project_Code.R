## Code only!!!
## For comments and further informations please check the PML_Report.Rmd

library(caret)
library(randomForest)


data1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
data2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
filename1 <- "pml-training.csv"
filename2 <- "pml-testing.csv"
download.file(url=data1, destfile=filename1,method="curl")
download.file(url=data2, destfile=filename2,method="curl")

training <- read.csv("pml-training.csv")
dim(training)
names(training)
#19622   160
testing <- read.csv("pml-testing.csv") 
columns <- c("roll_belt", "pitch_belt", "yaw_belt", 
             "total_accel_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x", 
             "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", 
             "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y", 
             "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", 
             "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "gyros_dumbbell_x", 
             "gyros_dumbbell_y", "gyros_dumbbell_z", "accel_dumbbell_x", "accel_dumbbell_y", 
             "accel_dumbbell_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", 
             "roll_forearm", "pitch_forearm", "yaw_forearm", "total_accel_forearm", "gyros_forearm_x", 
             "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x", "accel_forearm_y", "accel_forearm_z", 
             "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z")

training <- training[, c(columns, "classe")]
training$magnet_forearm_y <- as.integer(training$magnet_forearm_y)
training$magnet_forearm_z <- as.integer(training$magnet_forearm_z)
dim(training)

str(training)
testing <- testing[,columns]

head(training)
str(training)

print (paste("All NA removed:  ", any(is.na(training))==FALSE))

lstRF <- list()
nRuns <- 10
sumAcc <- 0
for (n in 1:nRuns){
        inTrain <- createDataPartition(training$classe, p=0.8, list=FALSE)
        train <- training[inTrain,]
        xvalid <- training[-inTrain,]
        rfObj = randomForest(classe ~ ., data = train, ntree=100)
        predX <-  predict(rfObj, xvalid[,columns])
        acc <- sum(predX == xvalid$classe) / nrow(xvalid)
        sumAcc <- sumAcc + acc
        print(sprintf ("Case:  %i, Accuracy:  %.2f%%", n, acc*100))
        lstRF[[n]] <- rfObj
}
avgAcc <- sumAcc / nRuns
sprintf("Overall Accuracy:  %.2f%%", avgAcc*100)

lstPred <- list()
for (n in 1:nRuns){
        pred <- predict(lstRF[[n]], testing)
        lstPred[[n]] <- pred
        #Check for consistent prediction
        if (n > 1)
                print(sprintf("Pred %i same as previous: %s", n, all(pred == lstPred[[n-1]])))
}

setwd("~/Desktop/PML_Project/Results")
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE, eol="")
        }
}
pred <- lstPred[[1]]
pml_write_files(pred)