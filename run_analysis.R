#source('~/COURSERA DATA SCIENCE test/COURSE3WK4/PROJECT/run_analysis.R')
library(dplyr)
library(reshape2)

#This script will:
#1. Merge two data sets(TEST and TRAINING)
#2. Extract only the measurements relating to mean and standard deviation 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately label the dataset with descriptive variable names
#5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. This will be returned as part of the function

run_analysis <- function(){
  
  #Read the total list of abbreviated measurement names
  feats = read.table("C:/Users/TB/Documents/COURSERA DATA SCIENCE test/COURSE3WK4/PROJECT/UCI HAR Dataset/features.txt")
  #Read the subject indices for TEST dataset
  subTest = read.table("C:/Users/TB/Documents/COURSERA DATA SCIENCE test/COURSE3WK4/PROJECT/UCI HAR Dataset/test/subject_test.txt")
  #Read the list of measurements
  xTest = read.table("C:/Users/TB/Documents/COURSERA DATA SCIENCE test/COURSE3WK4/PROJECT/UCI HAR Dataset/test/X_test.txt")
  #Read the acitivty indices 
  yTest = read.table("C:/Users/TB/Documents/COURSERA DATA SCIENCE test/COURSE3WK4/PROJECT/UCI HAR Dataset/test/y_test.txt")
  
  #Read the subject indices for TRAIN dataset
  subTrain = read.table("C:/Users/TB/Documents/COURSERA DATA SCIENCE test/COURSE3WK4/PROJECT/UCI HAR Dataset/train/subject_train.txt")
  #Read the list of measurements
  xTrain = read.table("C:/Users/TB/Documents/COURSERA DATA SCIENCE test/COURSE3WK4/PROJECT/UCI HAR Dataset/train/X_train.txt")
  #Read the acitivty indices 
  yTrain = read.table("C:/Users/TB/Documents/COURSERA DATA SCIENCE test/COURSE3WK4/PROJECT/UCI HAR Dataset/train/y_train.txt")
  
  #Assign abbreviated column names to data frames associated with TEST Dataset
  colnames(xTest) <-feats$V2
  colnames(subTest) <-"Subject"
  colnames(yTest) <-"ActIndex"
  
  #Merge all data frames for TEST
  xTest1.1 <- cbind(subTest, yTest)
  xTest1.1 <- cbind(xTest1.1, xTest)
  
  #Assign abbreviated column names to data frames associated with TRAIN Dataset
  colnames(xTrain) <-feats$V2
  colnames(subTrain) <-"Subject"
  colnames(yTrain) <-"ActIndex"
  
  #Merge all data frames for TRAIN
  xTrain1.1 <- cbind(subTrain, yTrain)
  xTrain1.1 <- cbind(xTrain1.1, xTrain)
  
  #Combine TEST and TRAIN datasets
  m1 = rbind(xTest1.1, xTrain1.1)
  
  #Select specified column names that show subject index, activity index, and variables associated with mean & std dev
  cols <- grep("Subject|ActIndex|.mean|.std", names(m1), value=TRUE)
  m2 <-m1[,cols]
  
  #create column to indicate activity NAME 
  for (i in 1:nrow(m2)){
    if   (m2$ActIndex[i] == 1) m2$Activity[i] <- "WALKING"
    if   (m2$ActIndex[i] == 2) m2$Activity[i] <- "WALKING_UPSTAIRS"
    if   (m2$ActIndex[i] == 3) m2$Activity[i] <- "WALKING_DOWNSTAIRS"
    if   (m2$ActIndex[i] == 4) m2$Activity[i] <- "SITTING"
    if   (m2$ActIndex[i] == 5) m2$Activity[i] <- "STANDING"
    if   (m2$ActIndex[i] == 6) m2$Activity[i] <- "LAYING"
  }
  
  #Shorten the data set to no longer include activity index, but rather activity name
  m3<-m2[1:81]
  colnames(m3)[2]<-"Activity"
  m3$Activity <-m2$Activity
  
  #Create descriptive column names from abbreviated column names
  col2test <- names(m2.1)
  col2test <- sub("^t","",col2test)
  col2test <- sub("^f","",col2test)
  col2test <- sub("-mean", "Mean ",col2test)
  col2test <- sub("-std", "Std Dev ",col2test)
  col2test <- sub("-X", " (x)",col2test)
  col2test <- sub("-Y", " (y)",col2test)
  col2test <- sub("-Z", " (z)",col2test)
  col2test <- sub("-meanFreq", "Mean Frequency ",col2test)
  col2test <- sub("\\()", "", col2test)
  col2test <- gsub("Body", "Body ", col2test)
  col2test <- sub("Acc", "Acceleration ", col2test)
  col2test <- sub("Gravity", "Gravity ", col2test)
  col2test <- sub("Gyro", "Gyro ", col2test)
  col2test <- sub("Jerk", "Jerk ", col2test)
  col2test <- sub("Mag", "Mag ", col2test)
  colnames(m3) <- col2test

  
  
  #Reshape data for tidy data set
  m3Melt <-melt(m3, id=c("Subject", "Activity"))
  m3Data<-dcast(m3Melt, Subject + Activity ~ variable, mean) #SHOWS DESIRED OUTPUT FROM STEP 5
  
  return(m3Data)
}
  