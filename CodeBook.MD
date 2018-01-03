---
title: "CodeBook"
author: "Tommy Brant"
date: "January 1, 2018"
output: html_document
---

# Code Book Description

This is a code book that describes the variables, the data, and any transformations performed to clean up the data to acheive the tidy data output.
##Data Dictionary

####Subject 
Subject performing the activity, total of 30
    
####Activity
Activity being performed at the time of taking the measurement, total of 6.

####Features
The features selected for this original dataset come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ

tGravityAcc-XYZ

tBodyAccJerk-XYZ

tBodyGyro-XYZ

tBodyGyroJerk-XYZ

tBodyAccMag

tGravityAccMag

tBodyAccJerkMag

tBodyGyroMag

tBodyGyroJerkMag

fBodyAcc-XYZ

fBodyAccJerk-XYZ

fBodyGyro-XYZ

fBodyAccMag

fBodyAccJerkMag

fBodyGyroMag

fBodyGyroJerkMag


The set of variables that were estimated from these signals are: 

mean(): Mean value

std(): Standard deviation

mad(): Median absolute deviation 

max(): Largest value in array

min(): Smallest value in array

sma(): Signal magnitude area

energy(): Energy measure. Sum of the squares divided by the number of values. 

iqr(): Interquartile range 

entropy(): Signal entropy

arCoeff(): Autorregresion coefficients with Burg order equal to 4

correlation(): correlation coefficient between two signals

maxInds(): index of the frequency component with largest magnitude

meanFreq(): Weighted average of the frequency components to obtain a mean frequency

skewness(): skewness of the frequency domain signal 

kurtosis(): kurtosis of the frequency domain signal 

bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.

angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean

tBodyAccMean

tBodyAccJerkMean

tBodyGyroMean

tBodyGyroJerkMean


The complete list of variables of each feature vector is available in 'UCI HAR Dataset/features.txt'
 

## Data Transformations

### Getting Started

As part of this project was to merge data sets, select appropriate data, then create a tidy data set, there are some prerequisites needed to view data appropriately.

####Prerequisites:

Determine Subjects with corresponding Activities, and select variables for the TEST and TRAIN data set.

All of the code below was used with RStudio version 3.4.1 on a 64 bit Windows environment.
This script requires the use of the reshape2 R package.

####-STEP 1- Identify how to organize data frames.

General:

Read the total list of abbreviated measurement names ('UCI HAR Dataset/features.txt'). Stored as feats

TEST Data Frames:

Read the subject indices ('UCI HAR Dataset/test/subject_test.txt') This data frame is stored as subTest

Read the list of measurement data ('UCI HAR Dataset/test/X_test.txt') This data frame is stored as xTest

Read the acitivty indices. This will later be converted to activity name ('UCI HAR Dataset/test/y_test.txt') This data frame is stored as yTest

TRAIN Data Frames:

Read the subject indices ('UCI HAR Dataset/train/subject_train.txt') This data frame is stored as subTrain

Read the list of measurement data ('UCI HAR Dataset/train/X_train.txt') This data frame is stored as  xTrain

Read the acitivty indices. This will later be converted to activity name ('UCI HAR Dataset/train/y_train.txt') This data frame is stored as  yTrain

####-STEP 2- Create data sets for TEST and TRAIN

Upon reading in the data, is becomes clear that the Test data sets have the same number of rows. Below, the Test and Train data is merged, respectively, into one dataset with cbind(), while naming the Subject and Activity Index name columns appropriately. The results here are data frames xTest1.1 and XTrain1.1

```
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
```
  
####-STEP 3- Combine TEST and TRAIN data sets

The resulting data sets are labeled xTest1.1 and xTrain1.1. It is identified the Subject indices between the TEST and TRAIN data set are exclusive. The two data sets are merged using rbind(). The resulting data set is called m1

```  
  #Combine TEST and TRAIN datasets
  m1 = rbind(xTest1.1, xTrain1.1)
```

####-STEP 4- Assign Abbreviated Measurement Names to Respective Columns

At this point, the column names of m1 includes the 561 variable readings form xTest and xTrain. One of the purposes of this project was to select mean and standard deviation measurements.
As such, grep() was used with regular expressions to identify these variables, a character vector cols, and create a data set with only those variables. This new data set is called m2

```
  #Select specified column names that show subject index, activity index, and variables associated with mean & std dev
  cols <- grep("Subject|ActIndex|.mean|.std", names(m1), value=TRUE)
  m2<-m1[,cols]
  
```
The appropriate column names can be seen at this point. 

```  
names(m2)
[1] "Subject"                         "ActIndex"                        "tBodyAcc-mean()-X"               "tBodyAcc-mean()-Y"               "tBodyAcc-mean()-Z"              
[6] "tBodyAcc-std()-X"                "tBodyAcc-std()-Y"                "tBodyAcc-std()-Z"                "tGravityAcc-mean()-X"            "tGravityAcc-mean()-Y"           
[11] "tGravityAcc-mean()-Z"            "tGravityAcc-std()-X"             "tGravityAcc-std()-Y"             "tGravityAcc-std()-Z"             "tBodyAccJerk-mean()-X"          
[16] "tBodyAccJerk-mean()-Y"           "tBodyAccJerk-mean()-Z"           "tBodyAccJerk-std()-X"            "tBodyAccJerk-std()-Y"            "tBodyAccJerk-std()-Z"           
[21] "tBodyGyro-mean()-X"              "tBodyGyro-mean()-Y"              "tBodyGyro-mean()-Z"              "tBodyGyro-std()-X"               "tBodyGyro-std()-Y"              
[26] "tBodyGyro-std()-Z"               "tBodyGyroJerk-mean()-X"          "tBodyGyroJerk-mean()-Y"          "tBodyGyroJerk-mean()-Z"          "tBodyGyroJerk-std()-X"          
[31] "tBodyGyroJerk-std()-Y"           "tBodyGyroJerk-std()-Z"           "tBodyAccMag-mean()"              "tBodyAccMag-std()"               "tGravityAccMag-mean()"          
[36] "tGravityAccMag-std()"            "tBodyAccJerkMag-mean()"          "tBodyAccJerkMag-std()"           "tBodyGyroMag-mean()"             "tBodyGyroMag-std()"             
[41] "tBodyGyroJerkMag-mean()"         "tBodyGyroJerkMag-std()"          "fBodyAcc-mean()-X"               "fBodyAcc-mean()-Y"               "fBodyAcc-mean()-Z"              
[46] "fBodyAcc-std()-X"                "fBodyAcc-std()-Y"                "fBodyAcc-std()-Z"                "fBodyAcc-meanFreq()-X"           "fBodyAcc-meanFreq()-Y"          
[51] "fBodyAcc-meanFreq()-Z"           "fBodyAccJerk-mean()-X"           "fBodyAccJerk-mean()-Y"           "fBodyAccJerk-mean()-Z"           "fBodyAccJerk-std()-X"           
[56] "fBodyAccJerk-std()-Y"            "fBodyAccJerk-std()-Z"            "fBodyAccJerk-meanFreq()-X"       "fBodyAccJerk-meanFreq()-Y"       "fBodyAccJerk-meanFreq()-Z"      
[61] "fBodyGyro-mean()-X"              "fBodyGyro-mean()-Y"              "fBodyGyro-mean()-Z"              "fBodyGyro-std()-X"               "fBodyGyro-std()-Y"              
[66] "fBodyGyro-std()-Z"               "fBodyGyro-meanFreq()-X"          "fBodyGyro-meanFreq()-Y"          "fBodyGyro-meanFreq()-Z"          "fBodyAccMag-mean()"             
[71] "fBodyAccMag-std()"               "fBodyAccMag-meanFreq()"          "fBodyBodyAccJerkMag-mean()"      "fBodyBodyAccJerkMag-std()"       "fBodyBodyAccJerkMag-meanFreq()" 
[76] "fBodyBodyGyroMag-mean()"         "fBodyBodyGyroMag-std()"          "fBodyBodyGyroMag-meanFreq()"     "fBodyBodyGyroJerkMag-mean()"     "fBodyBodyGyroJerkMag-std()"     
[81] "fBodyBodyGyroJerkMag-meanFreq()"
```

####-STEP 5- Activity Names from Activity Indices
The activity indices are not indicative of which activity is being performed. A new column, Activity, is created  in m2 based on the ActIndex value using a for loop with nested if statements.
  
```
  #create column to indicate activity NAME 
  for (i in 1:nrow(m2)){
    if   (m2$ActIndex[i] == 1) m2$Activity[i] <- "WALKING"
    if   (m2$ActIndex[i] == 2) m2$Activity[i] <- "WALKING_UPSTAIRS"
    if   (m2$ActIndex[i] == 3) m2$Activity[i] <- "WALKING_DOWNSTAIRS"
    if   (m2$ActIndex[i] == 4) m2$Activity[i] <- "SITTING"
    if   (m2$ActIndex[i] == 5) m2$Activity[i] <- "STANDING"
    if   (m2$ActIndex[i] == 6) m2$Activity[i] <- "LAYING"
  }
```

####-STEP 6- Shorten the data set to no longer include activity index, but rather activity name

As part of the project, the activity name is desired. The activity index is surperfluous at this point. The ActIndex column becomes Activity. A new data set m3 is used below.

```
  #Shorten the data set to no longer include activity index, but rather activity name
  m3<-m2[1:81]
  colnames(m3)[2]<-"Activity"
  m3$Activity <-m2$Activity
```

####-STEP 7- Description Variable Names

The measurement variables are not clearly indicative of what measurement they are. The column names are updated using a character vector, col2Test, sub(), and gsub() functions. 

```
  #Create descriptive column names from abbreviated column names
  col2test <- names(m3)
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
```

Descriptive column and measurement names can be seen below.

```
names(m3)
 [1] "Subject"                                   "Activity"                                 
 [3] "Body Acceleration Mean  (x)"               "Body Acceleration Mean  (y)"              
 [5] "Body Acceleration Mean  (z)"               "Body Acceleration Std Dev  (x)"           
 [7] "Body Acceleration Std Dev  (y)"            "Body Acceleration Std Dev  (z)"           
 [9] "Gravity Acceleration Mean  (x)"            "Gravity Acceleration Mean  (y)"           
[11] "Gravity Acceleration Mean  (z)"            "Gravity Acceleration Std Dev  (x)"        
[13] "Gravity Acceleration Std Dev  (y)"         "Gravity Acceleration Std Dev  (z)"        
[15] "Body Acceleration Jerk Mean  (x)"          "Body Acceleration Jerk Mean  (y)"         
[17] "Body Acceleration Jerk Mean  (z)"          "Body Acceleration Jerk Std Dev  (x)"      
[19] "Body Acceleration Jerk Std Dev  (y)"       "Body Acceleration Jerk Std Dev  (z)"      
[21] "Body Gyro Mean  (x)"                       "Body Gyro Mean  (y)"                      
[23] "Body Gyro Mean  (z)"                       "Body Gyro Std Dev  (x)"                   
[25] "Body Gyro Std Dev  (y)"                    "Body Gyro Std Dev  (z)"                   
[27] "Body Gyro Jerk Mean  (x)"                  "Body Gyro Jerk Mean  (y)"                 
[29] "Body Gyro Jerk Mean  (z)"                  "Body Gyro Jerk Std Dev  (x)"              
[31] "Body Gyro Jerk Std Dev  (y)"               "Body Gyro Jerk Std Dev  (z)"              
[33] "Body Acceleration Mag Mean "               "Body Acceleration Mag Std Dev "           
[35] "Gravity Acceleration Mag Mean "            "Gravity Acceleration Mag Std Dev "        
[37] "Body Acceleration Jerk Mag Mean "          "Body Acceleration Jerk Mag Std Dev "      
[39] "Body Gyro Mag Mean "                       "Body Gyro Mag Std Dev "                   
[41] "Body Gyro Jerk Mag Mean "                  "Body Gyro Jerk Mag Std Dev "              
[43] "Body Acceleration Mean  (x)"               "Body Acceleration Mean  (y)"              
[45] "Body Acceleration Mean  (z)"               "Body Acceleration Std Dev  (x)"           
[47] "Body Acceleration Std Dev  (y)"            "Body Acceleration Std Dev  (z)"           
[49] "Body Acceleration Mean Freq (x)"           "Body Acceleration Mean Freq (y)"          
[51] "Body Acceleration Mean Freq (z)"           "Body Acceleration Jerk Mean  (x)"         
[53] "Body Acceleration Jerk Mean  (y)"          "Body Acceleration Jerk Mean  (z)"         
[55] "Body Acceleration Jerk Std Dev  (x)"       "Body Acceleration Jerk Std Dev  (y)"      
[57] "Body Acceleration Jerk Std Dev  (z)"       "Body Acceleration Jerk Mean Freq (x)"     
[59] "Body Acceleration Jerk Mean Freq (y)"      "Body Acceleration Jerk Mean Freq (z)"     
[61] "Body Gyro Mean  (x)"                       "Body Gyro Mean  (y)"                      
[63] "Body Gyro Mean  (z)"                       "Body Gyro Std Dev  (x)"                   
[65] "Body Gyro Std Dev  (y)"                    "Body Gyro Std Dev  (z)"                   
[67] "Body Gyro Mean Freq (x)"                   "Body Gyro Mean Freq (y)"                  
[69] "Body Gyro Mean Freq (z)"                   "Body Acceleration Mag Mean "              
[71] "Body Acceleration Mag Std Dev "            "Body Acceleration Mag Mean Freq"          
[73] "Body Body Acceleration Jerk Mag Mean "     "Body Body Acceleration Jerk Mag Std Dev " 
[75] "Body Body Acceleration Jerk Mag Mean Freq" "Body Body Gyro Mag Mean "                 
[77] "Body Body Gyro Mag Std Dev "               "Body Body Gyro Mag Mean Freq"             
[79] "Body Body Gyro Jerk Mag Mean "             "Body Body Gyro Jerk Mag Std Dev "         
[81] "Body Body Gyro Jerk Mag Mean Freq"
```

####-STEP 8- Create the tidy data set

A tidy data set is created using the reshape2 package to create a second, independent tidy data set with the average of each variable for each activity and each subject.

```
  #Reshape data for tidy data set
  m3Melt <-melt(m3, id=c("Subject", "Activity"))
  m3Data<-dcast(m3Melt, Subject + Activity ~ variable, mean) #SHOWS DESIRED OUTPUT FROM STEP 5
```

A brief portion of the tidy data set is shown below.

```
m3.1 = run_analysis()
m3.1[1:10, 1:5]
   Subject           Activity Body Acceleration Mean  (x) Body Acceleration Mean  (y) Body Acceleration Mean  (z)
1        1             LAYING                   0.2215982                -0.040513953                  -0.1132036
2        1            SITTING                   0.2612376                -0.001308288                  -0.1045442
3        1           STANDING                   0.2789176                -0.016137590                  -0.1106018
4        1            WALKING                   0.2773308                -0.017383819                  -0.1111481
5        1 WALKING_DOWNSTAIRS                   0.2891883                -0.009918505                  -0.1075662
6        1   WALKING_UPSTAIRS                   0.2554617                -0.023953149                  -0.0973020
7        2             LAYING                   0.2813734                -0.018158740                  -0.1072456
8        2            SITTING                   0.2770874                -0.015687994                  -0.1092183
9        2           STANDING                   0.2779115                -0.018420827                  -0.1059085
10       2            WALKING                   0.2764266                -0.018594920                  -0.1055004
```



