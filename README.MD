---
title: "Getting and Cleaning Data Course Project"
author: "Tommy Brant"
date: "December 31, 2017"
output: html_document
---

# Getting and Cleaning Data Course Project

The purpose of this project was to create a script that would take files from two data sets, merge them, extract desired variables, and assign appropriate column names. Additionally, an second, independent tidy set was to be returned from this script.


## Project Files

To get started, familiaraize yourself with each of the project files.

Project files:
 - 'run_analysis.R' (Script)
 - README.MD
 - CodeBook.MD
 - 'tidyDataOutput.CSV' (Output)
 - Dataset Files (Input)

### Getting and Cleaning Data Script
 'run_analysis.R' is a script that contains a function called run_analysis(). The function utilizes input from the dataset files. The output of this function is a data frame that contains a tidy data set.
 
 This script will:
 1. Merge two data sets(TEST and TRAINING)
 2. Extract only the measurements relating to mean and standard deviation 
 3. Uses descriptive activity names to name the activities in the data set
 4. Appropriately label the dataset with descriptive variable names
 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. This will be returned as part of the function
  
See 'CodeBook.MD' for specifics related to variables, the data, and data transformation.

### Output Files
The tidy data set has been exported to CSV format. See project file 'tidyDataOutput.CSV' 

### Dataset Files

The dataset can be downloaded from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The dataset includes the following files:

- 'UCI HAR Dataset/README.txt'
- 'UCI HAR Dataset/features_info.txt': Shows information about the variables used on the feature vector.
- 'UCI HAR Dataset/features.txt': List of all features.
- 'UCI HAR Dataset/activity_labels.txt': Links the class labels with their activity name.
- 'UCI HAR Dataset/train/X_train.txt': Training set.
- 'UCI HAR Dataset/train/y_train.txt': Training labels.
- 'UCI HAR Dataset/test/X_test.txt': Test set.
- 'UCI HAR Dataset/test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'UCI HAR Dataset/train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- 'UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
- 'UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- 'UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

####Dataset Notes: 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about the dataset used, contact: activityrecognition@smartlab.ws

## Acknowledgements:
This publication acknowledges the following for providing the dataset files:
[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

## License

This project is licensed under the MIT License.
