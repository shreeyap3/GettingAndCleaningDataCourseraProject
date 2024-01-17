###################################################################
######################## SHREEYA PATEL : START ####################
###################################################################


## Assignment: Getting and Cleaning Data Course Project 

#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
#The goal is to prepare tidy data that can be used for later analysis. 

# Required to submit: 
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis, and 
# 3) a code book that describes the variables, the data, and any transformations or work 
#   that you performed to clean up the data called CodeBook.md. 
#   You should also include a README.md in the repo with your scripts. 

# This repo explains how all of the scripts work and how they are connected.

#The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S 
# smartphone. A full description is available at the site where the data was obtained:
#  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# Here are the data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


# This function merges the training & test data sets
# FunctionName : cleaningData
cleaningData <- function(){
  
  # Question 1 Start . Merges the training and the test sets to create one data set.
  
  # Download File
  if (!file.exists("data"))
    datadirectory <- dir.create("data")
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  datafile <- download.file(fileURL,destfile = "data/datafile.zip" , method = "curl")
  unzip(zipfile = "data/datafile.zip" , exdir = "data" )
  
  # Question 1 Reading Data
  
  #  Reading Training Data:
  x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
  
  #  Reading testing Data:
  x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
  
  #  Reading Features Data:
  features <- read.table('./data/UCI HAR Dataset/features.txt')
  
  # Reading Activity Labels Data:
  activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')
  
  # Assigning Column Names:
  colnames(x_train) <- features[,2] 
  colnames(y_train) <-"activityId"
  colnames(subject_train) <- "subjectId"
  
  colnames(x_test) <- features[,2] 
  colnames(y_test) <- "activityId"
  colnames(subject_test) <- "subjectId"
  
  colnames(activityLabels) <- c('activityId','activityType')
  
  # Merging Train Data, Test Data & All:
  mrg_train <- cbind(y_train, subject_train, x_train)
  mrg_test <- cbind(y_test, subject_test, x_test)
  setAllInOne <- rbind(mrg_train, mrg_test)
  
  # Question 1 End . Merges the training and the test sets to create one data set.
  
  
  # Question 2  Start . Extracts only the measurements on the mean and standard deviation for each measurement. 
  
  # Reading column names:
  colNames <- colnames(setAllInOne)
  
  # Creating Vector for defining ID, Mean and Standard Deviation:
  mean_and_std <- (grepl("activityId" , colNames) | 
                     grepl("subjectId" , colNames) | 
                     grepl("mean.." , colNames) | 
                     grepl("std.." , colNames) 
  )
  
  # Making necessary subset from setAllInOne:
  setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]
  
  # Question 2  End . Extracts only the measurements on the mean and standard deviation for each measurement.
  
  #Question 3 Start . Uses descriptive activity names to name the activities in the data set
  
  # Added the activitylabel & merged
  setWithActivityNames <- merge( activityLabels, setForMeanAndStd, 
                                by='activityId',
                                all.x=TRUE)
  #Question 3 End . Uses descriptive activity names to name the activities in the data set
  
  #Question 4 Start . Appropriately labels the data set with descriptive variable names.
    # Done above in question 1 & 2
  #Question 4 End . Appropriately labels the data set with descriptive variable names.
  
  
  #Question 5 Start . From the data set in step 4, creates a second, independent tidy data 
  # set with the average of each variable for each activity and each subject.
  # Creating the secondTidySet
  secondTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
  
  secondTidySet <- secondTidySet[order(secondTidySet$subjectId, secondTidySet$activityId),]
  View(secondTidySet)
  
  # Writing to a file
  write.table(secondTidySet, "secondTidySet.txt", row.name=FALSE)
  
  #Question 5 End . From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  
}

###################################################################
######################## SHREEYA PATEL : END ####################
###################################################################