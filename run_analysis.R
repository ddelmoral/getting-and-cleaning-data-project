# Getting and Cleaning Data - Course Project
# JHU Coursera
# run_analysis.R 
#
# Author: David del Moral
#
# This script does the following.
#
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load required libraries 
library (data.table)
library(reshape2)
  
# Load activity labels & test data 
activities 		<- read.table("./UCI HAR Dataset/activity_labels.txt") [,2]
features 		<- read.table("./UCI HAR Dataset/features.txt")	[,2]
xTest			<- read.table("./UCI HAR Dataset/test/X_test.txt")
yTest			<- read.table("./UCI HAR Dataset/test/y_test.txt")
subjectTest 	<- read.table("./UCI HAR Dataset/test/subject_test.txt")
names(xTest) 	<- features

# Extract only the measurements on the mean and standard deviation for each measurement & set activity labels
extractFeatures <- grepl("mean|std", features)
xTest 			<- xTest[,extractFeatures]
yTest[,2] 		<- activities[yTest[,1]]
names(yTest) 	<- c("activityId", "activityLabel")
names(subjectTest) <- "subject"

# Data merge from tests
testData 		<- cbind(as.data.table(subjectTest), yTest, xTest)

# Load and process X_train & y_train data.
xTrain			<- read.table("./UCI HAR Dataset/train/X_train.txt")
yTrain 			<- read.table("./UCI HAR Dataset/train/y_train.txt")

subjectTrain 	<- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(xTrain) 	<- features

# Extract mean and standard deviation for each measurement.
xTrain 			<- xTrain[,extractFeatures]

# Set activity labels
yTrain[,2] 		<- activities[yTrain[,1]]
names(yTrain) 	<- c("activityId", "activityLabel")
names(subjectTrain) <- "subject"

# Data merge 
trainData 		<- cbind(as.data.table(subjectTrain), yTrain, xTrain)

# Test and train data merge 
data = rbind(testData, trainData)

idLabels    	<- c("subject", "activityId", "activityLabel")
dataLabels  	<- setdiff(colnames(data), idLabels)
meltData    	<- melt(data, id = idLabels, measure.vars = dataLabels)

# Apply mean function to dataset using dcast function
tidyData  		<- dcast(meltData, subject + activityLabel ~ variable, mean)

# tidy data file is created 
write.table(tidyData, file = "./tidyData.txt")
