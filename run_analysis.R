# Getting and Cleaning Data Course Project

# Set the working directory
setwd("~/Coursera/Getting and Cleaning data/Course Project W4")

# Loading necessary packages
library(dplyr)
library(sjlabelled)
library(Hmisc)

################################################# Loading the necessary data #####################################

# Loading the training data
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subjects_train <- read.table("subject_train.txt")

# Loading test data
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subjects_test <- read.table("subject_test.txt")

# Loading feature data
features <- read.table("features.txt", as.is=TRUE)

# Loading activity labels
activity_labels <- read.table("activity_labels.txt")
colnames(activity_labels) <- c("activity_id", "activity_label")

############################################### Merge training and test set into onde dataset ##################

# Merge the x, y and subjects datasets
training_data <- cbind(subjects_train, y_train, x_train)
test_data <- cbind(subjects_test, y_test, x_test)

# Merge the train and test datasets to obtain one complete dataset
data <- rbind(training_data, test_data)

# Assign column names from the "features" file to the dataset
colnames(data) <- c("subject","activity",features$V2)

############################################### Extract only the measurements of the mean and standard deviation for each measurement #######

# Selecting columns with the mean and std
mean_data <- data[, grepl("mean\\(\\)", names(data))]
std_data <- data[, grepl("std\\(\\)", names(data))]

# Combining the mean and stdv datasets
features_data <- cbind(mean_data, std_data)

############################################# Use descriptive activity names to name the activities in the dataset #####################

# Convert id's of activity column to activity labels
data$activity <- factor(data$activity, levels=c(1,2,3,4,5,6), labels=activity_labels$activity_label)

############################################# Appropriatly label the datset with descriptive variable names ##################

# Adjusting the names of the features 
names(features_data) <- gsub("^t", "Time_", names(features_data))
names(features_data) <- gsub("^f", "Frequency_", names(features_data))
names(features_data) <- gsub("-", "", names(features_data))
names(features_data) <- gsub("mean\\(\\)", "_Mean_", names(features_data))
names(features_data) <- gsub("std\\(\\)", "_Stdv_", names(features_data))
names(features_data) <- gsub("Acc", "_Accelerometer", names(features_data))
names(features_data) <- gsub("Gyro", "_Gyroscope", names(features_data))

######################################### creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Merging the features, subjects and acitivites into one tidy dataset
tidy_data <- cbind(data$subject, data$activity, features_data)

# Adjust names of activity and subject columns
names(tidy_data)[1] <- "subject"
names(tidy_data)[2] <- "activity"

# Aggregate the data with the average of each activity and each subject
tidy_data <- aggregate(. ~ subject + activity, tidy_data, mean)

# Write the tidy dataset to a txt file
write.table(tidy_data, "tidy.txt", row.names=FALSE)
