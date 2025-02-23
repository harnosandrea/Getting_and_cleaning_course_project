---
title: "Getting and Cleaning data project"
author: "Harnos Andrea"
date: "10/23/2020"
output: 
  html_document:
    keep_md: yes
---



1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation for each measurement.
3.  Uses descriptive activity names to name the activities in the data set
4.  Appropriately labels the data set with descriptive variable names.
5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Getting the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#Download the data
if (!(dir.exists("data")) & !(file.exists("Dataset.zip")))
{download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile ="Dataset.zip", method="curl")
unzip("Dataset.zip",exdir="data")
setwd("data/UCI HAR Dataset/")
}
```

## Reading the data


```r
## Metadata
feature.names <- read.table("features.txt")
activity.labels <- read.table("activity_labels.txt", header = FALSE)

# Train data
subject.train <- read.table("train/subject_train.txt", header = FALSE)
activity.train <- read.table("train/y_train.txt", header = FALSE)
features.train <- read.table("train/X_train.txt", header = FALSE)

# Test data
subject.test <- read.table("test/subject_test.txt", header = FALSE)
activity.test <- read.table("test/y_test.txt", header = FALSE)
features.test <- read.table("test/X_test.txt", header = FALSE)
```


## 1.  Merges the training and the test sets to create one data set.


```r
# Binding the rows of train and test datasets
subject <- rbind(subject.train, subject.test)
activity <- rbind(activity.train, activity.test)
features <- rbind(features.train, features.test)

# Binding the datasets by coloumns

alldata <- cbind(features,activity,subject)

# Naming the merged dataset
colnames(alldata) <- c(as.character(feature.names[,2]), "Activity", "Subject")
```

## 2.  Extracts only the measurements on the mean and standard deviation for each measurement.


```r
# Column indices having mean or std in them.
col.MeanSTD <- grep(".*Mean.*|.*Std.*", names(alldata), ignore.case=TRUE)

# Adding 'activity' and 'subject' to col.MeanSTD
col.MeanSTD <- c(col.MeanSTD, 562, 563)

# Extract the required columns from 'alldata' 

extracteddata <- alldata[,col.MeanSTD]
```

## 3.  Uses descriptive activity names to name the activities in the data set


```r
extracteddata$Activity <- factor(extracteddata$Activity, levels = activity.labels[,1], labels = activity.labels[,2])
```

## 4.  Appropriately labels the data set with descriptive variable names.

List of replacements:
-    Acc: Accelerometer
-    Gyro: Gyroscope
-    BodyBody: Body
-    Mag: Magnitude
-    f: Frequency
-    t: Time


```r
names.extracteddata <- names(extracteddata)
names.extracteddata<-gsub("Acc", "Accelerometer", names.extracteddata)
names.extracteddata<-gsub("Gyro", "Gyroscope", names.extracteddata)
names.extracteddata<-gsub("BodyBody", "Body", names.extracteddata)
names.extracteddata<-gsub("Mag", "Magnitude", names.extracteddata)
names.extracteddata<-gsub("^t", "Time", names.extracteddata)
names.extracteddata<-gsub("^f", "Frequency", names.extracteddata)
names.extracteddata<-gsub("tBody", "TimeBody", names.extracteddata)
names.extracteddata<-gsub("-mean()", "Mean", names.extracteddata, ignore.case = TRUE)
names.extracteddata<-gsub("-std()", "STD", names.extracteddata, ignore.case = TRUE)
names.extracteddata<-gsub("-freq()", "Frequency", names.extracteddata, ignore.case = TRUE)
names.extracteddata<-gsub("angle", "Angle", names.extracteddata)
names.extracteddata<-gsub("gravity", "Gravity", names.extracteddata)
names(extracteddata)<-names.extracteddata
```

## 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


```r
extracteddata$Subject <- factor(extracteddata$Subject)

# We create avgdata for the average for each activity and subject. 

avgdata <- aggregate(. ~Subject + Activity, extracteddata, mean)
write.table(avgdata, file = "avgdata.txt", row.names = FALSE)
```

