

library(dplyr)
#Download the data
if (!(dir.exists("data")) & !(file.exists("Dataset.zip")))
{download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile ="Dataset.zip", method="curl")
unzip("Dataset.zip",exdir="data")
setwd("data/UCI HAR Dataset/")
}


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


# Binding the rows of train and test datasets
subject <- rbind(subject.train, subject.test)
activity <- rbind(activity.train, activity.test)
features <- rbind(features.train, features.test)

# Binding the datasets by coloumns

alldata <- cbind(features,activity,subject)

# Naming the merged dataset
colnames(alldata) <- c(as.character(feature.names[,2]), "Activity", "Subject")
 
# Column indices having mean or std in them.
col.MeanSTD <- grep(".*Mean.*|.*Std.*", names(alldata), ignore.case=TRUE)

# Adding 'activity' and 'subject' to col.MeanSTD
col.MeanSTD <- c(col.MeanSTD, 562, 563)

# Extract the required columns from 'alldata' 

extracteddata <- alldata[,col.MeanSTD]

# Labeling activity

extracteddata$Activity <- factor(extracteddata$Activity, levels = activity.labels[,1], labels = activity.labels[,2])

# Appropriately labeling the data set with descriptive variable names.

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

extracteddata$Subject <- factor(extracteddata$Subject)

# Tidy dataset for the average for each activity and subject. 

avgdata <- aggregate(. ~Subject + Activity, extracteddata, mean)
write.table(avgdata, file = "avgdata.txt", row.names = FALSE)

