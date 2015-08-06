## Load required libraries
library(data.table)
library(dplyr)

## STEP 1: Merges the training and the test sets to create one data set

# Read feature names
featureNames <- read.table("features.txt")[,2]

# Read activity labels
activityLabels <- read.table("activity_labels.txt")

# Read data
subjectTrain <- read.table("train/subject_train.txt")
subjectTest <- read.table("test/subject_test.txt")
featuresTrain <- read.table("train/X_train.txt")
featuresTest <- read.table("test/X_test.txt")
activityTrain <- read.table("train/y_train.txt")
activityTest <- read.table("test/y_test.txt")

# Merge data by sets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Name features
colnames(features) <- t(featureNames)

# Merge all sets in one variable
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

## STEP 2: Extracts only the measurements on the mean and standard
## deviation for each measurement.

# Determine which columns contain "mean()" or "std()"
MeanStdCols <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# Add activity and subject columns to the list
requiredColumns <- c(MeanStdCols, 562, 563)
dim(completeData)

# Create list with selected columns
extractedData <- completeData[,requiredColumns]
dim(extractedData)


## STEP 3: Uses descriptive activity names to name the activities 
## in the data set

extractedData$Activity <- as.factor(extractedData$Activity)

## STEP 4: Appropriately labels the data set with descriptive
## activity names. 

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

## STEP 5: Creates a second, independent tidy data set with the
## average of each variable for each activity and each subject.

# create the tidy data set
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

# write the tidy data set to a file
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)