rm(list = ls())
library(reshape2)

folderName <- "UCI HAR Dataset"

##Getting the Data:
if (!file.exists(folderName)){
  filename <- "getdata_dataset.zip"
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", filename)
  unzip(filename) 
}  

getPath <- function(x)
{
  paste(folderName,x,sep = "/")
}

# Getting activity names + relevent features names and indices 
activityLabels <- read.table(getPath("activity_labels.txt"))
activityNames <- as.character(activityLabels[,2])
allFeatures <- read.table(getPath("features.txt"))
allFeatures[,2] <- as.character(allFeatures[,2])
featuresIndices <- grep(".*mean.*|.*std.*", allFeatures[,2])
featuresNames <- allFeatures[featuresIndices,2]
featuresNames <- sub("mean", "Mean", featuresNames)
featuresNames <- sub("std", "SD", featuresNames)
featuresNames <- gsub("\\(\\)", "", featuresNames)
featuresNames <- gsub("-", "", featuresNames)

# Load and merging train data
train <- read.table(getPath("train/X_train.txt"))[featuresIndices]
trainActivities <- read.table(getPath("train/Y_train.txt"))
trainSubjects <- read.table(getPath("train/subject_train.txt"))
trainData <- cbind(trainSubjects, trainActivities, train)
# Load and merging test data
test <- read.table(getPath("test/X_test.txt"))[featuresIndices]
testActivities <- read.table(getPath("test/Y_test.txt"))
testSubjects <- read.table(getPath("test/subject_test.txt"))
testData <- cbind(testSubjects, testActivities, test)
# merging train and test data together and adding activity names
allData <- rbind(trainData, testData)
colnames(allData) <- c("subjectID", "activityName", featuresNames)
allData$activityName <- factor(allData$activity, levels = activityLabels[,1], labels = activityNames)
allData$subjectID <- as.factor(allData$subject)

#Createing a second, independent tidy data set with the average of each variable for each activity and each subject.
allData.melted <- melt(allData, id = c("subjectID", "activityName"))
allData.mean <- dcast(allData.melted, subjectID + activityName ~ variable, mean)

#Saving the Mean Table
write.table(allData.mean, "tidyData_Mean.txt", row.names = F, quote = F)
