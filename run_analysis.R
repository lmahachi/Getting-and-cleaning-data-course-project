# run_analysis.R

#Installing packages
library(plyr)
library(data.table)
library(dplyr)

# Downloading data and unziping 
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", temp)
unzip(temp, list = TRUE)

# Assigning all data frames
YTest <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))
XTest <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))
SubjectTest <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))
YTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))
XTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))
SubjectTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"))
Features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"))
unlink(temp)

# Getting features and merging the datasets
colnames(XTrain) <- t(Features[2])
colnames(XTest) <- t(Features[2])
XTrain$activities <- YTrain[, 1]
XTrain$participants <- SubjectTrain[, 1]
XTest$activities <- YTest[, 1]
XTest$participants <- SubjectTest[, 1]
Data <- rbind(XTrain, XTest)
duplicated(colnames(Data))
Data <- Data[, !duplicated(colnames(Data))]

# Getting mean and standard deviation
Mean <- grep("mean()", names(Data), value = FALSE, fixed = TRUE)
Mean <- append(Mean, 471:477)
InstrumentMeanMatrix <- Data[Mean]
STD <- grep("std()", names(Data), value = FALSE)
InstrumentSTDMatrix <- Data[STD]

# Naming activities with descriptive activity names
Data$activities <- as.character(Data$activities)
Data$activities[Data$activities == 1] <- "Walking"
Data$activities[Data$activities == 2] <- "Walking Upstairs"
Data$activities[Data$activities == 3] <- "Walking Downstairs"
Data$activities[Data$activities == 4] <- "Sitting"
Data$activities[Data$activities == 5] <- "Standing"
Data$activities[Data$activities == 6] <- "Laying"
Data$activities <- as.factor(Data$activities)

#Descriptive variable names and data labels
names(Data)
names(Data) <- gsub("Acc", "Accelerator", names(Data))
names(Data) <- gsub("Mag", "Magnitude", names(Data))
names(Data) <- gsub("Gyro", "Gyroscope", names(Data))
names(Data) <- gsub("^t", "time", names(Data))
names(Data) <- gsub("^f", "frequency", names(Data))
Data$participants <- as.character(Data$participants)
Data$participants[Data$participants == 1] <- "Participant 1"
Data$participants[Data$participants == 2] <- "Participant 2"
Data$participants[Data$participants == 3] <- "Participant 3"
Data$participants[Data$participants == 4] <- "Participant 4"
Data$participants[Data$participants == 5] <- "Participant 5"
Data$participants[Data$participants == 6] <- "Participant 6"
Data$participants[Data$participants == 7] <- "Participant 7"
Data$participants[Data$participants == 8] <- "Participant 8"
Data$participants[Data$participants == 9] <- "Participant 9"
Data$participants[Data$participants == 10] <- "Participant 10"
Data$participants[Data$participants == 11] <- "Participant 11"
Data$participants[Data$participants == 12] <- "Participant 12"
Data$participants[Data$participants == 13] <- "Participant 13"
Data$participants[Data$participants == 14] <- "Participant 14"
Data$participants[Data$participants == 15] <- "Participant 15"
Data$participants[Data$participants == 16] <- "Participant 16"
Data$participants[Data$participants == 17] <- "Participant 17"
Data$participants[Data$participants == 18] <- "Participant 18"
Data$participants[Data$participants == 19] <- "Participant 19"
Data$participants[Data$participants == 20] <- "Participant 20"
Data$participants[Data$participants == 21] <- "Participant 21"
Data$participants[Data$participants == 22] <- "Participant 22"
Data$participants[Data$participants == 23] <- "Participant 23"
Data$participants[Data$participants == 24] <- "Participant 24"
Data$participants[Data$participants == 25] <- "Participant 25"
Data$participants[Data$participants == 26] <- "Participant 26"
Data$participants[Data$participants == 27] <- "Participant 27"
Data$participants[Data$participants == 28] <- "Participant 28"
Data$participants[Data$participants == 29] <- "Participant 29"
Data$participants[Data$participants == 30] <- "Participant 30"
Data$participants <- as.factor(Data$participants)

# Creating a second, independent tidy data set with the average of each variable for each activity and each subject.
Data.dt <- data.table(Data)
TidyData <- Data.dt %>%
  group_by(participants, activities) %>%
  summarise_all(list(mean = mean))
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)


