#1. Create one dataset by merging TEST and TRAINING datasets:

trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
head(trainData)
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
table(trainLabel)

trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainLabel, testLabel)

joinSubject <- rbind(trainSubject, testSubject)

#2. Extract mean and standard deviation for each measurement:
 
features <- read.table("./UCI HAR Dataset/features.txt")

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
joinData <- joinData[, meanStdIndices]
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
names(joinData) <- gsub("mean", "Mean", names(joinData)) 
names(joinData) <- gsub("std", "Std", names(joinData)) 
names(joinData) <- gsub("-", "", names(joinData))

# 3. Assign descriptive activity names to name the activities:

activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# 4. Label the dataset with descriptive activity names:
 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)

#    WRITE the FIRST Dataset

write.table(cleanedData, "Cleaned_Merged.txt")

# 5. Create tidy dataset with avg of each activity and subject:
 
subjectLen <- length(table(joinSubject))
activityLen <- dim(activity)[1]
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)

#    WRITE the SECOND Dataset

write.table(result, "Avg_Actv_Subj.txt")
