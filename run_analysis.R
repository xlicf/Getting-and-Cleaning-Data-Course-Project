library(dplyr)


# 1: Merges the training and the test sets to create one data ------------------

# Import main data head
features <- read.table("features.txt", header = FALSE)
activityLabels <- read.table("activity_labels.txt", header = FALSE)

# Rename the main data
colnames(activityLabels) <-  c('activityID', 'activityLabel')

# Import test data
subjectTest <-  read.table('./test/subject_test.txt', header = FALSE)
xTest       <-  read.table('./test/x_test.txt', header = FALSE)
yTest       <-  read.table('./test/y_test.txt', header = FALSE)

# rename the test data
colnames(subjectTest)  = "subjectID";
colnames(xTest)       <-  features[, 2]
colnames(yTest)       <- "activityID"

# combine test data
testData <-  cbind(subjectTest, yTest, xTest)

# Import train data
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
xTrain       <- read.table('./train/x_train.txt',header=FALSE)
yTrain       <- read.table('./train/y_train.txt',header=FALSE)

# rename the train data
colnames(subjectTrain)  = "subjectID";
colnames(xTrain)        = features[,2];
colnames(yTrain)        = "activityID";

# combine train data
trainData <-  cbind(subjectTrain, yTrain, xTrain)

# Merge data
mergeData <- rbind(trainData, testData)


# 2: Extract only the measurements on the mean and std ------------------------


featuresNames <- features$V2[grep("mean\\(\\)|std\\(\\)",features$V2)]
#selectedNames<-c(as.character(featuresNames), "subject", "activity" )
meanStdData <- subset(mergeData, select = as.character(featuresNames))


# 3. Uses descriptive activity names to name the activities in the data set --------
activityNameData <- merge(activityLabels,mergeData, by = "activityID", all.x=TRUE)


# 4. Appropriately labels the data set with descriptive variable names --------

names(activityNameData) <- gsub("\\(\\)", "", names(activityNameData))
names(activityNameData) <- gsub("arCoeff", "AutorregresionCoeff", names(activityNameData))
names(activityNameData) <- gsub("maxInds", "maxIndex", names(activityNameData))

names(activityNameData) <- gsub("^t", "time", names(activityNameData))
names(activityNameData)<-gsub("^f", "frequency", names(activityNameData))
names(activityNameData)<-gsub("Acc", "Accelerometer", names(activityNameData))
names(activityNameData)<-gsub("Gyro", "Gyroscope", names(activityNameData))
names(activityNameData)<-gsub("Mag", "Magnitude", names(activityNameData))
names(activityNameData)<-gsub("BodyBody", "Body", names(activityNameData))


# 5. Creates a second, data set with the average for activity and subject --------

data2 <- aggregate( . ~ activityID + subjectID, activityNameData, mean)
write.table(data2, file = "tidyData.txt",row.name=FALSE)
