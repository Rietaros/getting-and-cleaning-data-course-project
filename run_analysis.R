##############################################################################
#
# FILE
#   run_analysis.R
#
# OVERVIEW
#   Using data collected from 
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#   work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".


library(dplyr)

#------------------------#
# STEP 0.1 - Unzip Data
#------------------------#

zipFile <- "UCI HAR Dataset.zip"

# unzip zip file containing data if data directory doesn't already exist

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

#------------------------#
# STEP 0.2 - Read data
#------------------------#

# read training data
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


#-------------------------------------------------------------------------#
# Step 1.1 - Merge the training and the test sets to create one data set
#-------------------------------------------------------------------------#

# concatenate individual data tables to make single data table
humanAct <- rbind(
  cbind(trainSubjects, trainValues, trainActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables
rm(trainSubjects, trainValues, trainActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanAct) <- c("subject", features[, 2], "activity")

#---------------------------------------------------------------#
# Step 2.1 - Extract measurements on mean and standard deviation
# for each measurement
#---------------------------------------------------------------#

# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanAct))

# keep data in these columns only
humanAct <- humanAct[, columnsToKeep]

#-----------------------------------------------------------------------------#
# Step 3.1 - Use descriptive activity names to name the activities in dataset
#-----------------------------------------------------------------------------#

# replace activity values with named factor levels
humanAct$activity <- factor(humanAct$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

#---------------------------------------------------------------------------#
# Step 4.1 - Appropriately label the data set with descriptive variable names
#---------------------------------------------------------------------------#

# get column names
humanActCols <- colnames(humanAct)

# remove special characters
humanActCols <- gsub("[\\(\\)-]", "", humanActCols)

# clean up names
humanActCols <- gsub("^f", "frequencyDomain", humanActCols)
humanActCols <- gsub("^t", "timeDomain", humanActCols)
humanActCols <- gsub("Acc", "Accelerometer", humanActCols)
humanActCols <- gsub("Gyro", "Gyroscope", humanActCols)
humanActCols <- gsub("Mag", "Magnitude", humanActCols)
humanActCols <- gsub("Freq", "Frequency", humanActCols)
humanActCols <- gsub("mean", "Mean", humanActCols)
humanActCols <- gsub("std", "StandardDeviation", humanActCols)

# correct typo
humanActCols <- gsub("BodyBody", "Body", humanActCols)

# use new labels as column names
colnames(humanAct) <- humanActCols

#----------------------------------------------------------------------------#
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
#----------------------------------------------------------------------------#

# group by subject and activity and summarise using mean
humanActMeans <- humanAct %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

