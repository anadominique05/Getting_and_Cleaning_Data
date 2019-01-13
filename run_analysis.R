# Pre-Processing Steps:
###############################################################################

# Load dplyr package
library(dplyr)

# Download Data from url
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(fileUrl, zipFile)
}

dataFile <- "UCI HAR Dataset"
if (!file.exists(dataFile)) {
  unzip(zipFile)
}

# Read training data
train_Subjects <- read.table(file.path(dataFile, "train", "subject_train.txt"))
train_X <- read.table(file.path(dataFile, "train", "X_train.txt"))
train_Y <- read.table(file.path(dataFile, "train", "y_train.txt"))

# Read test data
test_Subjects <- read.table(file.path(dataFile, "test", "subject_test.txt"))
test_x <- read.table(file.path(dataFile, "test", "X_test.txt"))
test_y <- read.table(file.path(dataFile, "test", "y_test.txt"))

# Read features data
feat <- read.table(file.path(dataFile,"features.txt"), as.is = TRUE)

# Read activity_labels
exerciseActivity <- read.table(file.path(dataFile, "activity_labels.txt"))
colnames(exerciseActivity) <- c("activityid", "activitylabel")

# Merge the training and test sets to create one data set
###############################################################################

exercise <- rbind(
  cbind(train_Subjects, train_X, train_Y),
  cbind(test_Subjects, test_x, test_y)
)

# assign column names
colnames(exercise) <- c("subject", feat[, 2], "activity")


# Extract only the measurements on the mean and standard deviation for each measurement
###############################################################################


# get only columns with mean() or std() in their names
mean_std <- grepl("subject|activity|mean|std", colnames(exercise))

# ... and keep data in these columns only
exercise <- exercise[, mean_std]


# Use descriptive activity names to name the activities in the data set
###############################################################################

# update values with correct activity names
exercise$activity <- factor(exercise$activity, 
                            levels = exercise[, 1], labels = exercise[, 2])


# Appropriately label the data set with descriptive variable names
###############################################################################

# get column names
exerciseActivityCol <- colnames(exercise)

# remove special characters
exerciseActivityCol <- gsub("[\\(\\)-]", "", exerciseActivityCol)

# expand abbreviations and clean up names
exerciseActivityCol <- gsub("^f", "frequencyDomain", exerciseActivityCol)
exerciseActivityCol <- gsub("^t", "timeDomain", exerciseActivityCol)
exerciseActivityCol <- gsub("Acc", "Accelerometer", exerciseActivityCol)
exerciseActivityCol <- gsub("Gyro", "Gyroscope", exerciseActivityCol)
exerciseActivityCol <- gsub("Mag", "Magnitude", exerciseActivityCol)
exerciseActivityCol <- gsub("Freq", "Frequency", exerciseActivityCol)
exerciseActivityCol <- gsub("mean", "Mean", exerciseActivityCol)
exerciseActivityCol <- gsub("std", "StandardDeviation", exerciseActivityCol)

# correct typo
exerciseActivityCol <- gsub("BodyBody", "Body", exerciseActivityCol)

# use new labels as column names
colnames(exercise) <- exerciseActivityCol

# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject
###############################################################################

# group by subject and activity and summarise using mean
exerciseMean <- exercise %>% 
  group_by(subject, activity) %>%
  summarize_each(funs(mean))

# output to file "tidy_data.txt"
write.table(exerciseMean, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)