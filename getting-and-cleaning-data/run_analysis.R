# Load data.table package
library(data.table)

# Get labels in the 2nd column
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")[, 2]
features <- read.table("UCI HAR Dataset/features.txt")[, 2]

# Read test data
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
names(X_test) = features

# Read train data
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
names(X_train) = features

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_sd_features <- grepl("mean|std", features)
X_test = X_test[, mean_sd_features]
X_train = X_train[, mean_sd_features]

## 3. Uses descriptive activity names to name the activities in the data set
y_test[, 2] = activity_labels[y_test[, 1]]
names(y_test) = c("ActivityID", "ActivityLabel")
names(subject_test) = "Subject"

y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("ActivityID", "ActivityLabel")
names(subject_train) = "Subject"

## 1. Merges the training and the test sets to create one data set.
test_data <- cbind(as.data.table(subject_test), y_test, X_test)
train_data <- cbind(as.data.table(subject_train), y_train, X_train)
merged_data = rbind(test_data, train_data)

## 4. Appropriately labels the data set with descriptive activity names.
ID_labels = c("Subject", "ActivityID", "ActivityLabel")
var_labels = setdiff(colnames(merged_data), ID_labels)
melt_data = melt(merged_data, id = ID_labels, measure.vars = var_labels)

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, Subject + ActivityLabel ~ variable, mean)
write.table(tidy_data, file = "tidy_data.txt")