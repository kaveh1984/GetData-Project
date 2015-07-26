library(dplyr)
# Read all the 8 files to read in the following 8 lines with the assumption
# that your working directory is the one where you have unzipped the provided data,
# and therefore has the UCI HAR Dataset folder inside it. Starting with 'test' group
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", colClasses = "factor")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", colClasses = "factor")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
#train group:
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", colClasses = "factor")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", colClasses = "factor")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
#descriptive files:
features <- read.table("./UCI HAR Dataset/features.txt", colClasses = c("numeric", "character"))
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
#Assign meaningful levels from 'activity_labels' to the indices in the 'y_*' files. 
levels(y_train$V1) <- levels(y_test$V1) <- activity_labels$V2
#Add the subject ID as first column, activity ID (y) as second column,
#And the rest of the 561 variables following from the 3rd column on:
test_data <- cbind(subject_test, y_test, X_test)
train_data <- cbind(subject_train, y_train, X_train)
test_train <- rbind(test_data, train_data)

mean_var_indices <- grep("mean", features$V2, ignore.case = T)                  # Extract the indices of variables containing the string 'mean' in their name.
std_var_indices <- grep("std", features$V2, ignore.case = T)                    # Extract the indices of variables containing the string 'std' in their name.
mean_std_indices <- c(mean_var_indices, std_var_indices)                        # Concatenate the two vectors of indices.
dataset <- test_train[,c(1:2,mean_std_indices+2)]                               # Extract the ID columns and the variables with 'mean' or 'std' in their names from the test+train data to construct our 'dataset' of interest. +2 because we have added the ID columns in the beginning.
selected_features <- features[mean_std_indices, 2]

abbreviated_features <- c("Acc", "Gyro", "Mag", "X", "Y", "Z", "meanFreq", "mean\\()", "std\\()")
descriptive_features <- c("Accelerometer", "Gyroscope", "Magnitude", "X-axis", "Y-axis", "Z-axis", "MeanFrequency", "Mean", "StandardDeviation")
for (i in 1:length(abbreviated_features)) {selected_features <- sub(abbreviated_features[i], descriptive_features[i], selected_features)}
for (i in 1:length(selected_features)) {if (substr(selected_features[i], 1, 1) == "t") selected_features[i] <- sub("t", "TimeDomain", selected_features[i])}
for (i in 1:length(selected_features)) {if (substr(selected_features[i], 1, 1) == "f") selected_features[i] <- sub("f", "FrequencyDomain", selected_features[i])}

colnames(dataset) <- c("SubjectID", "Activity", selected_features)
tidy_dataset <- dataset %>% group_by(SubjectID, Activity) %>% summarise_each(funs(mean)) 
write.table(tidy_dataset, "tidy_dataset.txt", row.names = F) 
