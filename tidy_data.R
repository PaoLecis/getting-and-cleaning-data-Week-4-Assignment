
# set the input directory
dir.in <- '/Users/paolapibiri/Desktop/Courses/2_R_course_coursera/3_Getting_cleaning_data/week 4/UCI HAR Dataset/'

# red the three input files in train
subject.train <- read.table(paste0(dir.in, 'train/subject_train.txt'))
x.train <- read.table(paste0(dir.in, 'train/X_train.txt'))
y.train <- read.table(paste0(dir.in, 'train/y_train.txt'))

# read the three input files in test
subject.test <- read.table(paste0(dir.in, 'test/subject_test.txt'))
x.test <- read.table(paste0(dir.in, 'test/X_test.txt'))
y.test <- read.table(paste0(dir.in, 'test/y_test.txt'))

# read features,
features <- read.table(paste0(dir.in, 'features.txt'), as.is = T)


# read activity labels
activities <- read.table(paste0(dir.in, 'activity_labels.txt'))
colnames(activities) <- c("activity_id", "activity_label")


## 1 Merges the training and the test sets to create one data set###########################


# merge the columns of the df
merged.train <- cbind(subject.train, x.train, y.train)
merged.test <- cbind(subject.test, x.test, y.test)

#merge test and train by rows
merged.train.test <- rbind(merged.train, merged.test)

# assign column names
colnames(merged.train.test) <- c("subject", features[, 'V2'], "activity")

## 2 Extracts only the measurements on the mean and standard deviation for each measurement ################

# determine columns of data set to keep based on column name...
keep.columns <- grepl("subject|activity|mean|std", colnames(merged.train.test))

# select only the df with the columns to keep
merged.train.test.filtered <- merged.train.test[, keep.columns]


## 3 Uses descriptive activity names to name the activities in the data set


# replace activity values with named factor levels
merged.train.test.filtered$activity <- factor(merged.train.test.filtered$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


## 4 Appropriately labels the data set with descriptive variable names ##############################

# get column names
merged.train.test.filtered.col <- colnames(merged.train.test.filtered)

# remove special characters
merged.train.test.filtered.col <- gsub("[\\(\\)-]", "", merged.train.test.filtered.col)

# expand abbreviations and clean up names
merged.train.test.filtered.col <- gsub("^f", "Frequency_Domain_", merged.train.test.filtered.col)
merged.train.test.filtered.col <- gsub("^t", "Time_Domain_", merged.train.test.filtered.col)
merged.train.test.filtered.col <- gsub("Acc", "_Accelerometer", merged.train.test.filtered.col)
merged.train.test.filtered.col <- gsub("Gyro", "_Gyroscope", merged.train.test.filtered.col)
merged.train.test.filtered.col <- gsub("Mag", "_Magnitude", merged.train.test.filtered.col)
merged.train.test.filtered.col <- gsub("Freq", "_Frequency", merged.train.test.filtered.col)
merged.train.test.filtered.col <- gsub("std", "_Standard_Deviation", merged.train.test.filtered.col)

# correct typo
merged.train.test.filtered.col <- gsub("BodyBody", "Body", merged.train.test.filtered.col)

# use new labels as column names
colnames(merged.train.test.filtered) <- merged.train.test.filtered.col


## 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# group by subject and activity and calculate mean
merged.train.test.filtered.mean <- merged.train.test.filtered %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))



# output to file "df2.txt"
write.table(merged.train.test.filtered.mean, paste0(dir.in, "df2.txt"), row.names = FALSE, 
            quote = FALSE)



