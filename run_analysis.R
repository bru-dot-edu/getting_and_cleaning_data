library(dplyr)

##############################################################################
# Load in the files
##############################################################################

features <- read.table("UCI HAR Dataset/features.txt", 
                       col.names = c("int","feature"))

activities <- read.table("UCI HAR Dataset/activity_labels.txt", 
                         col.names = c("int", "activity"))

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", 
                     col.names = features$feature)

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", 
                     col.names = "label")

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                           col.names = "subject")

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", 
                      col.names = features$feature)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", 
                      col.names = "label")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                            col.names = "subject")

##############################################################################
# Merges the training and the test sets to create one data set. 
##############################################################################

y <- rbind(y_train, y_test)

x <- rbind(x_train, x_test)

sub <- rbind(subject_train, subject_test)

merge_df <- cbind(sub, y, x)

##############################################################################
# Extracts only the measurements on the mean and standard deviation for each measurement. 
##############################################################################

df <- merge_df %>% select(subject, label, contains("mean"), contains("std"))

##############################################################################
# Uses descriptive activity names to name the activities in the data set
##############################################################################

df$label <- activities[df$label, 2]

##############################################################################
# Appropriately labels the data set with descriptive variable names. 
##############################################################################

names(df)[2] <- "activity"

names(df) <- gsub("^t", 
                  "time", 
                  names(df))

names(df) <- gsub("tBody", 
                  "timeBody", 
                  names(df))

names(df) <- gsub("Acc", 
                  "accelerometer", 
                  names(df))

names(df) <- gsub("Mag", 
                  "magnitude", 
                  names(df))

names(df) <- gsub("^f", 
                  "frequency", 
                  names(df))

names(df) <- gsub("BodyBody", 
                  "body", 
                  names(df))

names(df) <- gsub("-mean()", 
                  "mean", 
                  names(df), 
                  ignore.case = TRUE)

names(df) <- gsub("-std()", 
                  "std", 
                  names(df), 
                  ignore.case = TRUE)

names(df) <- gsub("-freq()", 
                  "frequency", 
                  names(df), 
                  ignore.case = TRUE)

names(df) <- gsub("Gyro", 
                  "gyroscope", 
                  names(df))

##############################################################################
# From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.
##############################################################################

dff <- df %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

write.table(dff, 
            "dff.txt", 
            row.name=FALSE)