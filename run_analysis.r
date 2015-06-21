# Coursera Getting and Cleanind Data course project
# Here are the data for the project: "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Tasks: " You should create one R script called run_analysis.R that does the following:"


# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# 1) Merges the training and the test sets to create one data set.
#reading in the data of x
dx1 <- read.table("./train/X_train.txt")
dx2 <- read.table("./test/X_test.txt")
#reading in the data of y
dy1 <- read.table("train/y_train.txt")
dy2 <- read.table("test/y_test.txt")
#reading in the data of the subjects
ds1 <- read.table("./train/subject_train.txt")
ds2 <- read.table("./test/subject_test.txt")
#Merging the data sets
dx <- rbind(dx1, dx2)
dy <- rbind(dy1, dy2)
ds <- rbind(ds1, ds2)

dim(dx) #10299   561
dim(dy) #10299     1
dim(ds) #10299     1

#delete temporary data
rm(dx1, dx2, dy1, dy2, ds1, ds2)

# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
#read in the features
features <- read.table("features.txt")
#define the needed features
needed_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
#subsetting
dx <- dx[, needed_features]
# correct var names
names(dx) <- features[needed_features, 2]
# remove "()" from var names
names(dx) <- gsub("\\(\\)", "", features[needed_features, 2])
# remove "-" from var names
names(dx) <- gsub("-", "", names(dx))
# capital m in "mean"
names(dx) <- gsub("mean", "Mean", names(dx)) 
#capital s in "std"
names(dx) <- gsub("std", "Std", names(dx))

#delete temporary data
rm(needed_features, features)

# 3. Uses descriptive activity names to name the activities in the data set.
#read in the features
activities <- read.table("activity_labels.txt")
# correct names
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
substr(activities[2, 2], 8, 8) <- toupper(substr(activities[2, 2], 8, 8))
substr(activities[3, 2], 8, 8) <- toupper(substr(activities[3, 2], 8, 8))
#assign names
dy[,1] = activities[dy[,1], 2]
names(dy) <- "activity"


# 4. Appropriately labels the data set with descriptive activity names.

names(ds) <- "subject"
cleaned_data <- cbind(ds, dy, dx)
if(!file.exists("./Cleaned_Data")){dir.create("./Cleaned_Data")}
write.table(cleaned_data, "./Cleaned_Data/merged_cleaned_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

Subjects_un = unique(ds)[,1]
Subjects_all = length(unique(ds)[,1])
Activities_all = length(activities[,1])
cols = dim(cleaned_data)[2]
data_averages = cleaned_data[1:(Subjects_all*Activities_all), ]

row = 1
for (s in 1:Subjects_all) {
  for (a in 1:Activities_all) {
    data_averages[row, 1] = Subjects_un[s]
    data_averages[row, 2] = activities[a, 2]
    temp <- cleaned_data[cleaned_data$subject==s & cleaned_data$activity==activities[a, 2], ]
    data_averages[row, 3:cols] <- colMeans(temp[, 3:cols])
    row = row+1
  }
}

#delete temporary data
rm(a, Activities_all, cols, row,s, Subjects_all, Subjects_un, activities, temp, ds, dx, dy)

#write cleaned data
write.table(data_averages, "./Cleaned_Data/data_averages.txt", row.name=FALSE)


