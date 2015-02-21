# Install and declare required library files
install.packages("plyr") 
install.packages("dplyr") 
library(plyr) 
library(dplyr) 
library(reshape2) 
library(stringr) 

# Read the Features List
features <- read.table("UCI HAR Dataset\\features.txt") 

# Read the Test Data
testX <- read.table("UCI HAR Dataset\\test\\X_test.txt") 
testY <- read.table("UCI HAR Dataset\\test\\y_test.txt") 
testSubjects <- read.table("UCI HAR Dataset\\test\\subject_test.txt")

# Task 1:  Merges the training and the test sets to create one data set

# Read the train Data
trainX <- read.table("UCI HAR Dataset\\train\\X_train.txt")
trainY <- read.table("UCI HAR Dataset\\train\\y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset\\train\\subject_train.txt")

# Merge the train and test to creat unified data set
combinedX <- rbind(testX, trainX)
combinedY <- rbind(testY, trainY)
combinedSubjects <- rbind(testSubjects, trainSubjects)

# Adding descriptive Headers (Extracted from Features column V2)
# Adjust the lables as characters (removes any non ISO symbols which could lead to errors)
mylables <- as.character(features$V2)
mylables <- make.names(mylables, unique=TRUE)
# setting the names vecotrs to the combined Data set
names(combinedX) <- mylables

#Adding the combined Y data set as the first column to the Combined X data set, creating a new result data set
# names : combinedData
combinedData <- cbind(combinedY,combinedX)
names(combinedSubjects) = c('Subjects')
combinedData <- cbind(combinedSubjects,combinedData)

# Task 2: extract only the measurements on the mean and standard deviation for each measurement 
subsetData <- select(combinedData, Subjects, V1, contains(".mean."), contains(".std.")) 

# Task 3: use descriptive activity names to name the activites in the data set 
subsetData$V1<- mapvalues(subsetData$V1, from = c("1", "2", "3", "4", "5", "6"), to = c("Walking", "WalkingUpStairs", "WalkingDownStairs", "Sitting", "Standing", "Lying"))

# Task 4: Appropriately labels the data set with descriptive variable names.
# See codebook.md for an additional detailes and explanation regarding the variable names

names(subsetData) <- str_replace_all(names(subsetData), "[.][.]", "")
names(subsetData) <- str_replace_all(names(subsetData), "BodyBody", "Body")
names(subsetData) <- str_replace_all(names(subsetData), "tBody", "Body")
names(subsetData) <- str_replace_all(names(subsetData), "fBody", "FFTBody")
names(subsetData) <- str_replace_all(names(subsetData), "tGravity", "Gravity")
names(subsetData) <- str_replace_all(names(subsetData), "fGravity", "FFTGravity")
names(subsetData) <- str_replace_all(names(subsetData), "Acc", "Acceleration")
names(subsetData) <- str_replace_all(names(subsetData), "Mag", "Magnitude")
# replace std with full descriptive StandardDeviation
for(i in 3:68) {if (str_detect(names(subsetData)[i], "[.]std"))  
    {names(subsetData)[i] <- paste0("StandardDeviation", str_replace(names(subsetData)[i], "[.]std", ""))}} 
# replace std with full descriptive Mean
for(i in 3:68) {if (str_detect(names(subsetData)[i], "[.]mean"))  
{names(subsetData)[i] <- paste0("Mean", str_replace(names(subsetData)[i], "[.]mean", ""))}}
names(subsetData) <- str_replace_all(names(subsetData), "[.]X", "XAxis")
names(subsetData) <- str_replace_all(names(subsetData), "[.]Y", "YAxis")
names(subsetData) <- str_replace_all(names(subsetData), "[.]Z", "ZAxis")

#Task 5: From the data set in step 4, creates a second, independent tidy data set 
#        with the average of each variable for each activity and each subject.

# Step 1: Use a split/apply/combine method. First, split the data by the subject and activity factors.
splitSet <- split(select(subsetData, 3:68), list(subsetData$Subjects, subsetData$V1))

# Step 2: use lapply to iterate over each item in the resulting list, and use apply to calculate the mean of the columns.
meanSet <- lapply(splitSet, function(x) apply(x, 2, mean, na.rm=TRUE))

# Step 3: Convert the list output to data frame
tidySet <- data.frame(t(sapply(meanSet,c)))

# Step 4: Split the Subject and Activity column snow merged within row names into distinct columns
labels <- data.frame(t(sapply(strsplit(rownames(tidySet), "[.]"),c)))
tidySet <- cbind(labels, tidySet)
# Setting the column names and convert infro Factors
tidySet <- dplyr::rename(tidySet,TestSubject = X1, Activity = X2)
tidySet$TestSubject <- as.factor(tidySet$TestSubject)
tidySet$Activity <- as.factor(tidySet$Activity)
rownames(tidySet) <- NULL

# write the tidy data set to a file for project submission
write.table(tidySet, "tidy_data_set.txt", row.names=FALSE)

# clean the environment
#rm(list=ls(all=TRUE))



























