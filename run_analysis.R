library("car"); library("data.table"); library("plyr"); library("dplyr")

## Step 0: Download the files from the web

zipped_file <- "smartphone_activity.zip" ## the name of the zipped file

# Download the file if it doesn't exist
if (!file.exists("smartphone_activity.zip")) {
        fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileurl, zipped_file, method = "curl")
}

data_directory <- "./UCI HAR Dataset" ## location of the unzipped data

# unzip the file if not already unzipped
if (!dir.exists(data_directory)) {
        unzip(zipped_file)
}

## Step 1: merge the training and test dataset into one dataset

# create a function that reads and merges the files within one half of the 
# dataset ("train" or "test") together and returns a data.frame
mergeFilesOfHalf <- function(directory = data_directory, half) {
        half_path <- paste(data_directory, "/", half, sep = "")
        
        ## read features list into R and create a vector of feature names
        features_path <- paste(directory, "/", "features.txt", sep = "")
        features <<- as.character(read.table(features_path)[,2])
        
        
        ## read subject, X, and y files into R

        subjects <- read.table(paste(half_path, "/", "subject", "_", half, 
                                    ".txt" , sep = ""))
        activities <- read.table(paste(half_path, "/", "y", "_", half, 
                                       ".txt", sep = ""))
        measures <- read.table(paste(half_path, "/", "X", "_", half, 
                                         ".txt", sep = "")) 
        
        # apply names to columns (this takes care of Step 4)
        setnames(subjects, names(subjects), "Subject")
        setnames(activities, names(activities), "Activity")
        setnames(measures, names(measures), features)
        
        # merge subjects, activities, and measures together
        DF <- cbind(subjects, activities, measures)
        
        DF # return data.frame
}

## merge the two halves together and convert to data.table
merged_data <- data.table(rbind(mergeFilesOfHalf(half = "test"), 
                                mergeFilesOfHalf(half = "train")))

## Step 2: Extract only the variables with mean and standard deviation for each
## measurement. Only includes variables with "mean()" or "std()"

# identify relevant variables
columns <- names(merged_data)
mean_vars <- grep("mean()", columns, fixed = TRUE, value = TRUE)
std_vars <- grep("std()", columns, fixed = TRUE, value = TRUE)
vars <- c("Subject", "Activity", mean_vars, std_vars)

# select only relevant variables
merged_data <- select(merged_data, one_of(vars))

## Step 3: Apply descriptive activity names to the dataset
merged_data[, Activity := as.character(Activity)][, Activity := recode(Activity, 
                "1='Walking'; 2='Walking Upstairs';
                3='Walking Downstairs'; 4='Sitting'; 5='Standing'; 6='Laying'")]

merged_data[, Activity := factor(Activity)]

## Step 4: Label the dataset with descriptive variable names

new_names <- make.names(names(merged_data))
new_names <- gsub(".", "", new_names, fixed = TRUE)
new_names <- gsub("mean", "Mean", new_names, fixed = TRUE)
new_names <- gsub("std", "STD", new_names, fixed = TRUE)
new_names <- gsub("Acc", "Accelerometer", new_names, fixed = TRUE)
setnames(merged_data, names(merged_data), new_names)

## Step 5: Create a separate, tidy dataset with the average of each variable 
## for each activity for each subject

tidy_data <- merged_data %>% group_by(Subject, Activity) %>% 
        summarize_each(funs(mean)) %>% arrange(Subject, Activity)

# save the tidy dataset to disk as a txt file
write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)





