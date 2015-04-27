setwd("~/stud/datasciencecoursera/run_analysis")
library(reshape2)

if(!file.exists("./data")){dir.create("./data")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile = "./data/dataset.zip", method = "curl")
unzip("./data/dataset.zip")


tests <- list.files("./UCI HAR Dataset/test", full.names = TRUE)
trains <- list.files("./UCI HAR Dataset/train", full.names = TRUE)
commons <- list.files("./UCI HAR Dataset", full.names = TRUE)

## Reading the "test" data sets
subject_test <- read.table(tests[2], header = FALSE)
X_test <- read.table(tests[3], header = FALSE)
y_test <- read.table(tests[4], header = FALSE)

## Reading the "train" data sets
subject_train <- read.table(trains[2], header = FALSE)
X_train <- read.table(trains[3], header = FALSE)
y_train <- read.table(trains[4], header = FALSE)

## Reading the "activity_lables" and "features"
activity_labels <- read.table(commons[1], header = FALSE, stringsAsFactors = FALSE)
features_tmp <- read.table(commons[3], header = FALSE, stringsAsFactors = FALSE)
features <- features_tmp[,2] 

## Merging data sets
subject <- rbind(subject_train, subject_test)
activity <- rbind(y_train, y_test)
X <- rbind(X_train, X_test)
DF <- cbind(subject, activity, X)
colnames(DF) <- c("subject", "activity", features)

## Replace activity codes
DF$activity <- factor(DF$activity, levels = c(1:6), labels = activity_labels$V2)

## Extracting the mean and sd variables
cols_selected <- grep("mean|std", colnames(DF))
extracted_data <- DF[, c(1, 2, cols_selected)]

## Creating a data set with the average of each variable
melt_data <- melt(extracted_data, id=c("subject", "activity")) 
tidy_data <- dcast(melt_data, subject + activity ~ variable, mean)

##Save tidy data file
write.table(tidy_data, file = "./tidy_data.txt", row.names = FALSE, sep = ",")
