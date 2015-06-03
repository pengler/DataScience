#####
## run_analysis.R
##
## This script will read in the data set from the "Human Activity Recognition
## Using Smartphone" Data Set and produce a file containing the merged test and 
## training data sets. The data will summarize all of the mean and 
## standard deviation data point. The summarized data points will be the average
## as grouped by activity and subject. 

#####
## Import the libraries that we will be using, if the library is missing
## the script will fail 
require (dplyr)

#####
## Script outputs
## finalData - Data Frame - contains the tidy data
## finalDataSummarized - Data Frame that contains the summarized data that
##                       will be written to a file as defined by the
##                       "fileDataSummarized" variable in the next section


#####
## Configuration Variables
nrowsToImport      <- -1 #Number of rows to import, to allow for quicker testing, set to -1 for whole file
fileActivities     <- "./UCI HAR Dataset/activity_labels.txt"      # List of activities
fileFeatures       <- "./UCI HAR Dataset/features.txt"             # List of features (columns in the data)
fileTesting        <- "./UCI HAR Dataset/test/X_test.txt"          # Data from the test group
fileTraining       <- "./UCI HAR Dataset/train/X_train.txt"        # Data from the training group
fileTestSubjects   <- "./UCI HAR Dataset/test/subject_test.txt"    # Subjects (rows) for test group data
fileTrainSubjects  <- "./UCI HAR Dataset/train/subject_train.txt"  # Subject (rows) for training group data
fileDataSummarized <- "./tidyData.txt"                             # File to write summarized data to 

## You should not need to modify the script below this point

#####
## Read in the files that will be used to lable the data sets. 

## activities.txt contains the mapping from numeric to activitiy description
## Read in the activities.txt file
activities <- read.table(fileActivities)

## factors.txt contains the labels for the columns in the data sets 
## Read in the factors.txt file 
features <- read.table (fileFeatures)
numCols <-nrow(features)

## Look for any column that contains "mean" or "std" in the description
## and store this in a vector. I use this vector to build a new vector that
## will be used by the "colClasses" arg in read.table so that the script only
## reads in the columns we want, and we don't have to clean up later
matchColumns <-grep ("\\-mean\\(\\)|\\-std\\(\\)", features$V2, perl=TRUE)
importColumns <- rep("NULL",numCols) #fill our vector with nulls

for ( v in matchColumns) {
  importColumns[v]="numeric"         #set fields to "numeric" for the colums we want
}

## Read in the testing and training data sets
testing <- read.table (file = fileTesting, colClasses = importColumns, nrows=nrowsToImport)
training <- read.table (file = fileTraining, colClasses = importColumns, nrows=nrowsToImport) 

## Clean up the Column names on both data sets to represent what is in the factors file
myCols<-as.character(features[matchColumns,]$V2)
myCols<-gsub("^t","time",myCols)
myCols<-gsub("^f","frequency",myCols)
myCols<-gsub("\\(|\\)","",myCols)
##myCols<-gsub("^tB","timeB",myCols)
colnames(testing) <- myCols
colnames(training) <- myCols


## Read in the testing and training subject information
testSubjects <-read.table (file = fileTestSubjects,  nrows=nrowsToImport)
trainingSubjects <- read.table (file = fileTrainSubjects, nrows=nrowsToImport)

## Add the subject information into the testing and training data sets
testing$subject <- testSubjects$V1
training$subject <- trainingSubjects$V1

## Clean up the subject information to save memory
rm(testSubjects)
rm(trainingSubjects)

## Read in the testing and training activity information and convert to factor
testActivities <-read.table (file = "./UCI HAR Dataset/test/y_test.txt", nrows=nrowsToImport)
testActivities$V1 <-factor (testActivities$V1)
trainingActivities <- read.table (file = "./UCI HAR Dataset/train/y_train.txt", nrows=nrowsToImport)
trainingActivities$V1 <-factor (trainingActivities$V1)

## Add the activity information into the testing and training data sets
testing$activity <- testActivities$V1
training$activity <- trainingActivities$V1

## Clean up the subject information to save memory
rm(testActivities)
rm(trainingActivities)

## Reorder out stat sets so that the added columns are on the left
myCols <- append (myCols, c("subject","activity"), after=0)

testing<-testing[,myCols]
training<-training[,myCols]

## Merge the two data sets and clean up the temp tables
finalData<-merge (testing, training, all=TRUE)
rm(testing)
rm(training)

## Convert our activities which are integers to factors
## This is the final cleanup of the data set. 
## The finalData data frame is now tidy as per Requirement 4 
levels (finalData$activity) <- activities$V2

# Clean up miscellaneous data we no longer need
rm(features)
rm(activities)
rm(fileActivities) 
rm(fileFeatures)
rm(fileTesting)
rm(fileTraining) 
rm(fileTestSubjects) 
rm(fileTrainSubjects) 

#####
## Summarize the data and write to a file
finalDataSummarized <- finalData %>% group_by (activity,subject) %>% summarise_each(funs(mean),c(3:68))
write.table (finalDataSummarized, file=fileDataSummarized, row.names=FALSE)
