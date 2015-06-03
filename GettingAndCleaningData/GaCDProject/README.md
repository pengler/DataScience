# run_analysis.R

This is the course project submission for:   
Coursera Course: Getting and Cleaning Data (getdata-013)   

## Purpose
This script will read in the data set from the "Human Activity Recognition
Using Smartphone" Data Set and produce a file containing the merged test and 
training data sets. The data will summarize all of the mean and 
standard deviation data point. The summarized data points will be the average
as grouped by activity and subject. 

## Aquiring the data set
The data set can be aquired from here: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The description of the data is available here: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Running the script
1. Before running the script ensure that the data set has been downloaded and unzipped in the 
same directory as the **run_analysis.R** script
2. Start and **R** console or **rstudio**
3. **source ("run_analysis.R")**
4. The tidied data will be in same directory in a file named **./tidyData.txt**
 
### Assumptions 
* The script expects the data to be downloaded and un-zipped into the same directory 
as the run_analysis.R script 
* The script expects the data to be in the same hierarchy as when you unzip the file

### File locations and variables 

The following variables can be modified to change the location 
of the files for data processing. 

| Variable | Description | Script Default |
|:---------|:------------|:---------------|
|fileActivities    |  Activities file             | "./UCI HAR Dataset/activity_labels.txt" |
|fileFeatures      |  Feature file                | "./UCI HAR Dataset/features.txt" |
|fileTesting       |  Test group data             | "./UCI HAR Dataset/test/X_test.txt" |
|fileTraining      |  Training group data         | "./UCI HAR Dataset/train/X_train.txt" |
|fileTestSubjects  |  Test group subject data     | "./UCI HAR Dataset/test/subject_test.txt" |
|fileTrainSubjects |  Training group subject data | "./UCI HAR Dataset/train/subject_train.txt" |
|fileDataSummarized|  Output of tidy data         | "./tidyData.txt" |

Other Variables:  

| Variable | Description | Script Default|
|:---------|:------------|:--------------|
|nrowsToImport| Number of rows to import (-1 = all) | -1 |
 
### Dependencies
The script requires the **dplyr** library to function
