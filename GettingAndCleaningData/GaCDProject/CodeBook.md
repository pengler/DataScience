## CodeBook 

###Source of dataset 

The original dataset from which this data set is derived from  the "Human Activity Recognition Using Smartphone" data set.

###Aquiring the data set

The data set can be aquired from here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The description of the data is available here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones



###Files used

The following files from the "Human Activity and Recognition Using Smartphone" data set are used: 

    ./UCI HAR Dataset/activity_labels.txt
    ./UCI HAR Dataset/features.txt
    ./UCI HAR Dataset/test/subject_test.txt
    ./UCI HAR Dataset/test/X_test.txt
    ./UCI HAR Dataset/test/y_test.txt
    ./UCI HAR Dataset/train/subject_train.txt
    ./UCI HAR Dataset/train/X_train.txt
    ./UCI HAR Dataset/train/y_train.txt

### How the data was derived
 
* The original **training** and **test** data sets were culled to limit them 
to columns that matched either **-mean()** or **-std()**. These represent
the columns containing the means and the standard deviations of the original
data sets. Please note that several column names contained the value **Mean** but
they do not represent mean values. Please look at the exclusions section at the 
end of this document for more detail.  
* The **training** and **test** data were extended with additonal columns that
represent the **subject** and **activity** for each sample (row)
* The **training** and **test** data sets were then merged based on matching
all columns
* The **activity** column in the data set was modified from the original 
numeric factors to more descriptive factors as follows:  

| Original Factor | New Factor |
|-----------------|:-----------|
| 1 | WALKING |
| 2 | WALKING_UPSTAIRS |
| 3 | WALKING_DOWNSTAIRS |
| 4 | SITTING |
| 5 | STANDING |
| 6 | LAYING |

* The column header were cleaned up to make them more ledgible as follows:  
  * The ( and ) charcters were removed.  
  * The inital "t" was expanded into "time"  
  * The inital "f" was expanded into "frequency"  

### Description of variables

| Variable            |  Type  | Description |
|:--------------------|:-------|:------------|
|activity                   | Factor w/ 6 levels | The type of activity that is being measured |
|subject                    | int | The identifier for the subject that was measured |
|timeBodyAcc-mean-X          | num | mean of the time domain body acceleration for the x-axis via accelerometer|
|timeBodyAcc-mean-Y          | num | mean of the time domain body acceleration for the y-axis via accelerometer|
|timeBodyAcc-mean-Z          | num | mean of the time domain body acceleration for the z-axis via accelerometer|
|timeBodyAcc-std-X           | num | standard deviation of the time domain body acceleration for the x-axis via accelerometer|
|timeBodyAcc-std-Y           | num | standard deviation of the time domain body acceleration for the y-axis via accelerometer|
|timeBodyAcc-std-Z           | num | standard deviation of the time domain body acceleration for the z-axis via accelerometer|
|timeGravityAcc-mean-X       | num | mean of the time domain gravity acceleration for the x-axis via accelerometer|
|timeGravityAcc-mean-Y       | num | mean of the time domain gravity acceleration for the y-axis via accelerometer|
|timeGravityAcc-mean-Z       | num | mean of the time domain gravity acceleration for the z-axis via accelerometer|
|timeGravityAcc-std-X        | num | standard deviation of the time domain gravity acceleration for the x-axis via accelerometer|
|timeGravityAcc-std-Y        | num | standard deviation of the time domain gravity acceleration for the y-axis via accelerometer|
|timeGravityAcc-std-Z        | num | standard deviation of the time domain gravity acceleration for the z-axis via accelerometer|
|timeBodyAccJerk-mean-X      | num | mean of the time domain jerk signals for the x-axis via accelerometer|
|timeBodyAccJerk-mean-Y      | num | mean of the time domain jerk signals for the y-axis via accelerometer|
|timeBodyAccJerk-mean-Z      | num | mean of the time domain jerk signals for the z-axis via accelerometer|
|timeBodyAccJerk-std-X       | num | standard deviation of the time domain jerk signals for the x-axis via accelerometer|
|timeBodyAccJerk-std-Y       | num | standard deviation of the time domain jerk signals for the y-axis via accelerometer|
|timeBodyAccJerk-std-Z       | num | standard deviation of the time domain jerk signals for the z-axis via accelerometer|
|timeBodyGyro-mean-X         | num | mean of the time domain body acceleration for the x-axis via gyroscope|
|timeBodyGyro-mean-Y         | num | mean of the time domain body acceleration for the y-axis via gyroscope|
|timeBodyGyro-mean-Z         | num | mean of the time domain body acceleration for the z-axis via gyroscope|
|timeBodyGyro-std-X          | num | standard deviation of the time domain body acceleration for the x-axis via gyroscope|
|timeBodyGyro-std-Y          | num | standard deviation of the time domain body acceleration for the y-axis via gyroscope|
|timeBodyGyro-std-Z          | num | standard deviation of the time domain body acceleration for the z-axis via gyroscope|
|timeBodyGyroJerk-mean-X     | num | mean of the time domain jerk signals for the x-axis via gyroscope|
|timeBodyGyroJerk-mean-Y     | num | mean of the time domain jerk signals for the y-axis via gyroscope|
|timeBodyGyroJerk-mean-Z     | num | mean of the time domain jerk signals for the z-axis via gyroscope|
|timeBodyGyroJerk-std-X      | num | standard deviation of the time domain jerk signals for the x-axis via gyroscope|
|timeBodyGyroJerk-std-Y      | num | standard deviation of the time domain jerk signals for the y-axis via gyroscope|
|timeBodyGyroJerk-std-Z      | num | standard deviation of the time domain jerk signals for the z-axis via gyroscope|
|timeBodyAccMag-mean         | num | mean of the magnitude of the time domain body acceleration via accelerometer|
|timeBodyAccMag-std          | num | standard deviation of the magnitude of the time domain body acceleration via accelerometer|
|timeGravityAccMag-mean      | num | mean of the magnitude of the time domain gravity acceleration via accelerometer|
|timeGravityAccMag-std       | num | standard deviation of the magnitude of the time domain gravity acceleration via accelerometer|
|timeBodyAccJerkMag-mean     | num | mean of the magnitude of the time domain jerk signals via accelerometer|
|timeBodyAccJerkMag-std      | num | standard deviation of the magnitude of the time domain jerk signals via accelerometer|
|timeBodyGyroMag-mean        | num | mean of the magnitude of the time domain body acceleration for the via gyroscope|
|timeBodyGyroMag-std         | num | standard deviation of the magnitude of the time domain body acceleration for the via gyroscope|
|timeBodyGyroJerkMag-mean    | num | mean of the magnitude of the time domain jerk signals  via gyroscope|
|timeBodyGyroJerkMag-std     | num | standard deviation of the magnitude of the time domain jerk signals via gyroscope|
|frequencyBodyAcc-mean-X          | num | mean of the frequency domain body acceleration for the x-axis via accelerometer|
|frequencyBodyAcc-mean-Y          | num | mean of the frequency domain body acceleration for the y-axis via accelerometer|
|frequencyBodyAcc-mean-Z          | num | mean of the frequency domain body acceleration for the z-axis via accelerometer|
|frequencyBodyAcc-std-X           | num | standard deviation of the frequency domain body acceleration for the x-axis via accelerometer|
|frequencyBodyAcc-std-Y           | num | standard deviation of the frequency domain body acceleration for the y-axis via accelerometer|
|frequencyBodyAcc-std-Z           | num | standard deviation of the frequency domain body acceleration for the x-axis via accelerometer|
|frequencyBodyAccJerk-mean-X      | num | mean of the time domain jerk signals for the x-axis via accelerometer|
|frequencyBodyAccJerk-mean-Y      | num | mean of the time domain jerk signals for the x-axis via accelerometer|
|frequencyBodyAccJerk-mean-Z      | num | mean of the time domain jerk signals for the x-axis via accelerometer|
|frequencyBodyAccJerk-std-X       | num | standard deviation of the frequency domain jerk signals for the x-axis via accelerometer|
|frequencyBodyAccJerk-std-Y       | num | standard deviation of the frequency domain jerk signals for the y-axis via accelerometer|
|frequencyBodyAccJerk-std-Z       | num | standard deviation of the frequency domain jerk signals for the z-axis via accelerometer|
|frequencyBodyGyro-mean-X         | num | mean of the frequency domain body acceleration for the x-axis via gyroscope|
|frequencyBodyGyro-mean-Y         | num | mean of the frequency domain body acceleration for the y-axis via gyroscope|
|frequencyBodyGyro-mean-Z         | num | mean of the frequency domain body acceleration for the z-axis via gyroscope|
|frequencyBodyGyro-std-X          | num | standard deviation of the frequency domain body acceleration for the x-axis via gyroscope|
|frequencyBodyGyro-std-Y          | num | standard deviation of the frequency domain body acceleration for the y-axis via gyroscope|
|frequencyBodyGyro-std-Z          | num | standard deviation of the frequency domain body acceleration for the z-axis via gyroscope|
|frequencyBodyAccMag-mean         | num | mean of the magnitude of the frequency domain body acceleration via accelerometer|
|frequencyBodyAccMag-std          | num | standard deviation of the magnitude of the frequency domain body acceleration via accelerometer|
|frequencyBodyBodyAccJerkMag-mean | num | mean of the magnitude of the frequency domain jerk signals via accelerometer|
|frequencyBodyBodyAccJerkMag-std  | num | standard deviation of the magnitude of the frequency domain jerk signals via accelerometer|
|frequencyBodyBodyGyroMag-mean    | num | mean of the magnitude of the frequency domain body acceleration for the via gyroscope|
|frequencyBodyBodyGyroMag-std     | num | standard deviation of the magnitude of the frequency domain body acceleration for the via gyroscope|
|frequencyBodyBodyGyroJerkMag-mean| num | mean of the magnitude of the frequency domain jerk signals for the x-axis via gyroscope|
|frequencyBodyBodyGyroJerkMag-std | num | standard deviation of the magnitude of the frequency domain jerk signals for the x-axis via gyroscope|


## Exculsions
The following columns contain the term **Mean** however they were purposefully
omitted from the final data set because the **Mean** is a component in computing 
these values but the computed values are not **Mean** values. Here are the 
exlusions for your reference.  

    angle(tBodyAccMean,gravity)
    angle(tBodyAccJerkMean),gravityMean)
    angle(tBodyGyroMean,gravityMean)
    angle(tBodyGyroJerkMean,gravityMean)
    angle(X,gravityMean)
    angle(Y,gravityMean)
    angle(Z,gravityMean)

