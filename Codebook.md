## Introduction

This codebook corresponds to the run_analysis.R script included in the srlindley/run_analysis repository

It modifies and updates the original codebook "features_info.txt" and the corresponding variables list "features.txt" that were provided with the original data set on which this script is based:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

This represents data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The script run_analysis.R carries out the following activities to produce a tidy data set:

1) Merges the training and the test sets to create one data set.  
2) Extracts only the measurements on the mean and standard deviation for each measurement. 
3) Uses descriptive activity names to name the activities in the data set.  
4) Appropriately labels the data set with descriptive variable names.   
5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject.   

The operation of the script to carry out these activities is explained in the corresponding README.md file in the same Github repository set out above (together with additional comments in the script itself).

## Variable (feature) selection

### Original full features

The following explanation is as per the original original codebook "features_info.txt"

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ  
tGravityAcc-XYZ  
tBodyAccJerk-XYZ  
tBodyGyro-XYZ  
tBodyGyroJerk-XYZ  
tBodyAccMag  
tGravityAccMag  
tBodyAccJerkMag  
tBodyGyroMag  
tBodyGyroJerkMag  
fBodyAcc-XYZ  
fBodyAccJerk-XYZ  
fBodyGyro-XYZ  
fBodyAccMag  
fBodyAccJerkMag  
fBodyGyroMag  
fBodyGyroJerkMag  

The set of variables that were estimated from these signals are: 

mean(): Mean value  
std(): Standard deviation    
mad(): Median absolute deviation  
max(): Largest value in array  
min(): Smallest value in array  
sma(): Signal magnitude area  
energy(): Energy measure. Sum of the squares divided by the number of values.  
iqr(): Interquartile range   
entropy(): Signal entropy  
arCoeff(): Autorregresion coefficients with Burg order equal to 4  
correlation(): correlation coefficient between two signals  
maxInds(): index of the frequency component with largest magnitude  
meanFreq(): Weighted average of the frequency components to obtain a mean frequency  
skewness(): skewness of the frequency domain signal  
kurtosis(): kurtosis of the frequency domain signal  
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.  
angle(): Angle between to vectors.  

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean  
tBodyAccMean  
tBodyAccJerkMean  
tBodyGyroMean  
tBodyGyroJerkMean  

### Reduced set of features for final dataset

The set of features is reduced by the script, so that while all the original feature vector patterns were retained, ie:

**feature vector patterns:**  
tBodyAcc-XYZ  
tGravityAcc-XYZ  
tBodyAccJerk-XYZ  
tBodyGyro-XYZ  
tBodyGyroJerk-XYZ  
tBodyAccMag  
tGravityAccMag  
tBodyAccJerkMag  
tBodyGyroMag  
tBodyGyroJerkMag  
fBodyAcc-XYZ  
fBodyAccJerk-XYZ  
fBodyGyro-XYZ  
fBodyAccMag  
fBodyAccJerkMag  
fBodyGyroMag  
fBodyGyroJerkMag  

the set of variables estimated from these signals was reduced from the original list to only two types:

**estimated variables**  
mean(): Mean value  
std(): Standard deviation    

## Explanation of variable naming

Variable names are based on combining the feature vector pattern names with the estimed variable name (ie mean or std in final data set) and if appropriate the dimension (X, Y, Z).

Variable naming in the tidy data set is derived from the variable naming used in the original dataset (which are readable and understandable based on the above explanation) with formatting to make them valid names in R (ie removing special characters notably parantheses and dashes).

For the final dataset there are therefore 68 variable name combinations:

Two of the variable names are purely descriptive of the source of the observation:

"subjectID" is an integer identifying the subject who carried out the corresponding activity and from which the corresponding measurements were taken (range from 1 to 30)

"activity" is the nature of the activity undertaken against which the measurements have been taken, there are 6 of these:  
"LAYING"  
"SITTING"  
"STANDING"  
"WALKING"           
"WALKING_DOWNSTAIRS"  
"WALKING_UPSTAIRS"

The remaining 66 variable names reflect the pattern "featurevectorpattern"."estimatedvariable"."dimension"

where the feature vector pattern and estimated variable are as per the lists above and the dimension is coded as follows:

X as "...X"
Y as "...Y"
Z as "...Z"
None as ".."

for example: 

"tBodyAcc.mean...X"  is the feature vector pattern tBodyAcc and this is the estimated mean for the X dimension.

This naming scheme enables the use and modification of the original coding book without major rework, while remaining intelligible to anyone already familiar with the naming of the original data set.

The dot pattern in the dimmension component of the variable could have been simplified to remove 2 of the 3 dots, this was not done for two reasons:
1) When reviewing a list of the variable names the visibility of the dimension and readability of the name is much clearer because of the additional spacing provided by these additional dots 

2) The generation/ translation of variable names follows a standard pattern for all variable names from the original dataset, which is completed by the make.names function in R.  This makes it a simple reproducible process that will work for any subsequent datasets that may incorporate additional names/ dimensions without any subjectivity and resulting differences in comparability between users.


The 66 data variables reflect the following combinations

8 feature vector patterns with one of three possible dimensions (X, Y, Z) ie giving 24 unique patterns  
9 feature vector patterns without any dimension 

giving a total of 33 unique feature vector patterns, for each of which there is a "mean" or "std" measure, for a total of 66 data variables 


## Summaries calculated

The final data set produced calculates a mean value for each of the 66 data variables (as described above and listed in the final section below) for each unique combination of activity and subject ID.  This gives a resulting data set of dimensions:  
180 rows (observations) being 6 activity measures for each of which there are 30 subject IDs leading to 180 unique combinations.  
68 fields being the 66 data variables plus subject ID and activty columns.
Units of measurement were unchanged from the original dataset.  No additional normalisation or transformation of data values was carried out other than the calculation of means as a summary value and as described above.

## Transformation and clean up work on the data

The README.MD file corresponding to this codebook provides explanation of the script and rationale for the way it operates.  The explanation for the variable naming has been covered above in this codebook.  This transformation and tidy up used the R make.names function.  

The "reshape2" library was used for melting and casting the data frame for the final data set.

More details are in the README file.


## Full variables list prior to restricting to mean and standard deviation

"subjectID"  
"activity"  
"tBodyAcc.mean...X"    
"tBodyAcc.mean...Y"  
"tBodyAcc.mean...Z"  
"tBodyAcc.std...X"  
"tBodyAcc.std...Y"  
"tBodyAcc.std...Z"  
"tBodyAcc.mad...X"  
"tBodyAcc.mad...Y"  
"tBodyAcc.mad...Z"  
"tBodyAcc.max...X"  
"tBodyAcc.max...Y"  
"tBodyAcc.max...Z"  
"tBodyAcc.min...X"  
"tBodyAcc.min...Y"  
"tBodyAcc.min...Z"  
"tBodyAcc.sma.."  
"tBodyAcc.energy...X"  
"tBodyAcc.energy...Y"  
"tBodyAcc.energy...Z"  
"tBodyAcc.iqr...X"  
"tBodyAcc.iqr...Y"  
"tBodyAcc.iqr...Z"  
"tBodyAcc.entropy...X"  
"tBodyAcc.entropy...Y"  
"tBodyAcc.entropy...Z"  
"tBodyAcc.arCoeff...X.1"  
"tBodyAcc.arCoeff...X.2"  
"tBodyAcc.arCoeff...X.3"  
"tBodyAcc.arCoeff...X.4"  
"tBodyAcc.arCoeff...Y.1"  
"tBodyAcc.arCoeff...Y.2"  
"tBodyAcc.arCoeff...Y.3"  
"tBodyAcc.arCoeff...Y.4"  
"tBodyAcc.arCoeff...Z.1"  
"tBodyAcc.arCoeff...Z.2"  
"tBodyAcc.arCoeff...Z.3"  
"tBodyAcc.arCoeff...Z.4"  
"tBodyAcc.correlation...X.Y"  
"tBodyAcc.correlation...X.Z"  
"tBodyAcc.correlation...Y.Z"  
"tGravityAcc.mean...X"  
"tGravityAcc.mean...Y"  
"tGravityAcc.mean...Z"  
"tGravityAcc.std...X"  
"tGravityAcc.std...Y"  
"tGravityAcc.std...Z"  
"tGravityAcc.mad...X"  
"tGravityAcc.mad...Y"  
"tGravityAcc.mad...Z"  
"tGravityAcc.max...X"  
"tGravityAcc.max...Y"  
"tGravityAcc.max...Z"  
"tGravityAcc.min...X"  
"tGravityAcc.min...Y"  
"tGravityAcc.min...Z"  
"tGravityAcc.sma.."  
"tGravityAcc.energy...X"  
"tGravityAcc.energy...Y"  
"tGravityAcc.energy...Z"  
"tGravityAcc.iqr...X"  
"tGravityAcc.iqr...Y"  
"tGravityAcc.iqr...Z"  
"tGravityAcc.entropy...X"  
"tGravityAcc.entropy...Y"  
"tGravityAcc.entropy...Z"  
"tGravityAcc.arCoeff...X.1"  
"tGravityAcc.arCoeff...X.2"  
"tGravityAcc.arCoeff...X.3"  
"tGravityAcc.arCoeff...X.4"  
"tGravityAcc.arCoeff...Y.1"  
"tGravityAcc.arCoeff...Y.2"  
"tGravityAcc.arCoeff...Y.3"  
"tGravityAcc.arCoeff...Y.4"  
"tGravityAcc.arCoeff...Z.1"  
"tGravityAcc.arCoeff...Z.2"  
"tGravityAcc.arCoeff...Z.3"  
"tGravityAcc.arCoeff...Z.4"  
"tGravityAcc.correlation...X.Y"  
"tGravityAcc.correlation...X.Z"  
"tGravityAcc.correlation...Y.Z"  
"tBodyAccJerk.mean...X"  
"tBodyAccJerk.mean...Y"  
"tBodyAccJerk.mean...Z"  
"tBodyAccJerk.std...X"  
"tBodyAccJerk.std...Y"  
"tBodyAccJerk.std...Z"  
"tBodyAccJerk.mad...X"  
"tBodyAccJerk.mad...Y"  
"tBodyAccJerk.mad...Z"  
"tBodyAccJerk.max...X"  
"tBodyAccJerk.max...Y"  
"tBodyAccJerk.max...Z"  
"tBodyAccJerk.min...X"  
"tBodyAccJerk.min...Y"  
"tBodyAccJerk.min...Z"  
"tBodyAccJerk.sma.."  
"tBodyAccJerk.energy...X"  
"tBodyAccJerk.energy...Y"  
"tBodyAccJerk.energy...Z"  
"tBodyAccJerk.iqr...X"  
"tBodyAccJerk.iqr...Y"  
"tBodyAccJerk.iqr...Z"  
"tBodyAccJerk.entropy...X"  
"tBodyAccJerk.entropy...Y"  
"tBodyAccJerk.entropy...Z"  
"tBodyAccJerk.arCoeff...X.1"  
"tBodyAccJerk.arCoeff...X.2"  
"tBodyAccJerk.arCoeff...X.3"  
"tBodyAccJerk.arCoeff...X.4"  
"tBodyAccJerk.arCoeff...Y.1"  
"tBodyAccJerk.arCoeff...Y.2"  
"tBodyAccJerk.arCoeff...Y.3"  
"tBodyAccJerk.arCoeff...Y.4"  
"tBodyAccJerk.arCoeff...Z.1"  
"tBodyAccJerk.arCoeff...Z.2"  
"tBodyAccJerk.arCoeff...Z.3"  
"tBodyAccJerk.arCoeff...Z.4"  
"tBodyAccJerk.correlation...X.Y"  
"tBodyAccJerk.correlation...X.Z"  
"tBodyAccJerk.correlation...Y.Z"  
"tBodyGyro.mean...X"  
"tBodyGyro.mean...Y"  
"tBodyGyro.mean...Z"  
"tBodyGyro.std...X"  
"tBodyGyro.std...Y"  
"tBodyGyro.std...Z"  
"tBodyGyro.mad...X"  
"tBodyGyro.mad...Y"  
"tBodyGyro.mad...Z"  
"tBodyGyro.max...X"  
"tBodyGyro.max...Y"  
"tBodyGyro.max...Z"  
"tBodyGyro.min...X"  
"tBodyGyro.min...Y"  
"tBodyGyro.min...Z"  
"tBodyGyro.sma.."  
"tBodyGyro.energy...X"  
"tBodyGyro.energy...Y"  
"tBodyGyro.energy...Z"  
"tBodyGyro.iqr...X"  
"tBodyGyro.iqr...Y"  
"tBodyGyro.iqr...Z"  
"tBodyGyro.entropy...X"  
"tBodyGyro.entropy...Y"  
"tBodyGyro.entropy...Z"  
"tBodyGyro.arCoeff...X.1"  
"tBodyGyro.arCoeff...X.2"  
"tBodyGyro.arCoeff...X.3"  
"tBodyGyro.arCoeff...X.4"  
"tBodyGyro.arCoeff...Y.1"  
"tBodyGyro.arCoeff...Y.2"  
"tBodyGyro.arCoeff...Y.3"  
"tBodyGyro.arCoeff...Y.4"  
"tBodyGyro.arCoeff...Z.1"  
"tBodyGyro.arCoeff...Z.2"  
"tBodyGyro.arCoeff...Z.3"  
"tBodyGyro.arCoeff...Z.4"  
"tBodyGyro.correlation...X.Y"  
"tBodyGyro.correlation...X.Z"  
"tBodyGyro.correlation...Y.Z"  
"tBodyGyroJerk.mean...X"  
"tBodyGyroJerk.mean...Y"  
"tBodyGyroJerk.mean...Z"  
"tBodyGyroJerk.std...X"  
"tBodyGyroJerk.std...Y"  
"tBodyGyroJerk.std...Z"  
"tBodyGyroJerk.mad...X"  
"tBodyGyroJerk.mad...Y"  
"tBodyGyroJerk.mad...Z"  
"tBodyGyroJerk.max...X"  
"tBodyGyroJerk.max...Y"  
"tBodyGyroJerk.max...Z"  
"tBodyGyroJerk.min...X"  
"tBodyGyroJerk.min...Y"  
"tBodyGyroJerk.min...Z"  
"tBodyGyroJerk.sma.."  
"tBodyGyroJerk.energy...X"  
"tBodyGyroJerk.energy...Y"  
"tBodyGyroJerk.energy...Z"  
"tBodyGyroJerk.iqr...X"  
"tBodyGyroJerk.iqr...Y"  
"tBodyGyroJerk.iqr...Z"  
"tBodyGyroJerk.entropy...X"  
"tBodyGyroJerk.entropy...Y"  
"tBodyGyroJerk.entropy...Z"  
"tBodyGyroJerk.arCoeff...X.1"  
"tBodyGyroJerk.arCoeff...X.2"  
"tBodyGyroJerk.arCoeff...X.3"  
"tBodyGyroJerk.arCoeff...X.4"  
"tBodyGyroJerk.arCoeff...Y.1"  
"tBodyGyroJerk.arCoeff...Y.2"  
"tBodyGyroJerk.arCoeff...Y.3"  
"tBodyGyroJerk.arCoeff...Y.4"  
"tBodyGyroJerk.arCoeff...Z.1"  
"tBodyGyroJerk.arCoeff...Z.2"  
"tBodyGyroJerk.arCoeff...Z.3"  
"tBodyGyroJerk.arCoeff...Z.4"  
"tBodyGyroJerk.correlation...X.Y"  
"tBodyGyroJerk.correlation...X.Z"  
"tBodyGyroJerk.correlation...Y.Z"  
"tBodyAccMag.mean.."  
"tBodyAccMag.std.."  
"tBodyAccMag.mad.."  
"tBodyAccMag.max.."  
"tBodyAccMag.min.."  
"tBodyAccMag.sma.."  
"tBodyAccMag.energy.."  
"tBodyAccMag.iqr.."  
"tBodyAccMag.entropy.."  
"tBodyAccMag.arCoeff..1"  
"tBodyAccMag.arCoeff..2"  
"tBodyAccMag.arCoeff..3"  
"tBodyAccMag.arCoeff..4"  
"tGravityAccMag.mean.."  
"tGravityAccMag.std.."  
"tGravityAccMag.mad.."  
"tGravityAccMag.max.."  
"tGravityAccMag.min.."  
"tGravityAccMag.sma.."  
"tGravityAccMag.energy.."  
"tGravityAccMag.iqr.."  
"tGravityAccMag.entropy.."  
"tGravityAccMag.arCoeff..1"  
"tGravityAccMag.arCoeff..2"  
"tGravityAccMag.arCoeff..3"  
"tGravityAccMag.arCoeff..4"  
"tBodyAccJerkMag.mean.."  
"tBodyAccJerkMag.std.."  
"tBodyAccJerkMag.mad.."  
"tBodyAccJerkMag.max.."  
"tBodyAccJerkMag.min.."  
"tBodyAccJerkMag.sma.."  
"tBodyAccJerkMag.energy.."  
"tBodyAccJerkMag.iqr.."  
"tBodyAccJerkMag.entropy.."  
"tBodyAccJerkMag.arCoeff..1"  
"tBodyAccJerkMag.arCoeff..2"  
"tBodyAccJerkMag.arCoeff..3"  
"tBodyAccJerkMag.arCoeff..4"  
"tBodyGyroMag.mean.."  
"tBodyGyroMag.std.."  
"tBodyGyroMag.mad.."  
"tBodyGyroMag.max.."  
"tBodyGyroMag.min.."  
"tBodyGyroMag.sma.."  
"tBodyGyroMag.energy.."  
"tBodyGyroMag.iqr.."  
"tBodyGyroMag.entropy.."  
"tBodyGyroMag.arCoeff..1"  
"tBodyGyroMag.arCoeff..2"  
"tBodyGyroMag.arCoeff..3"  
"tBodyGyroMag.arCoeff..4"  
"tBodyGyroJerkMag.mean.."  
"tBodyGyroJerkMag.std.."  
"tBodyGyroJerkMag.mad.."  
"tBodyGyroJerkMag.max.."  
"tBodyGyroJerkMag.min.."  
"tBodyGyroJerkMag.sma.."  
"tBodyGyroJerkMag.energy.."    
"tBodyGyroJerkMag.iqr.."  
"tBodyGyroJerkMag.entropy.."  
"tBodyGyroJerkMag.arCoeff..1"  
"tBodyGyroJerkMag.arCoeff..2"  
"tBodyGyroJerkMag.arCoeff..3"  
"tBodyGyroJerkMag.arCoeff..4"  
"fBodyAcc.mean...X"  
"fBodyAcc.mean...Y"  
"fBodyAcc.mean...Z"  
"fBodyAcc.std...X"  
"fBodyAcc.std...Y"  
"fBodyAcc.std...Z"  
"fBodyAcc.mad...X"  
"fBodyAcc.mad...Y"  
"fBodyAcc.mad...Z"  
"fBodyAcc.max...X"  
"fBodyAcc.max...Y"  
"fBodyAcc.max...Z"  
"fBodyAcc.min...X"  
"fBodyAcc.min...Y"  
"fBodyAcc.min...Z"  
"fBodyAcc.sma.."  
"fBodyAcc.energy...X"  
"fBodyAcc.energy...Y"  
"fBodyAcc.energy...Z"  
"fBodyAcc.iqr...X"  
"fBodyAcc.iqr...Y"  
"fBodyAcc.iqr...Z"  
"fBodyAcc.entropy...X"  
"fBodyAcc.entropy...Y"  
"fBodyAcc.entropy...Z"  
"fBodyAcc.maxInds.X"  
"fBodyAcc.maxInds.Y"  
"fBodyAcc.maxInds.Z"  
"fBodyAcc.meanFreq...X"  
"fBodyAcc.meanFreq...Y"  
"fBodyAcc.meanFreq...Z"  
"fBodyAcc.skewness...X"  
"fBodyAcc.kurtosis...X"  
"fBodyAcc.skewness...Y"  
"fBodyAcc.kurtosis...Y"  
"fBodyAcc.skewness...Z"  
"fBodyAcc.kurtosis...Z"  
"fBodyAcc.bandsEnergy...1.8"  
"fBodyAcc.bandsEnergy...9.16"  
"fBodyAcc.bandsEnergy...17.24"  
"fBodyAcc.bandsEnergy...25.32"  
"fBodyAcc.bandsEnergy...33.40"  
"fBodyAcc.bandsEnergy...41.48"  
"fBodyAcc.bandsEnergy...49.56"  
"fBodyAcc.bandsEnergy...57.64"  
"fBodyAcc.bandsEnergy...1.16"  
"fBodyAcc.bandsEnergy...17.32"  
"fBodyAcc.bandsEnergy...33.48"  
"fBodyAcc.bandsEnergy...49.64"  
"fBodyAcc.bandsEnergy...1.24"  
"fBodyAcc.bandsEnergy...25.48"  
"fBodyAcc.bandsEnergy...1.8"  
"fBodyAcc.bandsEnergy...9.16"  
"fBodyAcc.bandsEnergy...17.24"  
"fBodyAcc.bandsEnergy...25.32"  
"fBodyAcc.bandsEnergy...33.40"  
"fBodyAcc.bandsEnergy...41.48"  
"fBodyAcc.bandsEnergy...49.56"  
"fBodyAcc.bandsEnergy...57.64"  
"fBodyAcc.bandsEnergy...1.16"  
"fBodyAcc.bandsEnergy...17.32"  
"fBodyAcc.bandsEnergy...33.48"  
"fBodyAcc.bandsEnergy...49.64"  
"fBodyAcc.bandsEnergy...1.24"  
"fBodyAcc.bandsEnergy...25.48"  
"fBodyAcc.bandsEnergy...1.8"  
"fBodyAcc.bandsEnergy...9.16"  
"fBodyAcc.bandsEnergy...17.24"  
"fBodyAcc.bandsEnergy...25.32"  
"fBodyAcc.bandsEnergy...33.40"  
"fBodyAcc.bandsEnergy...41.48"  
"fBodyAcc.bandsEnergy...49.56"  
"fBodyAcc.bandsEnergy...57.64"  
"fBodyAcc.bandsEnergy...1.16"  
"fBodyAcc.bandsEnergy...17.32"  
"fBodyAcc.bandsEnergy...33.48"  
"fBodyAcc.bandsEnergy...49.64"  
"fBodyAcc.bandsEnergy...1.24"  
"fBodyAcc.bandsEnergy...25.48"  
"fBodyAccJerk.mean...X"  
"fBodyAccJerk.mean...Y"  
"fBodyAccJerk.mean...Z"  
"fBodyAccJerk.std...X"  
"fBodyAccJerk.std...Y"  
"fBodyAccJerk.std...Z"  
"fBodyAccJerk.mad...X"  
"fBodyAccJerk.mad...Y"  
"fBodyAccJerk.mad...Z"  
"fBodyAccJerk.max...X"  
"fBodyAccJerk.max...Y"  
"fBodyAccJerk.max...Z"  
"fBodyAccJerk.min...X"  
"fBodyAccJerk.min...Y"  
"fBodyAccJerk.min...Z"  
"fBodyAccJerk.sma.."  
"fBodyAccJerk.energy...X"  
"fBodyAccJerk.energy...Y"  
"fBodyAccJerk.energy...Z"  
"fBodyAccJerk.iqr...X"  
"fBodyAccJerk.iqr...Y"  
"fBodyAccJerk.iqr...Z"  
"fBodyAccJerk.entropy...X"  
"fBodyAccJerk.entropy...Y"  
"fBodyAccJerk.entropy...Z"  
"fBodyAccJerk.maxInds.X"  
"fBodyAccJerk.maxInds.Y"  
"fBodyAccJerk.maxInds.Z"  
"fBodyAccJerk.meanFreq...X"  
"fBodyAccJerk.meanFreq...Y"  
"fBodyAccJerk.meanFreq...Z"  
"fBodyAccJerk.skewness...X"  
"fBodyAccJerk.kurtosis...X"  
"fBodyAccJerk.skewness...Y"  
"fBodyAccJerk.kurtosis...Y"  
"fBodyAccJerk.skewness...Z"  
"fBodyAccJerk.kurtosis...Z"  
"fBodyAccJerk.bandsEnergy...1.8"  
"fBodyAccJerk.bandsEnergy...9.16"  
"fBodyAccJerk.bandsEnergy...17.24"  
"fBodyAccJerk.bandsEnergy...25.32"  
"fBodyAccJerk.bandsEnergy...33.40"  
"fBodyAccJerk.bandsEnergy...41.48"  
"fBodyAccJerk.bandsEnergy...49.56"  
"fBodyAccJerk.bandsEnergy...57.64"  
"fBodyAccJerk.bandsEnergy...1.16"  
"fBodyAccJerk.bandsEnergy...17.32"  
"fBodyAccJerk.bandsEnergy...33.48"  
"fBodyAccJerk.bandsEnergy...49.64"  
"fBodyAccJerk.bandsEnergy...1.24"  
"fBodyAccJerk.bandsEnergy...25.48"  
"fBodyAccJerk.bandsEnergy...1.8"  
"fBodyAccJerk.bandsEnergy...9.16"  
"fBodyAccJerk.bandsEnergy...17.24"  
"fBodyAccJerk.bandsEnergy...25.32"  
"fBodyAccJerk.bandsEnergy...33.40"  
"fBodyAccJerk.bandsEnergy...41.48"  
"fBodyAccJerk.bandsEnergy...49.56"  
"fBodyAccJerk.bandsEnergy...57.64"  
"fBodyAccJerk.bandsEnergy...1.16"  
"fBodyAccJerk.bandsEnergy...17.32"  
"fBodyAccJerk.bandsEnergy...33.48"  
"fBodyAccJerk.bandsEnergy...49.64"  
"fBodyAccJerk.bandsEnergy...1.24"  
"fBodyAccJerk.bandsEnergy...25.48"  
"fBodyAccJerk.bandsEnergy...1.8"  
"fBodyAccJerk.bandsEnergy...9.16"  
"fBodyAccJerk.bandsEnergy...17.24"  
"fBodyAccJerk.bandsEnergy...25.32"  
"fBodyAccJerk.bandsEnergy...33.40"  
"fBodyAccJerk.bandsEnergy...41.48"  
"fBodyAccJerk.bandsEnergy...49.56"  
"fBodyAccJerk.bandsEnergy...57.64"  
"fBodyAccJerk.bandsEnergy...1.16"  
"fBodyAccJerk.bandsEnergy...17.32"  
"fBodyAccJerk.bandsEnergy...33.48"  
"fBodyAccJerk.bandsEnergy...49.64"  
"fBodyAccJerk.bandsEnergy...1.24"  
"fBodyAccJerk.bandsEnergy...25.48"  
"fBodyGyro.mean...X"  
"fBodyGyro.mean...Y"  
"fBodyGyro.mean...Z"  
"fBodyGyro.std...X"  
"fBodyGyro.std...Y"  
"fBodyGyro.std...Z"  
"fBodyGyro.mad...X"  
"fBodyGyro.mad...Y"  
"fBodyGyro.mad...Z"  
"fBodyGyro.max...X"  
"fBodyGyro.max...Y"  
"fBodyGyro.max...Z"  
"fBodyGyro.min...X"  
"fBodyGyro.min...Y"  
"fBodyGyro.min...Z"  
"fBodyGyro.sma.."  
"fBodyGyro.energy...X"  
"fBodyGyro.energy...Y"  
"fBodyGyro.energy...Z"  
"fBodyGyro.iqr...X"  
"fBodyGyro.iqr...Y"  
"fBodyGyro.iqr...Z"  
"fBodyGyro.entropy...X"  
"fBodyGyro.entropy...Y"  
"fBodyGyro.entropy...Z"  
"fBodyGyro.maxInds.X"  
"fBodyGyro.maxInds.Y"  
"fBodyGyro.maxInds.Z"  
"fBodyGyro.meanFreq...X"  
"fBodyGyro.meanFreq...Y"  
"fBodyGyro.meanFreq...Z"  
"fBodyGyro.skewness...X"  
"fBodyGyro.kurtosis...X"  
"fBodyGyro.skewness...Y"  
"fBodyGyro.kurtosis...Y"  
"fBodyGyro.skewness...Z"  
"fBodyGyro.kurtosis...Z"  
"fBodyGyro.bandsEnergy...1.8"  
"fBodyGyro.bandsEnergy...9.16"  
"fBodyGyro.bandsEnergy...17.24"   
"fBodyGyro.bandsEnergy...25.32"  
"fBodyGyro.bandsEnergy...33.40"  
"fBodyGyro.bandsEnergy...41.48"  
"fBodyGyro.bandsEnergy...49.56"  
"fBodyGyro.bandsEnergy...57.64"  
"fBodyGyro.bandsEnergy...1.16"  
"fBodyGyro.bandsEnergy...17.32"  
"fBodyGyro.bandsEnergy...33.48"  
"fBodyGyro.bandsEnergy...49.64"  
"fBodyGyro.bandsEnergy...1.24"  
"fBodyGyro.bandsEnergy...25.48"  
"fBodyGyro.bandsEnergy...1.8"  
"fBodyGyro.bandsEnergy...9.16"  
"fBodyGyro.bandsEnergy...17.24"  
"fBodyGyro.bandsEnergy...25.32"  
"fBodyGyro.bandsEnergy...33.40"  
"fBodyGyro.bandsEnergy...41.48"  
"fBodyGyro.bandsEnergy...49.56"  
"fBodyGyro.bandsEnergy...57.64"  
"fBodyGyro.bandsEnergy...1.16"  
"fBodyGyro.bandsEnergy...17.32"  
"fBodyGyro.bandsEnergy...33.48"  
"fBodyGyro.bandsEnergy...49.64"  
"fBodyGyro.bandsEnergy...1.24"  
"fBodyGyro.bandsEnergy...25.48"  
"fBodyGyro.bandsEnergy...1.8"  
"fBodyGyro.bandsEnergy...9.16"  
"fBodyGyro.bandsEnergy...17.24"  
"fBodyGyro.bandsEnergy...25.32"  
"fBodyGyro.bandsEnergy...33.40"  
"fBodyGyro.bandsEnergy...41.48"  
"fBodyGyro.bandsEnergy...49.56"  
"fBodyGyro.bandsEnergy...57.64"  
"fBodyGyro.bandsEnergy...1.16"  
"fBodyGyro.bandsEnergy...17.32"  
"fBodyGyro.bandsEnergy...33.48"  
"fBodyGyro.bandsEnergy...49.64"  
"fBodyGyro.bandsEnergy...1.24"  
"fBodyGyro.bandsEnergy...25.48"  
"fBodyAccMag.mean.."  
"fBodyAccMag.std.."  
"fBodyAccMag.mad.."  
"fBodyAccMag.max.."  
"fBodyAccMag.min.."  
"fBodyAccMag.sma.."  
"fBodyAccMag.energy.."  
"fBodyAccMag.iqr.."  
"fBodyAccMag.entropy.."  
"fBodyAccMag.maxInds"  
"fBodyAccMag.meanFreq.."  
"fBodyAccMag.skewness.."  
"fBodyAccMag.kurtosis.."  
"fBodyBodyAccJerkMag.mean.."  
"fBodyBodyAccJerkMag.std.."  
"fBodyBodyAccJerkMag.mad.."  
"fBodyBodyAccJerkMag.max.."  
"fBodyBodyAccJerkMag.min.."  
"fBodyBodyAccJerkMag.sma.."  
"fBodyBodyAccJerkMag.energy.."  
"fBodyBodyAccJerkMag.iqr.."  
"fBodyBodyAccJerkMag.entropy.."  
"fBodyBodyAccJerkMag.maxInds"  
"fBodyBodyAccJerkMag.meanFreq.."  
"fBodyBodyAccJerkMag.skewness.."  
"fBodyBodyAccJerkMag.kurtosis.."  
"fBodyBodyGyroMag.mean.."  
"fBodyBodyGyroMag.std.."  
"fBodyBodyGyroMag.mad.."  
"fBodyBodyGyroMag.max.."  
"fBodyBodyGyroMag.min.."  
"fBodyBodyGyroMag.sma.."  
"fBodyBodyGyroMag.energy.."  
"fBodyBodyGyroMag.iqr.."  
"fBodyBodyGyroMag.entropy.."  
"fBodyBodyGyroMag.maxInds"  
"fBodyBodyGyroMag.meanFreq.."  
"fBodyBodyGyroMag.skewness.."  
"fBodyBodyGyroMag.kurtosis.."  
"fBodyBodyGyroJerkMag.mean.."  
"fBodyBodyGyroJerkMag.std.."  
"fBodyBodyGyroJerkMag.mad.."  
"fBodyBodyGyroJerkMag.max.."  
"fBodyBodyGyroJerkMag.min.."  
"fBodyBodyGyroJerkMag.sma.."  
"fBodyBodyGyroJerkMag.energy.."  
"fBodyBodyGyroJerkMag.iqr.."  
"fBodyBodyGyroJerkMag.entropy.."  
"fBodyBodyGyroJerkMag.maxInds"  
"fBodyBodyGyroJerkMag.meanFreq.."  
"fBodyBodyGyroJerkMag.skewness.."  
"fBodyBodyGyroJerkMag.kurtosis.."  
"angle.tBodyAccMean.gravity."  
"angle.tBodyAccJerkMean..gravityMean."  
"angle.tBodyGyroMean.gravityMean."  
"angle.tBodyGyroJerkMean.gravityMean."  
"angle.X.gravityMean."  
"angle.Y.gravityMean."  
"angle.Z.gravityMean."  

## Subset of variables after restriction to mean and standard deviation

"activity"  
"subjectID"  
"tBodyAcc.mean...X"  
"tBodyAcc.mean...Y"  
"tBodyAcc.mean...Z"  
"tBodyAcc.std...X"  
"tBodyAcc.std...Y"  
"tBodyAcc.std...Z"  
"tGravityAcc.mean...X"  
"tGravityAcc.mean...Y"  
"tGravityAcc.mean...Z"  
"tGravityAcc.std...X"  
"tGravityAcc.std...Y"  
"tGravityAcc.std...Z"  
"tBodyAccJerk.mean...X"  
"tBodyAccJerk.mean...Y"  
"tBodyAccJerk.mean...Z"  
"tBodyAccJerk.std...X"  
"tBodyAccJerk.std...Y"  
"tBodyAccJerk.std...Z"  
"tBodyGyro.mean...X"  
"tBodyGyro.mean...Y"  
"tBodyGyro.mean...Z"  
"tBodyGyro.std...X"  
"tBodyGyro.std...Y"  
"tBodyGyro.std...Z"  
"tBodyGyroJerk.mean...X"  
"tBodyGyroJerk.mean...Y"  
"tBodyGyroJerk.mean...Z"  
"tBodyGyroJerk.std...X"  
"tBodyGyroJerk.std...Y"  
"tBodyGyroJerk.std...Z"  
"tBodyAccMag.mean.."  
"tBodyAccMag.std.."  
"tGravityAccMag.mean.."  
"tGravityAccMag.std.."  
"tBodyAccJerkMag.mean.."  
"tBodyAccJerkMag.std.."  
"tBodyGyroMag.mean.."  
"tBodyGyroMag.std.."  
"tBodyGyroJerkMag.mean.."  
"tBodyGyroJerkMag.std.."  
"fBodyAcc.mean...X"  
"fBodyAcc.mean...Y"  
"fBodyAcc.mean...Z"  
"fBodyAcc.std...X"  
"fBodyAcc.std...Y"  
"fBodyAcc.std...Z"  
"fBodyAccJerk.mean...X"  
"fBodyAccJerk.mean...Y"  
"fBodyAccJerk.mean...Z"  
"fBodyAccJerk.std...X"  
"fBodyAccJerk.std...Y"  
"fBodyAccJerk.std...Z"  
"fBodyGyro.mean...X"  
"fBodyGyro.mean...Y"  
"fBodyGyro.mean...Z"  
"fBodyGyro.std...X"  
"fBodyGyro.std...Y"  
"fBodyGyro.std...Z"  
"fBodyAccMag.mean.."  
"fBodyAccMag.std.."  
"fBodyBodyAccJerkMag.mean.."  
"fBodyBodyAccJerkMag.std.."  
"fBodyBodyGyroMag.mean.."  
"fBodyBodyGyroMag.std.."  
"fBodyBodyGyroJerkMag.mean.."  
"fBodyBodyGyroJerkMag.std.."  