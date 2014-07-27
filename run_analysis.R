##                            Project Instructions                           ##
# You should create one R script called run_analysis.R that does the following# 
# 1. Merges the training and the test sets to create one data set.            #
# 2. Extracts only the measurements on the mean and standard deviation for    #
#    each measurement.                                                        #
# 3. Uses descriptive activity names to name the activities in the data set   #
# 4. Appropriately labels the data set with descriptive variable names.       #
# 5. Creates a second, independent tidy data set with the average of each     #
#    variable for each activity and each subject.                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


#########################
# Preparing environment #
#########################
 # 1. set working directory
    setwd("~/Google Drive/Coursera/DataScience/03-GettingAndCleaningData/Workspace")
 # 2. check and install packages needed
  #Generic function to check if a package is already installed
    pkgTest <- function(x)
    {
      if (!require(x,character.only = TRUE))
      {
        install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
      }
      else
      {
        cat(sprintf("Package \"%s\" already installed",x))
      }
    }
  # Package(s) needed for this project
    pkgTest("data.table")
    pkgTest("reshape2")
    
    # 3. Get raw data ready to use
    zipFile <- "DataSet.zip"
    dataDir <- "UCI HAR Dataset"
    
    if (!file.exists(dataDir)) {
      if (!file.exists(zipFile)) {
        fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, zipFile)
      }
      
      if (!file.exists(dataDir)) {
        unzip(zipFile)
      }
    }
    
#########################
#     Project Code      #
#########################

# Step 1: Merge the training and the test sets to create one data set
    # 1. Load relevant data tables
      xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
      xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt") 
      subjectTest<- read.table("./UCI HAR Dataset/test/subject_test.txt") 
      subjectTrain<- read.table("./UCI HAR Dataset/train/subject_train.txt") 
      ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt") 
      ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
    
    # 2. Rename subject table field to avoid duplicate field names 
      names(subjectTest) <- "subjectID"
      names(subjectTrain) <- "subjectID"
    # 3. Rename label field
      names(ytrain) <- "activity"
      names(ytest) <- "activity"
    
    # 4. Merge datatables
      # Bind subjects to activities to data sets
        testTable <- cbind(subjectTest, ytest, xtest)
        trainTable <- cbind(subjectTrain, ytrain, xtrain)
      # Bind train and test data sets
        data <- rbind(trainTable, testTable)

# Step 2: Appropriately labels the data set with descriptive variable names. 
    
    # 1.Load the table of features
      features <- read.table("./UCI HAR Dataset/features.txt")
      features <-features[,-1]
      # Create valid R names from the activity names provided
        validFeatures <- sapply(features, make.names, unique = TRUE) 
      # Create ndata to hold new data and names        
        ndata <- data 
      # Replace the names for ndata with the valid names
        names(ndata)[3:563] <- validFeatures 

# Step 3: Extracts only the measurements on the mean and standard deviation for each measurement. 
    # 1. Find initial list of columns needed
      cols <- grep("subjectID|activity|mean|std", names(ndata))
    # 2. Create subset of ndata table with required columns
      shortdata <- ndata[,cols] ## subsetting ndata table by the required columns
    # 3. Remove meanFreq entries found in names(shortdata)
      # Find columns where mean is meanFreq to be excluded
        excl <-grep("meanFreq", names(shortdata)) 
      # Remove columns to be excluded
        shortdata <- shortdata[,-excl] 

# Step 4: Use descriptive activity names to name the activities in the data set
    # 1. Replace activity ids with activity name
        activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
      # Merge activity labels table against datatable
        labelData <- merge(shortdata, activityLabels, by.x="activity", by.y="V1", all=TRUE) 
        labelData$activity <- labelData$V2
      # Remove the V2 column now
        labelData[,"V2"]  <- NULL 

# Step 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
    # 1. Melt the data set
      melted <- melt(labelData, id.vars=c("subjectID", "activity"), measure.vars = names(labelData[,3:68]))
    # 2. Cast a table 
      tidyData <- dcast(melted, activity + subjectID ~ variable, mean)
    # 3.  write the table as a text file 
      write.table(tidyData, "./UCI HAR Dataset/tidydata.txt", row.names = FALSE)












