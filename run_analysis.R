#  Merges the training and the test sets to create one data set.
#  Extracts only the measurements on the mean and standard deviation for each measurement. 
#  Uses descriptive activity names to name the activities in the data set
#  Appropriately labels the data set with descriptive variable names. 
#  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



setwd("C:/Users/Phillip/Desktop/Coursera/Data Clean")

if (!file.exists("dataproject1")) {
        dir.create("dataproject1")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./dataproject1/downloaded.zip") 
dateDownloaded <-date()
unzip (zipfile="./dataproject1/downloaded.zip",exdir="./dataproject1")

setwd("C:/Users/Phillip/Desktop/Coursera/Data Clean/dataproject1/UCI HAR Dataset")



#Read all the relevant files 

activityType = read.table("./activity_labels.txt", header=FALSE)
features     = read.table("./features.txt", header=FALSE)

subjectTrain = read.table("./train/subject_train.txt", header=FALSE)
xTrain       = read.table("./train/X_train.txt", header=FALSE)
yTrain       = read.table("./train/y_train.txt", header=FALSE)

subjectTest  = read.table("./test/subject_test.txt", header=FALSE)
xTest        = read.table("./test/X_test.txt", header=FALSE)
yTest        = read.table("./test/y_test.txt", header=FALSE)

# Use summary function to check all the relevant data to 
# determine number of column variables & their data types

#Assign column names to the tables


colnames(activityType) = c('activity_id','activity_type');

colnames(subjectTrain) = "subject_id";
colnames(xTrain) = features[,2];
colnames(yTrain) = "activity_id";

colnames(subjectTest) = "subject_id";
colnames(xTest) = features[,2];
colnames(yTest) = "activity_id";


# Merge Train & Test data

trainBind <- cbind(yTrain,subjectTrain,xTrain)

testBind <- cbind(yTest,subjectTest,xTest)

finalData <- rbind(trainBind,testBind )

# Check the merged data

summary(finalData)
dim(finalData)
dim(testBind)



# Check for missing NA values
 chk <- any(is.na(finalData))
 chk

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

colAllNames <-colnames(finalData)
mean_and_std_features <- grep("-(mean|std)\\(\\)",colAllNames )

finalData2 <- finalData  
head(finalData2)

# 3. Uses descriptive activity names to name the activities in the data set



mergefinalData <- merge(finalData,activityType,by.x="activity_id", all.x=TRUE)

summary(mergefinalData)

colAllNames2 <-colnames(mergefinalData)

# 4.  Appropriately labels the data set with descriptive variable names. 
for (i in 1:length(colAllNames2))
{
        colAllNames2[i] <- gsub("\\()","",colAllNames2[i])
        colAllNames2[i] <- gsub("-std$","Std_Dev",colAllNames2[i])
        colAllNames2[i] <- gsub("-mean","Mean",colAllNames2[i])
        colAllNames2[i] <- gsub("-sma","Signal_Magnitude_Area",colAllNames2[i])
        colAllNames2[i] <- gsub("-iqr","Interquartile_Range",colAllNames2[i])
        colAllNames2[i] <- gsub("^(t)","time",colAllNames2[i])
        colAllNames2[i] <- gsub("^(f)","freq",colAllNames2[i])
        colAllNames2[i] <- gsub("([Gg]ravity)","Gravity",colAllNames2[i])
        colAllNames2[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colAllNames2[i])
        colAllNames2[i] <- gsub("[Gg]yro","Gyro",colAllNames2[i])
        colAllNames2[i] <- gsub("AccMag","Acc_Magnitude",colAllNames2[i])
        colAllNames2[i] <- gsub("([Bb]odyaccjerkmag)","Body_Acc_Jerk_Magnitude",colAllNames2[i])
        colAllNames2[i] <- gsub("JerkMag","Jerk_Magnitude",colAllNames2[i])
        colAllNames2[i] <- gsub("GyroMag","Gyro_Magnitude",colAllNames2[i])
}
colnames(mergefinalData) <- colAllNames2
# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for 
# each activity and each subject.

library(dplyr)

Data2<-aggregate(. ~subject_id + activity_id, mergefinalData, mean)
Data2<-Data2[order(Data2$subject_id,Data2$activity_id),]


write.table(Data2, './tidyData.txt',row.names=FALSE,sep='\t')










