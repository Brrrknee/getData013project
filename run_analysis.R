## Clean Data Project script
##  1)  Merges the training and the test sets to create one data set.
##        these are:
##          ./UCI HAR Dataset/train/X_train.txt
##          ./UCI HAR Dataset/test/X_test.txt

##  2)  Extracts only the measurements on the mean and standard deviation for 
##      each measurement. { mean() and std() in the following columns:
##      <2,4>    TIME:   tBodyAcc-XYZ                1:6 (1-3, mean-X(),-Y(),-Z())
##                                                    (4-6, std-X(),,-Y(),-Z())
##                     tGravityAcc-XYZ           41:46
##                     tBodyAccJerk-XYZ          81:86
##                     tBodyGyro-XYZ             121:126   
##                     tBodyGyroJerk-XYZ          161:166
##                     tBodyAccMag                201 mean() & 202 std()
##                     tGravityAccMag             214 & 215
##                     tBodyAccJerkMag            227 & 228
##                     tBodyGyroMag               240 & 241
##                     tBodyGyroJerkMag           253 & 254
##               Freq: fBodyAcc-XYZ               266:271
##                     fBodyAccJerk-XYZ           345:350
##                     fBodyGyro-XYZ              424:429
##                     fBodyAccMag                503 & 504
##                     fBodyAccJerkMag            516 & 517
##                     fBodyGyroMag               529 & 530
##                     fBodyGyroJerkMag           542 & 543
## }
##
##  3)  Uses descriptive activity names to name the activities in the data set
##          >Combine the y_train.txt/ y_test.txt activity by-record indexed value from 
##            acivity_labels.txt as the values of the $activity factor variable.
##          >Also combine in the subject_test.txt/ subject_train.txt by-record
##            to have complete dataset view.
##  4)  Appropriately labels the data set with descriptive variable names. 
##          Named as above in 2
##  5)  From the data set in step 4, 
##      creates a second, independent tidy data set with 
##          >the average of each variable 
##              >for each activity and 
##              >for each subject.
##      >>>melt to drive 66 columns to 2, add in subject and activity for total of 4
##      <5> > Subject:  id              integer
##      <3> > Activity: [WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS,
##                      SITTING, STANDING, LAYING]  (factor)
## ------------------------------------------------------------------------------------------------
## ------the Following is what the data COULD be tidied up to, but I am not aware of how to split these
## ------programmatically... 
##        > > Type: [TIME, FREQ]                  factor-> prefix 't' or 'f', one or the other
##        > > Origin: [Body, Gravity]               factor-> each recorded var is one or the other
##        > > Device: [Accelerometer, Gyroscope]  factor-> is one or the other
##        > > Jerk: Jerk                boolean   measure is a jerk calculation or it is not
##        > > Magnitude:  Mag           boolean   measure is a magnitude or it is not
##        > > Axes: [X, Y, Z] or "NA"   factor    factor-> if not a magnitude, 
##                                                    the var belongs to one of the 3 axes      
##                                                    NA -> Magnitudes do not have axes components
##        > > mean: mean                numeric   each combination of the above has a `mean()` and ...
##        > > std:  std                 numeric   ... a standard deviation `std()`
## ------------------------------------------------------------------------------------------------
## ------the Following is what I was able to do with MELT
## ------programmatically... 
##        > > variable:                 factor    the column name -(66 possible rows per subject per activity )
##        > > value:                    numeric   the values of the `mean()` and `std()`
## -------------------------
## -----we add this accumulator columns as well, per step 5 instructions.
##        > > subjectActivityMeasureAvg:   numeric   for each subject, across each of their activities,
##                                                    calculate their `mean()` (average)

## id files to read
fileUrlTest <-".\\UCI HAR Dataset\\test\\X_test.txt"
fileUrlTest_activity <-".\\UCI HAR Dataset\\test\\y_test.txt"
fileUrlTest_subject <-".\\UCI HAR Dataset\\test\\subject_test.txt"

fileUrlActions <-".\\UCI HAR Dataset\\activity_labels.txt"

fileUrlTrain <-".\\UCI HAR Dataset\\train\\X_train.txt"
fileUrlTrain_activity <-".\\UCI HAR Dataset\\train\\y_train.txt"
fileUrlTrain_subject <-".\\UCI HAR Dataset\\train\\subject_train.txt"

library(data.table)
TestDT <- read.table(colClasses = "numeric", 
                     file=fileUrlTest)
##  <step 2>  TestDT[,c] to get all rows for specific columns
TestDT <- TestDT[,c(1:6, 41:46, 81:86, 121:126, 161:166,
                             201:202, 214:215, 227:228, 240:241, 253:254,
                             266:271, 345:350, 424:429,
                             503:504, 516:517, 529:530, 542:543)] 
TestSubjectDT <- read.table(colClasses = "factor", 
                            file=fileUrlTest_subject)

TrainDT <- read.table(colClasses = "numeric", 
                      file=fileUrlTrain)
##  <step 2>   TrainDT[,c] to get all rows for specific columns
TrainDT <- TrainDT[,c(1:6, 41:46, 81:86, 121:126, 161:166,
                             201:202, 214:215, 227:228, 240:241, 253:254,
                             266:271, 345:350, 424:429,
                             503:504, 516:517, 529:530, 542:543)]
TrainSubjectDT <- read.table(colClasses = "factor", 
                             file=fileUrlTrain_subject)
## get activities as descriptions
LabelDT <- read.fortran(fileUrlActions, 
                        format = c("I1", "X1", "A30"))
library(stringr)

##  <step 3> combine activity description into DF
TestActDT <- read.table(colClasses = "factor", 
                             file=fileUrlTest_activity)
tmpA <- LabelDT[TestActDT[[1]],2]
TestActivityList <- as.factor(str_trim(tmpA,
                                       side = "both"))
##  <step 5> combine subject into DF
TestDT <- cbind(TestSubjectDT,
                TestActivityList,
                TestDT
                )

##  <step 3> combine activity description into DF
TrainActDT <- read.table(colClasses = "factor", 
                              file=fileUrlTrain_activity)
tmpA <- NULL
tmpA <- LabelDT[TrainActDT[[1]],2]
TrainActivityList <- as.factor(str_trim(tmpA,
                                        side = "both"))
##  <step 5> combine subject into DF
TrainDT <- cbind(TrainSubjectDT,
                 TrainActivityList,
                 TrainDT
                 )

##  <step 4>  assign names to extracted columns
colnames(TestDT) <- c("Subject", "Activity",
                        "tBodyAccXmean","tBodyAccYmean","tBodyAccZmean",
                        "tBodyAccXstdev","tBodyAccYstdev","tBodyAccZstdev",
                        "tGravityAccXmean","tGravityAccYmean","tGravityAccZmean",
                        "tGravityAccXstdev","tGravityAccYstdev","tGravityAccZstdev",
                        "tBodyAccJerkXmean","tBodyAccJerkYmean","tBodyAccJerkZmean",
                        "tBodyAccJerkXstdev","tBodyAccJerkYstdev","tBodyAccJerkZstdev",
                        "tBodyGyroXmean","tBodyGyroYmean","tBodyGyroZmean",
                        "tBodyGyroXstdev","tBodyGyroYstdev","tBodyGyroZstdev",
                        "tBodyGyroJerkXmean","tBodyGyroJerkYmean","tBodyGyroJerkZmean",
                        "tBodyGyroJerkXstdev","tBodyGyroJerkYstdev","tBodyGyroJerkZstdev",
                        "tBodyAccMagmean","tBodyAccMagstdev",
                        "tGravityAccMagmean","tGravityAccMagstdev",
                        "tBodyAccJerkMagmean","tBodyAccJerkMagstdev",
                        "tBodyGyroMagmean","tBodyGyroMagstdev",
                        "tBodyGyroJerkMagmean","tBodyGyroJerkMagstdev",
                        "fBodyAccXmean","fBodyAccYmean","fBodyAccZmean",
                        "fBodyAccXstdev","fBodyAccYstdev","fBodyAccZstdev",
                        "fBodyAccJerkXmean","fBodyAccJerkYmean","fBodyAccJerkZmean",
                        "fBodyAccJerkXstdev","fBodyAccJerkYstdev","fBodyAccJerkZstdev",
                        "fBodyGyroXmean","fBodyGyroYmean","fBodyGyroZmean",
                        "fBodyGyroXstdev","fBodyGyroYstdev","fBodyGyroZstdev",
                        "fBodyAccMagmean","fBodyAccMagstdev",
                        "fBodyAccJerkMagmean","fBodyAccJerkMagstdev",
                        "fBodyGyroMagmean","fBodyGyroMagstdev",
                        "fBodyGyroJerkMagmean","fBodyGyroJerkMagstdev"
)
##  <step 4>  assign names to extracted columns
colnames(TrainDT) <- c("Subject", "Activity",
                        "tBodyAccXmean","tBodyAccYmean","tBodyAccZmean",
                        "tBodyAccXstdev","tBodyAccYstdev","tBodyAccZstdev",
                        "tGravityAccXmean","tGravityAccYmean","tGravityAccZmean",
                        "tGravityAccXstdev","tGravityAccYstdev","tGravityAccZstdev",
                        "tBodyAccJerkXmean","tBodyAccJerkYmean","tBodyAccJerkZmean",
                        "tBodyAccJerkXstdev","tBodyAccJerkYstdev","tBodyAccJerkZstdev",
                        "tBodyGyroXmean","tBodyGyroYmean","tBodyGyroZmean",
                        "tBodyGyroXstdev","tBodyGyroYstdev","tBodyGyroZstdev",
                        "tBodyGyroJerkXmean","tBodyGyroJerkYmean","tBodyGyroJerkZmean",
                        "tBodyGyroJerkXstdev","tBodyGyroJerkYstdev","tBodyGyroJerkZstdev",
                        "tBodyAccMagmean","tBodyAccMagstdev",
                        "tGravityAccMagmean","tGravityAccMagstdev",
                        "tBodyAccJerkMagmean","tBodyAccJerkMagstdev",
                        "tBodyGyroMagmean","tBodyGyroMagstdev",
                        "tBodyGyroJerkMagmean","tBodyGyroJerkMagstdev",
                        "fBodyAccXmean","fBodyAccYmean","fBodyAccZmean",
                        "fBodyAccXstdev","fBodyAccYstdev","fBodyAccZstdev",
                        "fBodyAccJerkXmean","fBodyAccJerkYmean","fBodyAccJerkZmean",
                        "fBodyAccJerkXstdev","fBodyAccJerkYstdev","fBodyAccJerkZstdev",
                        "fBodyGyroXmean","fBodyGyroYmean","fBodyGyroZmean",
                        "fBodyGyroXstdev","fBodyGyroYstdev","fBodyGyroZstdev",
                        "fBodyAccMagmean","fBodyAccMagstdev",
                        "fBodyAccJerkMagmean","fBodyAccJerkMagstdev",
                        "fBodyGyroMagmean","fBodyGyroMagstdev",
                        "fBodyGyroJerkMagmean","fBodyGyroJerkMagstdev"
)
library(dplyr)
TestDT <- dplyr::arrange(TestDT, 
                         Subject, 
                         Activity)
TrainDT <- dplyr::arrange(TrainDT, 
                         Subject, 
                         Activity)
##  <step 1>
MergedDT <- merge(TestDT, 
                  TrainDT,
                  all=T)
## free memory objects
rm(TestDT,
   TrainDT,
   LabelDT,
   TestSubjectDT,
   TrainSubjectDT,
   TestActDT,
   TrainActDT,
   TestActivityList,
   TrainActivityList,
   tmpA) 

##  <step 5> generate new DF (reshape) in tidy format:
##      <5> > Subject:  id                        integer
##      <3> > Activity: [WALKING, WALKING_UPSTAIRS, 
##                      WALKING_DOWNSTAIRS, SITTING, 
##                      STANDING, LAYING]         factor
##        > > variable:                 factor    the column name -(66 possible rows per subject per activity )
##        > > value:                    numeric   the values of the `mean()` and `std()`

##      > Arrange by subject, then by activity
arrangedDT <- dplyr::arrange(MergedDT, 
                             Subject, 
                             Activity)
rm(MergedDT)
library(reshape2)
meltedDT <- melt(arrangedDT,
                id.vars = c("Subject", "Activity"),
                 variable.name = "Mean_StdDev_Var",
                 value.name = "Mean_StdDev_Value",
                 na.rm = TRUE
                 )
rm(arrangedDT)
##        > > subjectActivityMeasureAvg:   numeric   
##            for each subject, across each of their activities,
##                              calculate their `mean()` (average)
groupedDT <- group_by(meltedDT,
                      Subject,
                      Activity,
                      Mean_StdDev_Var
                      )
recastDT <- dcast(groupedDT,
                  Subject + Activity + Mean_StdDev_Var ~ .,
                  mean,
                  value.var= "Mean_StdDev_Value"
                  )
recastDT <- dcast(recastDT,
                  Subject + Activity ~ Mean_StdDev_Var,
                  value.var= "."
                  )
## change subject back to a numeric to allow a final proper sort...
recastDT <- mutate(recastDT, 
                   Subject = as.numeric(Subject))
setnames(recastDT,
         old = colnames(recastDT)[3:68], 
         new= paste0("MeanOf_",colnames(recastDT)[3:68]))
## ...now arrange ordering...
PerSubjectPerActivityAverages <- arrange(recastDT,        
                                         Subject,        
                                         Activity)
## ... and change subject back to a factor for proper form
PerSubjectPerActivityAverages <- mutate(PerSubjectPerActivityAverages, 
                                        Subject = as.factor(Subject))

## set up a new-tidy dataset data directory
if(!file.exists("Tidydata")) {dir.create("Tidydata")}
for (texFl in list.files(".//Tidydata//")) {
  if(grepl(".txt",texFl)) {
    file.remove(paste0(".//Tidydata//",texFl))
  }
}
## identify the time of the merging
dateTidyCreated <- Sys.time()
library(stringr)
dateTidyCreated <- paste0("Merged_", str_trim(format(dateTidyCreated, 
                                                     format = "%a %b %d %Y @ %I~%M~%S %p"),
                                              side = "both"))

file.create(paste0(".//Tidydata//",dateTidyCreated,".txt"))
## export the condensed tidy file
write.table(PerSubjectPerActivityAverages, 
            file=".//Tidydata//TidySubjActAverages.txt",
            col.names=TRUE,
            row.names=FALSE)
## clear up workspace and memeory
rm(PerSubjectPerActivityAverages,
   recastDT,
   groupedDT,
   meltedDT)
