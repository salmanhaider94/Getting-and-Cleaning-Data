#questions : 
# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names.
# 5 From the data set in step 4, creates a second, independent 
#   tidy data set with the average of each variable for each activity and each subject.

#pwd()
#current dir is : "C:/Users/Sc/Documents/UCI HAR Dataset"
library(plyr)

## Reading all Data files
subject_test <- read.table("test/subject_test.txt")
subject_train <- read.table("train/subject_train.txt")

x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")

x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt") 

#merging train data
traindata<-cbind(subject_train,x_train,y_train)

#merging test data
testdata<-cbind(subject_test,x_test,y_test)

#final merged dataset 
finaldata<- rbind(traindata,testdata)
head(finaldata)

# Extracts only the measurements on the mean and standard deviation for each measurement
finaldata<- finaldata[,grep("mean()|std()", features[, 2])] 

#4 Appropriately labels the data set with descriptive activity names
Labelss <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(finaldata) <- Labelss[grep("mean()|std()", features[, 2])]

subject <- rbind(subject_train, subject_test)
names(subject) <- 'subject'
activity <- rbind(y_train, y_test)
names(activity) <- 'activity'
finaldata <- cbind(subject,activity, finaldata)

#(did 4 above!) 3 Uses descriptive activity names to name the activities in the data set
groupacts <- factor(finaldata$activity)
levels(groupacts) <- activity_labels[,2]
finaldata$activity <- groupacts
 

#5 From the data set in step 4, creates a second, independent 
# tidy data set with the average of each variable for each activity and each subject.
library("reshape2")

indData <- melt(finaldata,(id.vars=c("subject","activity")))
ind2data <- dcast(indData, subject + activity ~ variable, mean)
names(ind2data)[-c(1:2)] <- paste("[mean of]" , names(ind2data)[-c(1:2)] )
write.table(ind2data, "tidy_data.txt", sep = ",",row.name = FALSE)