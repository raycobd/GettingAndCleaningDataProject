
# Initialize the data set
print("Importing subject... ")
subjectTrain <- read.table(file.path(".", "train", "subject_train.txt"), col.names = "subjectID")
subjectTest  <- read.table(file.path(".", "test" , "subject_test.txt" ), col.names = "subjectID")

print("Importing activity labels... ")
activityLabelTrain <- read.table(file.path(".", "train", "y_train.txt"), col.names = "activityID")
activityLabelTest  <- read.table(file.path(".", "test" , "y_test.txt" ), col.names = "activityID")

print("Importing data set... ")
dataTrain <- read.table(file.path(".", "train", "X_train.txt"))
dataTest  <- read.table(file.path(".", "test" , "X_test.txt" ))

print("")
print("")
# Task number 1
print("Task Number 1: Merges the training and the test sets to create one data set... ")

#mergin rows 
subjects <- rbind(subjectTrain, subjectTest)
activityLabels <- rbind(activityLabelTrain, activityLabelTest)
dataSet <- rbind(dataTrain, dataTest)

#mergin all the datasets in a column level
fullDataMerged <- cbind(subjects,activityLabels, dataSet)

print("")
print("")
# Task number 2
print("Task Number 2: Extracts only the measurements on the mean and standard deviation for each measurement... ")
features <- read.table(file.path(".", "features.txt"), col.names = c("featureID", "featureLabel"))
variableNames <- c("subjectID", "activityID", as.vector(features$featureLabel))
fullDataMerged <-setNames(fullDataMerged, variableNames)
dataExtracted <-fullDataMerged[grepl("subjectID|activityID|mean\\(|std\\(", variableNames)]

print("")
print("")
# Task number 3
print("Task Number 3: Uses descriptive activity names to name the activities in the data set... ")
activities <- read.table(file.path(".", "activity_labels.txt"), col.names = c("activityID", "activityLabel"))
dataExtratedctivity <- merge(dataExtracted, activities, by.x="activityID", by.y = "activityID", all=TRUE)

print("")
print("")
# Task number 4
print("Task Number 4: Appropriately labels the data set with descriptive variable names... ")
# Cleaning up the variable names
variableNames <-names(dataExtratedctivity)
for (i in 1:length(variableNames)) 
{
  variableNames[i] = gsub("\\()","",variableNames[i])
  variableNames[i] = gsub("-std","StdDev",variableNames[i])
  variableNames[i] = gsub("-mean","Mean",variableNames[i])
  variableNames[i] = gsub("^t","Time-",variableNames[i])
  variableNames[i] = gsub("^f","Fourier-",variableNames[i])
  variableNames[i] = gsub("([Gg]ravity)","Gravity-",variableNames[i])
  variableNames[i] = gsub("[Bb]ody","Body-",variableNames[i])
  variableNames[i] = gsub("[Aa]cc","Accelerometer-",variableNames[i])
  variableNames[i] = gsub("[Mm]ag","Magnitude-",variableNames[i])
  variableNames[i] = gsub("[Jj]erk","Jerk-",variableNames[i])
  variableNames[i] = gsub("[Gg]yro","Gyroscope-",variableNames[i])
  variableNames[i] = gsub("Body-Body-","Body-",variableNames[i])
};
labelledData <-setNames(dataExtratedctivity, variableNames)

print("")
print("")
# Task number 5
print("Task Number 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.... ")
tidyData <-aggregate(labelledData[, names(labelledData) != "subjectID" & names(labelledData) != "activityID"  & names(labelledData) != "activityLabel" ], by=list(subjectID=labelledData$subjectID,activityID=labelledData$activityID, activityLabel=labelledData$activityLabel ), mean)


# RESULT
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');






