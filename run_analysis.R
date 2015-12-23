### Reads in tables from delimited text files.
### X_?????.txt contains the observational data (561 variables)
### y_?????.txt contains the test subject index for X_?????.txt
### subject_?????.txt contains the activity index for X_?????.txt

### Transforms raw dataset into a tidy dataset containing only the mean and std
### of each observed vector. Further tidies dataset by grouping observations by
### Subject and Activity and calculates the mean over the aggregation. With 30
### Subjects and 6 Activities the final should have 180 rows.

library(data.table)

columnNames <- read.delim('features.txt',sep="",header=FALSE)
testSubjects <- read.delim('test/subject_test.txt',sep="",header = FALSE,col.names = c('Subject'))
testActivities <- read.delim('test/y_test.txt',sep="",header = FALSE,col.names = c('Activity'))
testData <- read.delim('test/X_test.txt',sep="",header = FALSE,col.names = columnNames[,2])
testTotal <- cbind(testSubjects,testActivities,testData)
rm(testData) ## try and conserve memory usage
trainSubjects <- read.delim('train/subject_train.txt',sep="",header = FALSE,col.names = c('Subject'))
trainActivities <- read.delim('train/y_train.txt',sep="",header = FALSE,col.names = c('Activity'))
trainData <- read.delim('train/X_train.txt',sep="",header = FALSE,col.names = columnNames[,2])
trainTotal <- cbind(trainSubjects,trainActivities,trainData)
rm(trainData) ## try and conserve memory usage
fullTotal <- rbind(testTotal,trainTotal)
rm(testTotal) ## more conservation of memory
rm(trainTotal)
### Extract column ids for the the means and STD of each type
means <- grep("*mean()",colnames(fullTotal))
stds <- grep("*std()",colnames(fullTotal))
### Add the subject and activity columns to the extracted columns
tidyColumns = c(1,2,means,stds)
### Extract the tidyColumns from the combined dataset.
tidyTotal = fullTotal[,tidyColumns]
rm(fullTotal) ## more memory conservation
### Add labelling column for a human readable activities title
activityLabels <- read.delim('activity_labels.txt',sep="",header = FALSE)
tidyTotal$ActivityLabel <- activityLabels[match(tidyTotal$Activity,activityLabels$V1),2,drop = F]
tidyTotal <- cbind(tidyTotal[,1:2],tidyTotal[,length(colnames(tidyTotal))],tidyTotal[,4:length(colnames(tidyTotal))-1])
### Create Averages(mean) Grouped By Subject and Activity
tidyDT <- data.table(tidyTotal)
setnames(tidyDT,"V2","ActivityLabel")
setkey(tidyDT,Subject,ActivityLabel)
tidyMeans <- tidyDT[,lapply(.SD,mean),by=list(Subject,ActivityLabel)]
### Output as requested in assignment description
write.table(tidyMeans,file="tidyData.txt",row.names = FALSE)
