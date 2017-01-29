#set working directory to the location where you unzipped the data
setwd("C:/Users/johnarmistead/Data Sci Coursera/Class3/UCI HAR Dataset")

# Read data
features     <- read.table('./features.txt',header=FALSE); #imports features.txt
activityType <- read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       <- read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       <- read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       <- read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       <- read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Give column names to data
colnames(activityType)  <- c('activityId','activityType');
colnames(subjectTrain)  <- "subjectId";
colnames(xTrain)        <- features[,2]; 
colnames(yTrain)        <- "activityId";
colnames(subjectTest) <- "subjectId";
colnames(xTest)       <- features[,2]; 
colnames(yTest)       <- "activityId";

# Get rid of all the other columns without mean or st dev data
data <- rbind(cbind(yTrain,subjectTrain,xTrain), cbind(yTest,subjectTest,xTest))
colNames <- colnames(data)
subset = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
almost <- data[subset==TRUE]
finally <- merge(almost, activityType, by='activityId', all.x = TRUE)
colNames <- colnames(finally)

for (i in 1:length(colNames)) 
{
        colNames[i] <- gsub("\\()","",colNames[i])
        colNames[i] <- gsub("-std$","StdDev",colNames[i])
        colNames[i] <- gsub("-mean","Mean",colNames[i])
        colNames[i] <- gsub("^(t)","Time",colNames[i])
        colNames[i] <- gsub("^(f)","Freq",colNames[i])
        colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames(finally) <- colNames

laststep  <- finally[,names(finally) != 'activityType']

tidy    = aggregate(laststep[,names(laststep) != c('activityId','subjectId')],by=list(activityId=laststep$activityId,subjectId = laststep$subjectId),mean)

# Merge tidy with activityType with descriptions
tidy    = merge(tidy,activityType,by='activityId',all.x=TRUE)

# Export 
write.table(tidy, './tidyData.txt',row.names=TRUE,sep='\t')

#enjoy
