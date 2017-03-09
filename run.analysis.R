## start here
setwd("C:/Users/user/Data Sci Coursera/Class3/UCI HAR Dataset")

install.packages("plyr") ## for the use of ddply at the end
library(plyr)

install.packages("dplyr") # for the use of inner join and select
library(dplyr)

# Read data
features     <- read.table('./features.txt',header=FALSE); #imports features.txt
activityType <- read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       <- read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       <- read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       <- read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       <- read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Give column names to the raw data
colnames(activityType)  <- c('activityId','activityType');
colnames(subjectTrain)  <- "subjectId";
colnames(xTrain)        <- features[,2]; 
colnames(yTrain)        <- "activityId";
colnames(subjectTest) <- "subjectId";
colnames(xTest)       <- features[,2]; 
colnames(yTest)       <- "activityId";

# create a subset with only the column names you want
data <- rbind(cbind(yTrain,subjectTrain,xTrain), cbind(yTest,subjectTest,xTest))
        names(data) <- make.unique(names(data)) # make them unique
        subset <- select(data, activityId, subjectId, contains("-mean()", ignore.case = TRUE), contains("-std()", ignore.case = TRUE))

        colNames <- colnames(subset) ## break out column names
        for (i in 1:length(colNames)) ## clean up junk characters in column headers
        {
                colNames[i] <- gsub("\\()","",colNames[i])
                colNames[i] <- gsub("-std$","StdDev",colNames[i])
                colNames[i] <- gsub("-mean$","Mean",colNames[i])
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
        colnames(subset) <- colNames ## put column names back in
        subberset <- inner_join(subset, activityType, by='activityId') ## bring in Activity Type names

        tidy <- select(subberset, activityId, subjectId, activityType, everything()) ## put activity Type details in the far left column.
        average <- ddply(tidy, .(activityType), function(x) colMeans(x[, 4:68])) ## average all variables by the Activity Type factors

## export
write.table(tidy, './tidydata.txt',row.names=TRUE,sep='\t') ## there you have the tidy set.
write.table(average, "./averages.txt", row.names=TRUE,sep='\t')
