###
# Load data
###
# Working directory was set
unzip("getdata-projectfiles-UCI HAR Dataset.zip")

subject_train <- read.table("subject_train.txt", 
                            quote="\"", stringsAsFactors=FALSE)
X_train <- read.table("X_train.txt", 
                      quote="\"", stringsAsFactors=FALSE)
y_train <- read.table("y_train.txt", 
                      quote="\"", stringsAsFactors=FALSE)
subject_test <- read.table("subject_test.txt", 
                           quote="\"", stringsAsFactors=FALSE)
X_test <- read.table("X_test.txt", 
                     quote="\"", stringsAsFactors=FALSE)
y_test <- read.table("y_test.txt", 
                     quote="\"", stringsAsFactors=FALSE)

### Overview of the data
dim(subject_test)
dim(X_test)
dim(y_test)
# 2947 measurements per variable in the test set
dim(unique(subject_test))
# ...from 9 differenct subjects 
# (persons wearing the smartphone with measurement software)
dim(subject_train)
dim(X_train)
dim(y_train)
# 7352 measurements per variable in the train set
dim(unique(subject_train))
# ...from 21 differenct subjects 
# (persons wearing the smartphone with measurement software)


###
# Bind the test data to the train data
###
subject <- rbind(subject_train, subject_test); names(subject) <- "Subject"
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
dim(X) # 10299 561
dim(y) # 10299 1

###
# Label the variables in the data set with descriptive names
###
# Descriptive names can be obtained from the features data frame
# What are the features?
features <- read.table("~/Github/GACD-project/UCI HAR Dataset/features.txt", 
                       quote="\"", stringsAsFactors=FALSE)
dim(features)
# 561 features. That is OK, since the X data frame has 561 columns.
names(features) <- c("Index", "Varname")
colnames(X) <- features$Varname


###
# Extract only mean and standard deviation measures
###
# Where are the means?
meanindices <- grep(pattern = "mean", x = features$Varname)
# Where are the standard deviations?
stdindices <- grep(pattern = "std", x = features$Varname)
# Extract these columns and store in the new data frame Xmsd
Xmsd <- X[, meanindices]
Xmsd <- cbind(Xmsd, X[, stdindices])


###
# Label the activities
###
# This can be achieved by using a factor variable instead of a numeric value 
# for labeling the activities in the y vector. The labels are:
activity_labels <- read.table("~/Github/GACD-project/UCI HAR Dataset/activity_labels.txt", 
                              quote="\"")
activity_labels
# V1                 V2
# 1  1            WALKING
# 2  2   WALKING_UPSTAIRS
# 3  3 WALKING_DOWNSTAIRS
# 4  4            SITTING
# 5  5           STANDING
# 6  6             LAYING

# A factor variable can easily be converted back to numerical if needed.
y <- sapply(X = y, FUN = function(x) x = activity_labels$V2[x])
colnames(y) <- "Activity"
Activity <- factor(y)


###
# Creating a second, independent tidy data set with the average of each variable 
# for each activity and each subject. 
###
# Now, not only the measurements are needed but also the associated activities
# and the information which subject the measurements belong to. Merge
# measurements, activities and subjects
data <- cbind(subject, Activity, Xmsd)
averages <- aggregate(data, by = list(data$Subject, data$Activity), FUN = mean)
# Remove columns which averages are not needed for (Subject and Activity) and
# label the grouping columns (which were also Subject and Activity)
averages <- averages[, -c(3, 4)]
names(averages)[c(1,2)] <- c("Subject", "Activity")
# Write this table to a txt file
write.table(averages, "averages.txt", sep=";", row.names = F)
