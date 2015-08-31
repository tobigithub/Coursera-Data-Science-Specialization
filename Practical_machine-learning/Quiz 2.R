# Question 1
# Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
# Which of the following commands will create training and test sets with 
# about 50% of the observations assigned to each?
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


# Question 2
# Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# Make a plot of the outcome (CompressiveStrength) versus the index of the 
# samples. Color by each of the variables in the data set 
# (you may find the cut2() function in the Hmisc package useful for turning
#  continuous covariates into factors). What do you notice in these plots?
library(Hmisc)
library(ggplot2)
trainingCut <- (apply(training, 2, cut2))
trainingCut <- data.frame(trainingCut)
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$Cement, lwd = 2)
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$BlastFurnaceSlag, lwd = 2)
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$FlyAsh, lwd = 2)
# Within the four classes FlyAsh seems predictive of Compressive Strength
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$Water, lwd = 2)
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$Superplasticizer, lwd = 2)
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$Cement, lwd = 2)
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$CoarseAggregate, lwd = 2)
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$FineAggregate, lwd = 2)
qplot(1:length(training$CompressiveStrength), training$CompressiveStrength, 
      color = trainingCut$Age, lwd = 2)
qplot(trainingCut$Age, training$CompressiveStrength)
# There is a step-like pattern in the plot of outcome versus index in the 
# training set that isn't explained by any of the predictor variables so there
# may be a variable missing.


# Question 3
# Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# Make a histogram and confirm the SuperPlasticizer variable is skewed.
# Normally you might use the log transform to try to make the data more
# symmetric. Why would that be a poor choice for this variable?
hist(training$Superplasticizer)
# There are a large number of values that are the same and even if you took 
# the log(SuperPlasticizer + 1) they would still all be identical so the
# distribution would not be symmetric.


# Question 4
# Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Find all the predictor variables in the training set that begin with IL.
# Perform principal components on these variables with the preProcess() 
# function from the caret package. Calculate the number of principal 
# components needed to capture 90% of the variance. How many are there?
ILindex <- grep(pattern = "^IL_", x = colnames(training))
trainingIL <- training[, ILindex]
preProc <- preProcess(trainingIL, method="pca", thresh = 0.9)
trainPC <- predict(preProc, trainingIL)
# 9


# Question 5
# Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Create a training data set consisting of only the predictors with variable 
# names beginning with IL and the diagnosis. Build two predictive models, 
# one using the predictors as they are and one using PCA with principal 
# components explaining 80% of the variance in the predictors.
# Use method="glm" in the train function. What is the accuracy of each 
# method in the test set? Which is more accurate?
ILindex <- grep(pattern = "^IL_", x = colnames(training))
trainingIL <- training[, ILindex]
testingIL <- testing[, ILindex]
training2 <- cbind(trainingIL, training$diagnosis)
colnames(training2)[13] = "diagnosis"
preProc <- preProcess(trainingIL, method="pca", thresh = 0.8)
trainPC <- predict(preProc, trainingIL)
# 7 variables to capture 80% of the variance
modelFitPC <- train(trainPC, training2$diagnosis, method = "glm")
# Accuracy 70%
modelFit <- train(trainingIL, training2$diagnosis, method = "glm")
# Accuracy 68,7%
# Test set:
# With PCA
testPC <- predict(preProc, testingIL)
predictions <- predict(modelFitPC, testPC)
confusionMatrix(testing$diagnosis, predictions)
# Accuracy 71,95%
# Without PCA:
predictions <- predict(modelFit, testing)
confusionMatrix(testing$diagnosis, predictions)
# Accuracy 64,63%
