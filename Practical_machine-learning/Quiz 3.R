# Question 1
# Load the cell segmentation data from the AppliedPredictiveModeling package
# using the commands:
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
# 1. Subset the data to a training set and testing set based on the Case 
#      variable in the data set. 
# 2. Set the seed to 125 and fit a CART model with the rpart method using
#      all predictor variables and default caret settings. 
# 3. In the final model what would be the final model prediction for cases
#      with the following variable values:
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

test <- segmentationOriginal[which(segmentationOriginal$Case == "Test"), ]
train <- segmentationOriginal[which(segmentationOriginal$Case == "Train"), ]

set.seed(125)
fit <- train(Class ~ ., data=train, method = "rpart")
library(rattle)
library(rpart.plot)
fancyRpartPlot(fit$finalModel)

# Antworten nach Ansehen des trees:
# a. PS
# b. WS
# c. PS
# d. ??


#------------------------------------------------------------------------------
# Question 2
# If K is small in a K-fold cross validation is the bias in the estimate of 
# out-of-sample (test set) accuracy smaller or bigger? If K is small is the
# variance in the estimate of out-of-sample (test set) accuracy smaller or
# bigger. Is K large or small in leave one out cross validation?
# 
# The bias is larger and the variance is smaller. Under leave one out cross
# validation K is equal to the sample size.


#------------------------------------------------------------------------------
# Question 3
# Load the olive oil data using the commands:
library(pgmm)
data(olive)
olive = olive[,-1]
# (NOTE: If you have trouble installing the pgmm package, you can download 
#  the olive dataset here: olive_data.zip. After unzipping the archive, you can
#  load the file using the load() function in R.)
# These data contain information on 572 different Italian olive oils from
# multiple regions in Italy. Fit a classification tree where Area is the 
# outcome variable. Then predict the value of area for the following data
# frame using the tree command with all defaults
newdata = as.data.frame(t(colMeans(olive)))
# What is the resulting prediction? Is the resulting prediction strange?
# Why or why not?
# Area sollte factor und nicht numerisch sein
olive2 <- olive
olive2$Area <- factor(olive2$Area)
# ...das ist aber nicht gefragt, man soll wohl Area als numerisch behalten
library(tree)
fit <- tree(Area ~ ., data = olive)
predict(fit, newdata)


#-----------------------------------------------------------------------------
# Question 4
# Load the South Africa Heart Disease Data and create training and test sets
# with the following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1], size=dim(SAheart)[1]/2, replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
# Then set the seed to 13234 and fit a logistic regression model 
# (method="glm", be sure to specify family="binomial") with Coronary
# Heart Disease (chd) as the outcome and age at onset, current alcohol
# consumption, obesity levels, cumulative tabacco, type-A behavior, and
# low density lipoprotein cholesterol as predictors. Calculate the
# misclassification rate for your model using this function and a
# prediction on the "response" scale:
missClass = function(values,prediction){
      sum(((prediction > 0.5)*1) != values)/length(values)
}
# What is the misclassification rate on the training set? What is the
# misclassification rate on the test set?
set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
             family = "binomial", data = trainSA, method = "glm")
# Training set missclassification:
predictions <- predict(fit, trainSA)
missClass(trainSA$chd, predictions) # = 0.273
# Test set missclassification:
predictions <- predict(object = fit, newdata = testSA)
missClass(testSA$chd, predictions) # = 0.294

### Nebenbei:
### Man muss bei der glm() Funktion type = "response" wählen!!!
# sonst erhält man andere ergebnisse in der predict funktion als mit 
# predict auf das modellobjekt von train().
set.seed(13234)
fit2 <- glm(formula = chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
            family = "binomial", data = trainSA)
all(fit$finalModel$fitted.values == fit2$fitted.values)
all(fit$finalModel$fitted.values == fit2$fitted.values)

predictions <- predict(fit, trainSA)
predictions2 <- predict(fit2, trainSA, type = "response")
which(predictions == predictions2)
# named integer(0)
all(predictions == predictions2)
# FALSE

# Compare in sample "predictions" with fitted values from the model objects:
predictions_IS <- predict(fit, trainSA)
predictions2_IS <- predict(fit2, trainSA, type ="response")
# The in sample predictions have to be equal to the fitted values:
all(predictions_IS == fit$finalModel$fitted.values)
all(predictions2_IS == fit2$fitted.values)
# Having used type == "response" the in sample "predictions" using predict()
# are the same, too:
all(predictions_IS == predictions2_IS)

# Compare predictions on test set:
predictions <- predict(fit, testSA)
predictions2 <- predict(fit2, testSA, type = "response")
all(predictions == predictions2)

# Compare predictions on test set, no type argument:
predictions <- predict(fit, testSA)
predictions2 <- predict(fit2, testSA)
all(predictions == predictions2)
which(predictions == predictions2)


#------------------------------------------------------------------------------
# Question 5
# Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
# Set the variable y to be a factor variable in both the training and test
# set. Then set the seed to 125. Fit a random forest predictor relating
# the factor variable y to the remaining variables. Read about variable
# importance in random forests here:
# http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
# The caret package uses by defualt the Gini importance. Calculate the variable
# importance using the varImp function in the caret package. What is the order
# of variable importance?
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
data <- rbind(vowel.train, vowel.test)
set.seed(125)
fit <- train(y ~ ., data = vowel.train, method = "rf")
varImp(fit, data="vowel.train")
# The order of the variables is:
#       x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10