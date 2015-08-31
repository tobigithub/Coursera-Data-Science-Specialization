library(doParallel)
cl <- makeCluster(as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')))
registerDoParallel(cl)

# Question 1
# Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
library(caret)
# Set the variable y to be a factor variable in both the training and test set.
# Then set the seed to 33833. Fit (1) a random forest predictor relating the
# factor variable y to the remaining variables and (2) a boosted predictor
# using the "gbm" method. Fit these both with the train() command in the caret package.
# 
# What are the accuracies for the two approaches on the test data set? What is
# the accuracy among the test set samples where the two methods agree? 
vowel.test$y <- factor(vowel.test$y)
vowel.train$y <- factor(vowel.train$y)
set.seed(33833)
fit_rf <- train(y ~ ., data = vowel.train, method = "rf")
fit_gbm <- train(y ~ ., data = vowel.train, method = "gbm")
pred_rf <- predict(object = fit_rf, newdata = vowel.test)
pred_gbm <- predict(object = fit_gbm, newdata = vowel.test)
confusionMatrix(pred_rf, reference = vowel.test$y)
confusionMatrix(pred_gbm, reference = vowel.test$y)
# RF Accuracy = 0.6061
# GBM Accuracy = 0.5325
# Agreement Accuracy = 0.6518


#-----------------------------------------------------------------------------
# Question 2
# Load the Alzheimer's data using the following commands
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Set the seed to 62433 and predict diagnosis with all the other variables 
# using a random forest ("rf"), boosted trees ("gbm") and linear discriminant
# analysis ("lda") model. Stack the predictions together using random forests
# ("rf"). What is the resulting accuracy on the test set? Is it better or
# worse than each of the individual predictions? 
set.seed(62433)
fit_rf <- train(diagnosis ~ ., data = training, method = "rf")
set.seed(62433)
fit_gbm <- train(diagnosis ~ ., data = training, method = "gbm")
set.seed(62433)
fit_lda <- train(diagnosis ~ ., data = training, method = "lda")
pred_rf <- predict(object = fit_rf, newdata = testing)
pred_gbm <- predict(object = fit_gbm, newdata = testing)
pred_lda <- predict(object = fit_lda, newdata = testing)
confusionMatrix(pred_rf, reference = testing$diagnosis) # 79,3%
confusionMatrix(pred_gbm, reference = testing$diagnosis) # 84,2%
confusionMatrix(pred_lda, reference = testing$diagnosis) # 76,8%
# Fit ensemble model
predDF <- data.frame(pred_rf, pred_gbm, pred_lda, testing$diagnosis)
fit_ensemble <- train(testing.diagnosis ~., method="rf", data=predDF)
pred_ensemble <- predict(fit_ensemble, predDF)
confusionMatrix(pred_ensemble, testing$diagnosis) # 87,8%


set.seed(62433)
fit1<-train(diagnosis~., method="rf", data=training)
fit2<-train(diagnosis~., method="gbm", data=training, verbose=F)
fit3<-train(diagnosis~., method="lda", data=training)

pred1<-predict(fit1, testing)
pred2<-predict(fit2, testing)
pred3<-predict(fit3, testing)

df<-data.frame(pred1, pred2, pred3, diagnosis=testing$diagnosis)

combfit<-train(diagnosis~., method="rf", data=df)
combpred<-predict(combfit, df)

confusionMatrix(testing$diagnosis, pred1)
confusionMatrix(testing$diagnosis, pred2)
confusionMatrix(testing$diagnosis, pred3)
confusionMatrix(testing$diagnosis, combpred)
# The ensemble performs best, gbm just as good

#----------------------------------------------------------------------
# Question 3
# Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
# Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
# Which variable is the last coefficient to be set to zero as the penalty
# increases? (Hint: it may be useful to look up ?plot.enet). 
set.seed(233)
fit <- train(CompressiveStrength ~ ., method = "lasso", data = training)
plot.enet(fit$finalModel)
# Cement


#---------------------------------------------------------------------------
# Question 4
# 
# Load the data on the number of visitors to the instructors blog from here:

url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(url, destfile = "gaData.csv")
# Using the commands:
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
# Fit a model using the bats() function in the forecast package to the training 
# time series. Then forecast this model for the remaining time points. For
# how many of the testing points is the true value within the 95% prediction
# interval bounds? 
library(forecast)
fit <- bats(tstrain)
pred <- forecast(object = fit, h = nrow(testing))
true <- testing$visitsTumblr
upper95 <- pred$upper[,2]
lower95 <- pred$lower[,2]
length(true > lower95 & true < upper95)
sum(true > lower95 & true < upper95)
226/235



# Question 5
# Load the concrete data with the commands:

set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 325 and fit a support vector machine using the e1071 package
# to predict Compressive Strength using the default settings. Predict on 
# the testing set. What is the RMSE? 

library(e1071)
set.seed(325)
fit <- svm(CompressiveStrength ~ ., data = training)
pred <- predict(fit, newdata = testing)
accuracy(pred, x = testing$CompressiveStrength)
# 6.72 
