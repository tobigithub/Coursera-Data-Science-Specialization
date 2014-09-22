# Predicting the quality of the execution of physical exercises using measurement data

Load necessary packages:

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

Downloading and reading in the data:


```r
if (!file.exists("training.csv")){
      download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                    method = "wget", 
                    destfile = "training.csv")
      }

if (!file.exists("testing.csv")){
      download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                    method = "wget",
                    destfile = "testing.csv")
      }

training <- read.csv("training.csv", na.strings = c("#DIV/0!", "NA"))
test <- read.csv("testing.csv", na.strings = c("#DIV/0!", "NA"))
```

## Data processing
There are some variables in the data which will be dropped:
- X is the row number
- num_window is an increasing count
- user_name is the name of the person who performed the exercise. This may be helpful in predicting the quality of the execution here, but I would like to build a model that is more general.
- timestamps should theoretically play no role regarding the execution of exercises


```r
# Drop variables as described above
training <- training[, -(1:7)]
```

Additionally, there may be variables that contain virtually no variation. Those are not helpful in prediction and will be dropped as well. First, columns (features) that contain only NAs will be dropped.


```r
training <- training[, colSums(is.na(training)) != nrow(training)]

nzv <- nearZeroVar(training) # 29
training <- training[, -nzv]; rm(nzv)
```

There are many rows with missing values. As the intended method in this paper is Random Forest, which does not accept missing values, those rows will be dropped.


```r
training <- training[complete.cases(training), ]
```

There are 117 predictors left. The variable "classe" is to be predicted.


```r
str(training$classe)
```

```
##  Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

#### Partition data
There are 217 cases in the training data. The data will be split into a training set (70%) and test set(30%). The testing set contains just `nrow (testing)` cases and won't be used in this paper.


```r
inTest <- createDataPartition(y = training$classe, p=0.3, list=FALSE)
test <- training[inTest,]

training <- training[-inTest,]

# Check
dim(training)
```

```
## [1] 149 118
```

```r
dim(test)
```

```
## [1]  68 118
```

## Model building
For prediction the Random Forest algorithm will be used. It uses bootstrapped samples to estimate decision trees. At each split, also the variables that are used as predictors are bootstrapped. All trained trees then "vote" for the class that should be predicted. This algorithm is relatively accurate but prone to overfitting. The CV set will be used to deal with overfitting.


```r
# All variables as predictors
modFit <- train(classe ~ ., data = training, method="rf")
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
modFit
```

```
## Random Forest 
## 
## 149 samples
## 117 predictors
##   5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 149, 149, 149, 149, 149, 149, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##     2   0.7       0.6    0.06         0.07    
##    59   0.7       0.6    0.07         0.09    
##   117   0.7       0.6    0.07         0.08    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 59.
```

Additionally, two more models will be trained that contain less predictors. Instead of the original predictors n principal components will be used. In the paper that accompanies the data set the authors used only 17 of 96 derived features (p3, [Qualitative Activity Recognition of Weight Lifting Exercises](http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf)). 

Thus, models with 10, 17 and 40 predictors will be trained. These choices are a bit arbitrary but within the scope of this paper this approach should suffice. Additionally, a model that uses PCs that contain 95% of the variance will be estimated.


```r
modFit10 <- train(classe ~ ., data = training, method="rf", 
                  preProcess = "pca", pcaComp = 10)
```

```
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
```

```r
modFit10
```

```
## Random Forest 
## 
## 149 samples
## 117 predictors
##   5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: principal component signal extraction, scaled, centered 
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 149, 149, 149, 149, 149, 149, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##     2   0.5       0.3    0.07         0.09    
##    59   0.4       0.3    0.06         0.08    
##   117   0.4       0.3    0.06         0.08    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

```r
modFit17 <- train(classe ~ ., data = training, method="rf", 
                  preProcess = "pca", pcaComp = 17)
```

```
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
```

```r
modFit17
```

```
## Random Forest 
## 
## 149 samples
## 117 predictors
##   5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: principal component signal extraction, scaled, centered 
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 149, 149, 149, 149, 149, 149, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##     2   0.5       0.3    0.08         0.09    
##    59   0.5       0.3    0.08         0.10    
##   117   0.5       0.3    0.08         0.09    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

```r
modFit40 <- train(classe ~ ., data = training, method="rf", 
                  preProcess = "pca", pcaComp = 40)
```

```
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
```

```r
modFit40
```

```
## Random Forest 
## 
## 149 samples
## 117 predictors
##   5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: principal component signal extraction, scaled, centered 
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 149, 149, 149, 149, 149, 149, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##     2   0.5       0.3    0.06         0.08    
##    59   0.4       0.3    0.07         0.08    
##   117   0.4       0.3    0.06         0.08    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

```r
modFit95p <- train(classe ~ ., data = training, method="rf", 
                  preProcess = "pca", thresh = 0.95)
```

```
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
```

```r
modFit95p
```

```
## Random Forest 
## 
## 149 samples
## 117 predictors
##   5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: principal component signal extraction, scaled, centered 
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 149, 149, 149, 149, 149, 149, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##     2   0.4       0.3    0.07         0.08    
##    59   0.4       0.3    0.08         0.09    
##   117   0.4       0.3    0.07         0.08    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 117.
```

The models achieved the following in sample accuracies:
* all variables: 0.6601

* 40 PC: 0.4524

* 17 PC: 0.4615

* 10 PC: 0.4598

* 95% variance PCs: 0.4443

The model with all possible features has shown the best performance and will be tested using the test set.


```r
pred <- predict(modFit, test)
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
test$predRight <- pred==test$classe
modtable <- table(pred, test$classe)
# Accuracy
acctest <- sum(diag(modtable)) / nrow(test)
```

The model achieves an accuracy of 0.8235. This is also to be expected in further out of sample applications since the test set was not used before to judge the model. 


```r
modtable
```

```
##     
## pred  A  B  C  D  E
##    A 15  1  1  3  1
##    B  2 13  0  0  1
##    C  0  0 11  0  1
##    D  0  1  0  8  1
##    E  0  0  0  0  9
```
