GACD-project
============

This is a short introduction on what `run_analysis.R` does. The input data is
not included in this repo because of its size but can be obtained at 
[the UCI machine learning repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones). The dataset consists of various measurements 
from smartphone sensors that can be used to classify the activity of the 
person wearing the smartphone (e.g. sitting, walking downstairs).

Since the original data was split up into a test and training set `run_analysis.R`
first merges the test and training data for the sensor measurements (`X`), the
activities that were observed (`y`) and information which subject the data belongs
to (`subject`). Data from 30 different subjects is included.

The original data does not come with meaningful column names so in a next step
variable names are extracted from `features.txt` then used to label the columns.
Additionally, the numeric activity vector `y` is reformatted as a factor variable
to include easily readable activity names instead of numeric values:
- 1            WALKING
- 2   WALKING_UPSTAIRS
- 3 WALKING_DOWNSTAIRS
- 4            SITTING
- 5           STANDING
- 6             LAYING

Later, only mean and standard deviations will be of interest, so all columns 
including means and standard deviations are selected and stored in the data
frame `Xmsd`.

Finally, an independent data set `averages.txt` is created that includes the
averages of all 561 variables averaged over subject and activity. So, for example,
this data set gives information on the averages of every measurement variable
for subject 10 while laying.

There is also a codebook `CodeBook.md` here that gives further information
on data transformations and variables.