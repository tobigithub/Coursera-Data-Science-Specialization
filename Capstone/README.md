## Capstone project of the Coursera Data Science Specialization in cooperation with swiftkey

These are the main files of my capstone project app. The
app can be run at [this link](https://thie1e.shinyapps.io/Nextword). If possible
I am going to add a link to the data so that the creation of the app and the 
underlying algorithm is completely reproducible.

"Capstone create n-grams.R" has to be run fist to create the necessary RData-files
that Capstone.R will use later to solve the Coursera-quizzes that asked some
simple questions about the data and to make predictions for the next word in a 
sequence of words which is the purpose of the app. Capstone.R also calculates
Kneser-Ney-smoothed word probabilities, saves the results and defines several functions
that were later used for the Shiny app.

Interface and main features
========================================================
* Prediction of the next word (top prediction largest)
* Probable next words: a Markov Chain prediction of the continuation of the sentence given the top prediction
* Supports German and English

Additional features
========================================================
* The user can choose from two prediction algorithms
* A gauge for "relative confidence" in the top prediction: the square root of the quantile of the count or probability within its respective group of (skip-)n-grams

Prediction algorithms and data
========================================================
* *Data*
      + The predictions are based on 7.5 million unique (skip-)n-grams ranging from unigrams to
      4-grams. To account for longer dependencies skip-5-grams and skip-6-grams are used.
      + The (skip-)n-grams were generated using tweets, news and blog articles

****
* *Algorithms*
      + Raw counts and Katz-Backoff: The word with the highest count following the longest possible n-gram
      + Kneser-Ney-smoothing and backoff to skip-n-grams: Recursively
      calculated probabilities of n-grams and backoff to skip-n-grams

Further development and additional information
======================================================
* In the current version Kneser-Ney-smoothing does not beat Katz backoff in
benchmarks which can probably be improved
* Based on statistical tagging and Hidden-Markov-Models grammar could be
incorporated into the prediction algorithm (there was no resource for tagged
language data available)
* **Please allow a startup time of around 15 seconds when the app is opened**


## Descriptions of the algorithms 
### n-grams and skip-n-grams
The usual n-gram consists of a sequence of n words, for example "That is" is a 
bigram. In addition the available models both use skip-5-grams and skip-6-grams
which are defined here as a word at position $w_{i}$ and a preceding word at position 
$w_{i-n+1}$, so that e.g. a skip-5-gram is one order higher than a normal 4-gram.
Skip-n-grams are chosen because including the intermediate words would capture
too much of the content / context of the original statement.

### Raw counts and Katz-Backoff
This algorithm finds the matching n-gram of the highest possible order among the known
n-grams and returns the word with the highest count, i.e. the word that followed
the given n-gram most often. This is equivalent to a Maximum Likelihood prediction.
If no matching n-gram is found at the highest order the algorithm backs off to 
n-grams of order $n-1$. If no matches within the ordinary n-grams can be found
the algorithm backs off to skip-5-grams and skip-6-grams. If no matching n-grams
can be found at all the algorithm predicts the most common tokens (the, to and a).

### Kneser-Ney-Smoothing
Kneser-Ney-Smoothing uses not only information contained in one
n-gram but also information from lower order n-grams by calculating probabilities
recursively, e.g. the formula for 3-grams contains a the Kneser-Ney-probability
using the 2-grams. 

The main difference is that Kneser-Ney smoothing not only uses frequencies of n-grams
but also how likely it is for a given word to appear in different contexts. Some words
may be used in a lot of contexts whereas some other words like "Francisco" are used
mainly in specific contexts (here, usually after the word "San"). 

See http://www.foldl.me/2014/kneser-ney-smoothing/ and Jurafsky & Martin (2007),
Speech and Language Processing: An introduction to natural language processing, 
computational linguistics, and speech recognition, pp. 27ff. for more information
on Kneser-Ney-Smoothing.


## Benchmark results

These are the results of the benchmark which was provided by Jan Hagelauer in the 
[Coursera forums](https://class.coursera.org/dsscapstone-003/forum/thread?thread_id=273).

As can be seen, Kneser-Ney-smoothing does not beat the simpler backoff model. I 
checked the calculations and could not find errors but this would be a case for 
further investigation or development.

****

Raw counts with (Skip-)n-gram-Backoff:

```
Overall top-3 score:     20.07 %
Overall top-1 precision: 14.87 %
Overall top-3 precision: 24.72 %
Average runtime:         8.70 msec
Total memory used:       352.02 MB

Dataset details
 Dataset "blogs" (599 lines, 14587 words, hash 14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2)
  Score: 17.32 %, Top-1 precision: 12.89 %, Top-3 precision: 21.37 %
 Dataset "quizzes" (20 lines, 323 words, hash 07697c9cf45891a1f6da633299f35522711a17e65136ba261702e78e0abd09e1)
  Score: 24.75 %, Top-1 precision: 17.82 %, Top-3 precision: 31.02 %
 Dataset "tweets" (793 lines, 14011 words, hash 7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4)
  Score: 18.15 %, Top-1 precision: 13.88 %, Top-3 precision: 21.77 %
```

*****

Kneser-Ney-Smoothing with Skip-n-gram-Backoff:

```
Overall top-3 score:     19.40 %
Overall top-1 precision: 14.45 %
Overall top-3 precision: 23.98 %
Average runtime:         8.39 msec
Total memory used:       352.02 MB

Dataset details
 Dataset "blogs" (599 lines, 14587 words, hash 14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2)
  Score: 17.43 %, Top-1 precision: 12.81 %, Top-3 precision: 21.58 %
 Dataset "quizzes" (20 lines, 323 words, hash 07697c9cf45891a1f6da633299f35522711a17e65136ba261702e78e0abd09e1)
  Score: 22.88 %, Top-1 precision: 16.83 %, Top-3 precision: 29.04 %
 Dataset "tweets" (793 lines, 14011 words, hash 7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4)
  Score: 17.89 %, Top-1 precision: 13.71 %, Top-3 precision: 21.31 %
```

