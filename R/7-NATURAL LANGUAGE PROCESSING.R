# Natural Language Processing
setwd("C:/Users/Lucia/OneDrive - Universita degli Studi di Milano-Bicocca/Corsi/Machine Learning - Hands On/Mine/Dataset")
# Importing the dataset
dataset_original = read.delim(file = 'Restaurant_Reviews.tsv', 
                              quote = '', # ignoring any kind of quoting in the text.
                              stringsAsFactors = FALSE
                              )

# Cleaning the texts
# install.packages('tm')
# install.packages('SnowballC')
library(tm)
library(SnowballC)
# Create th so-called volatile corpus.
corpus = VCorpus(VectorSource(dataset_original$Review))
# Lower case.
corpus = tm_map(corpus, content_transformer(tolower))
# Remove digits.
corpus = tm_map(corpus, removeNumbers)
# Remove punctuation.
corpus = tm_map(corpus, removePunctuation)
# Remove stop words. 
corpus = tm_map(corpus, removeWords, stopwords())
# Stemming.
corpus = tm_map(corpus, stemDocument)
# Strip extra whitespaces from a text document
corpus = tm_map(corpus, stripWhitespace)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
# Take the most frequent words.
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked

# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred); cm

# Better than Naive Bayes.
(cm[4]+cm[1])/sum(cm)
