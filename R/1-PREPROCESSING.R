
# IMPORT DATASET.

dataset = read.csv('Dataset/Data.csv')

dataset

# MISSING DATA

dataset$Age = ifelse(is.na(dataset$Age), mean(dataset$Age, na.rm = TRUE), dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary), mean(dataset$Salary, na.rm = TRUE), dataset$Salary)
dataset

# ENCODING CATEGORICAL DATA.

# In place of encoding the categorical field, it's enough to use factor.
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels=c(1,2,3))

dataset$Purchased = factor(dataset$Purchased,
                         levels = c('No', 'Yes'),
                         labels=c(0,1))

# SPLITING DATASET INTO TRAIN AND TEST SET.
# install.packages('caTools')
library(caTools)

set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)


train = dataset[split,]
test = dataset[!split,]


# FEATURE SCALING.
# A lot of ML algorithms relies on the euclidean distance which is dominated by 
# high values. So, it seems that some features don't exist. 

# only the train set.
train[,2:3] = scale(train[,2:3])

# In order to avoid the leaking problem of scale(test[,2:3])
install.packages('caret')
library(caret)

normParam = preProcess(train[,2:3])
norm = predict(normParam, test[,2:3])
















