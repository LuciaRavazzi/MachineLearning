
# IT IS POSSIBLE TO DOWNLOAD A PACKAGE FROM CRAN AND INSTALL
# FROM INSTALL PACKAGE IN THE BOTTOM RIGHT PART OF GUI.
# YOU MUST SELECT THE .TGZ OPTION.

library(ElemStatLearn)
library(dplyr)
library(caTools)

setwd("C:/Users/Lucia/Desktop/Magistrale/Corsi/Machine Learning - Hands On/Mine")


# INITIAL SETUP.
dataset = read.csv('Dataset/Social_Network_Ads_R.csv')
dataset = dataset[,3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# split dataset.
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# scale.
# SECONDO ME LUI SCALA IN MODO SBAGLIATO: LO STA FACENDO IN
# MODO TOTALEMENTE SEPARATO.
training_set[,1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[,1:2])


#--------------------------------------- LOGISITIC REGRESSION

# perform the model.
LR = glm(formula = Purchased ~.,
                 family = binomial,
                 data = training_set)

# probability of prediction of being 1 (positive class).
prob_pred = predict(LR, 
        type = 'response', 
        newdata = test_set[-3]) # remove the last column.

y_pred = ifelse(prob_pred > 0.5, 1,0)

# confusion matrix.
CM = table(test_set[,3], y_pred)
ACC = (CM[1] +  CM[4])/(sum(CM))


# PLOT THE TRAINING SET AND THE CONTOUR REGION.
set = training_set
X1 = seq(from = set$Age %>% min() -1 , to = set$Age %>% max() +1, 0.01)
X2 = seq(from = set$EstimatedSalary %>% min() -1 , to = set$EstimatedSalary %>% max() +1, 0.01)
grid_set = expand.grid(X1, X2) %>% `colnames<-`(c('Age', 'EstimatedSalary'))
prob_set = predict(LR, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[,-3],
     main = 'LOGISTIC REGRESSION (TRAINING SET)',
     xlab = 'SALARY',
     ylab = 'YEARS',
     xlim = range(X1),
     ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid ==1, 'springgreen3', 'red'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))

# MA QUAL è LA SPIEGAZIONE DELLE ZONE COLORATE? 
# IL TRAINING SET SERBE PER MODELLARE LA FUNZIONE DI PROBABILITà. 
# IL PLOT DEFINISCE QUELLE REGIONI CHE SONO DDIRETTAMENTE COLLEGATE AD ESSA.
# INFATTI TUTTI I PUNTI SULLA LINEA RETTA SONO QUELLI TALI PER CUI LA 
# PROBABILITà DI ESSERE è DEL 50%. INFATTI LA MONOTONICITà
# DELLA FUNZIONE è VISIBILE DA COME LA REGIONE è STATA SPEZZATA.

# QUELLO CHE VEDIAMO è L'INTERSEZIONE DELLA FUNZIONE DI PROBABILITà CON IL 
# PIANO DEI PUNTI DEL TRAINING SET.

# GLI ERRORI SONO DOVUTI AL FATTO CHE I PUNTI NON SEGUONO UNA LINEARITà.

# TEST SET.
set = test_set
X1 = seq(from = set$Age %>% min() -1 , to = set$Age %>% max() +1, 0.01)
X2 = seq(from = set$EstimatedSalary %>% min() -1 , to = set$EstimatedSalary %>% max() +1, 0.01)
grid_set = expand.grid(X1, X2) %>% `colnames<-`(c('Age', 'EstimatedSalary'))
prob_set = predict(LR, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[,-3],
     main = 'LOGISTIC REGRESSION (TEST SET)',
     xlab = 'SALARY',
     ylab = 'YEARS',
     xlim = range(X1),
     ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid ==1, 'springgreen3', 'red'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))


# THE REGION REMAIN THE SAME BECAUSE THE PREDICTOR IS THE SAME 
# AS PREVIOUS.



#--------------------------------------- KNN

# perform the model.
library(class)
y_pred = knn(train = training_set[,-3],
             test = test_set[,-3],
             cl = training_set[,3],
             k=5)


# confusion matrix.
CM = table(test_set[,3], y_pred)
ACC = (CM[1] +  CM[4])/(sum(CM))

# PLOT TRAINING SET.

set = training_set
X1 = seq(from = set$Age %>% min() -1 , to = set$Age %>% max() +1, 0.01)
X2 = seq(from = set$EstimatedSalary %>% min() -1 , to = set$EstimatedSalary %>% max() +1, 0.01)
grid_set = expand.grid(X1, X2) %>% `colnames<-`(c('Age', 'EstimatedSalary'))
y_grid = knn(train = training_set[,-3],
               test = grid_set,
               cl = training_set[,3],
               k=5)
plot(set[,-3],
     main = ' K-NN (TRAINING SET)',
     xlab = 'SALARY',
     ylab = 'YEARS',
     xlim = range(X1),
     ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid ==1, 'springgreen3', 'red'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))


# PLOT THE TEST SET.

set = test_set
X1 = seq(from = set$Age %>% min() -1 , to = set$Age %>% max() +1, 0.01)
X2 = seq(from = set$EstimatedSalary %>% min() -1 , to = set$EstimatedSalary %>% max() +1, 0.01)
grid_set = expand.grid(X1, X2) %>% `colnames<-`(c('Age', 'EstimatedSalary'))
y_grid = knn(train = training_set[,-3],
             test = grid_set,
             cl = training_set[,3],
             k=5)
plot(set[,-3],
     main = 'K-NN (TEST SET)',
     xlab = 'SALARY',
     ylab = 'YEARS',
     xlim = range(X1),
     ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid ==1, 'springgreen3', 'red'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))


#--------------------------------------- LINEAR SUPPORT VECTOR MACHINES

# perform the model.
library(e1071)
SVM = e1071::svm(formula = Purchased ~.,
           data = training_set,
           type = 'C-classification',
           kernel = 'linear')


y_pred = predict(SVM, test_set[-3])

# confusion matrix.
CM = table(test_set[,3], y_pred); CM
ACC = (CM[1] +  CM[4])/(sum(CM)); ACC

# PLOT TRAINING SET.

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(SVM, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# PLOT THE TEST SET.

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(SVM, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


#--------------------------------------- NAIVE BAYES.

library(e1071)

# it's necessary to perform NB.
training_set$Purchased = factor(training_set$Purchased, levels = c(0,1) )

NB = e1071::naiveBayes(x = training_set[-3], 
                       y = training_set$Purchased)

y_pred = predict(NB, test_set[-3]); y_pred

# confusion matrix.
CM = table(test_set[,3], y_pred); CM
ACC = (CM[1] +  CM[4])/(sum(CM)); ACC

# PLOT TRAINING SET.

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(NB, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


# PLOT THE TEST SET.

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(NB, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


#--------------------------------------- DECISION TREE.
library(rpart)

DT = rpart::rpart(formula = Purchased ~.,
           data = training_set)


y_pred = predict(DT, newdata = test_set[-3], type = 'class')


# confusion matrix.
CM = table(test_set[,3], y_pred); CM
ACC = (CM[1] +  CM[4])/(sum(CM)); ACC

# PLOT TRAINING SET.

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(DT, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree Classification (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


# PLOT THE TEST SET.

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(DT, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree Classification (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



# we note less overfitting than the one which is in python. 

plot(DT)
text(DT)


#--------------------------------------- RANDOM FOREST
library(randomForest)
RF = randomForest(x = training_set[-3],
                  y = training_set$Purchased,
                  ntree = 10)


y_pred = predict(RF, newdata = test_set[-3], type = 'class')


# confusion matrix.
CM = table(test_set[,3], y_pred); CM
ACC = (CM[1] +  CM[4])/(sum(CM)); ACC

# PLOT TRAINING SET.

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(RF, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'RANDOM FOREST CLASSIFICATION (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


# PLOT THE TEST SET.

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(RF, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'RANDOM FOREST CLASSIFICATION (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))














