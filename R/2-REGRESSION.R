



dataset = read.csv('Dataset/Salary_Data.csv')

library(caTools)
set.seed(123)
split = sample.split(dataset$YearsExperience, SplitRatio = 0.9)
train = dataset[split,]
test = dataset[!split,]



#----------------------------------------------- SIMPLE REGRESSION.

# the package that we'll use take care of scaling.

regressor = lm(formula = Salary ~ YearsExperience,
               data = train)

# the lower is the regression, the higher is the significance of the variable. 
summary(regressor)

# prediction.
y_pred = predict(regressor, test)

# visualization.
library(ggplot2)
ggplot() +
  geom_point(aes(x = train$YearsExperience, y = train$Salary),
             color = 'red') +
  geom_line(aes(x = train$YearsExperience, y = predict(regressor, train)))


ggplot() +
  geom_point(aes(x = test$YearsExperience, y = test$Salary),
             color = 'red') +
  geom_line(aes(x = train$YearsExperience, y = predict(regressor, train)))




#----------------------------------------------- MULTIPLE REGRESSION.

# import.
dataset = read.csv('Dataset/50_Startups.csv')

# encoding.
dataset$State = factor(dataset$State,
                         levels = c('California', 'New York', 'Florida'),
                         labels=c(1,2,3))

split = sample.split(dataset$Profit, SplitRatio = 0.8)

train = dataset[split,]
test = dataset[!split,]

regressor = lm(formula = Profit ~ .,
               data = train)

# one dummy is turn out automatically. 
summary(regressor)
# only one variable is statistically significant, i.e. only one 
# leads to the rejection of the null hypothesis. 


regressor = lm(formula = Profit ~ R.D.Spend,
               data = train)
summary(regressor)
Y_pred = predict(regressor, newdata = test)


# Implement the BACKWARD ELIMINATION with the dataset. 
library(dplyr)
lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset) %>% summary
 
lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
   data = dataset) %>% summary

lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
   data = dataset) %>% summary

lm(formula = Profit ~ R.D.Spend,
   data = dataset) %>% summary


#----------------------------------------------- Polynomial regression

dataset = read.csv('Dataset/Position_Salaries.csv')
dataset = dataset[,2:3]
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4

poly_reg = lm(formula = Salary ~ .,
              data = dataset) %>% summary()



#----------------------------------------------- SVR
library(e1071)

dataset = read.csv('Dataset/Position_Salaries.csv')
dataset = dataset[,2:3]
regressor = svm(formula = Salary  ~ ., data = dataset, type = 'eps')

predict(regressor, data.frame(Level = 6.5))

# THE FINAL PART OF THE GRAPH IS VERY DIFFERENT FROM THE MODEL
# BECAUSE THE SALARY OF THE CEO IS AN OUTLIER.


#----------------------------------------------- DECISION TREE.

# feature scaling isn't necessary also because the model isn't build on
# the computation of the euclidean distance. 

library(rpart)
library(ggplot2)
dataset = read.csv('Dataset/Position_Salaries.csv')
dataset = dataset[,2:3]

regressor = rpart(formula = Salary ~ ., data = dataset)
predict(regressor, data.frame(Level=6.5))

ggplot() +
  geom_point(aes(x=dataset$Level, y = dataset$Salary), color = 'red') +
  geom_line(aes(x=dataset$Level, y = predict(regressor, dataset)))

# In R, the result is due to the number of split. There is no split. 

# to fix the problem, insert a parameter.
x_vis = seq(from = min(dataset$Level), to = max(dataset$Level), 0.01) 

predict(regressor,data.frame(Level=x_vis))


# there are different leaves in which some points and other ones in which there is only one point.
# Add some points in order to drop the linear behaviour that isn't necessary. 
regressor = rpart(formula = Salary ~ ., data = dataset, control = rpart.control(minsplit=1))
ggplot() +
  geom_point(aes(x=dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x=x_vis, y = predict(regressor,data.frame(Level = x_vis))))


#----------------------------------------------- RANDOM FOREST.

library(randomForest)
dataset = read.csv('Dataset/Position_Salaries.csv')
dataset = dataset[,2:3]
regressor = randomForest(x = dataset[1], 
                         y = dataset$Salary, 
                         ntree = 100)

ggplot() +
  geom_point(aes(x=dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x=x_vis, y = predict(regressor,data.frame(Level = x_vis))))

# the more trees, the more the mean converge to the true value. 



# FINAL OBSERVATION.
# The estimation of the parameters is defined by different unit of measure.
# However, it's possible to compare them on the basis of the per unit of the regressor. 
# Coefficient change with the change of variables.


