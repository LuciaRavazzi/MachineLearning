
# ASSOCIATION RULE. 


# APRIORI ALGORITHM.
library(arules)

# read file.
data = read.csv('Dataset/Market_Basket_Optimisation.csv', header = FALSE)
# build transactions.
data =  arules::read.transactions('Dataset/Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(data)

# the first 10 product with the highest support.
itemFrequencyPlot(data, topN =10)

# Train apriori model.
rules = apriori(data = data, parameter = list(support = 0.003, confidence = 0.4, minlen = 2))

inspect(sort(rules, by = 'lift')[1:10])


# ECLAT ALGORITHM.
inspect(sort(rules, by = 'support')[1:10])
