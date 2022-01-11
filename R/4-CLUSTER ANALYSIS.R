setwd("C:/Users/Lucia/Desktop/Magistrale/Corsi/Machine Learning - Hands On/Mine")

# CLUSTER ANALYSIS

data = read.csv('Dataset/Mall_Customers.csv')

X = data[4:5]
set.seed(6)

# ELBOW METHOD.
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(X,i)$withinss)
plot(1:10, wcss, type = 'b', main = paste('Cluster of clients'))

# APPLY K-MEANS.
set.seed(29)
KM = kmeans(X,5, iter.max = 300, nstart = 10)

# VISUALIZE.
library(cluster)
clusplot(X,
         KM$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE)


# HIERARCHICAL CLUSTER.
dendogram = hclust(dist(X, method = 'euclidean'), method = 'ward.D')
plot(dendogram)

hc = hclust(dist(X, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)
clusplot(X,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE)
