

# Reading the file

iris = read.csv("iris.csv")

head(iris)

# Removing the 1st and last column

iris = iris[, -c(1,6)]

head(iris)

# Removing rows with NA's
mydata = na.omit(iris)


# Scaling the data
mydata <- scale(mydata)


# Finding within sum of squares 

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

apply(mydata,2,var)

wss

# Plotting the elbow curve

for (i in 1:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Clusters",               #type="b" is line chart
     ylab="Within groups sum of squares")


# K-Means Cluster Analysis


fit <- kmeans(mydata, 3) # 3 clusters seems to be the correct option from the graph

fit$cluster                  # Which cluster is assigned to which row.



mydata = cbind(iris,fit$cluster)
head(mydata)

# get cluster means 
aggregate(iris,by=list(fit$cluster),FUN=mean)

# append cluster assignment
mydata <- data.frame(iris, fit$cluster)
head(mydata)
table(mydata[,5])

cluster1 = mydata[mydata$fit.cluster==1,]  # Cluster 1 dataframe
 
cluster2 = mydata[mydata$fit.cluster==2,]  # Cluster 2 dataframe

cluster3 = mydata[mydata$fit.cluster==3,]  # Cluster 3 dataframe

dim(cluster1)
 
table(mydata$fit.cluster)             #No.of observations in each of the cluster

## using Principal components to view the cluster formation
# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

  # Conclusion

# 3 clusters are a good choice



