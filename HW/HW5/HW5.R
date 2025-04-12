x1 <- c(6,5,4,1,1,0)
x2 <- c(4,2,3,2,1,0)


plot(x1,x2,col='red')

x <- data.frame(x1,x2)

RNGkind(sample.kind = "default")
set.seed(2)
labels <- sample(2,nrow(x), replace = T)
labels

library(factoextra)
km.obj <- eclust(x, FUNcluster = "kmeans",k=2)


#question2
x1 <- c(48,54,55,60,62,61,56,55,51)

plot(x1)
set.seed(1)
km.obj <- eclust(x1,FUNcluster = "kmeans", k=3,nstart=20)

km.obj$centers
km.obj$cluster


#question 3
set.seed(1)
x1 <- sample(1:20, replace = F)
x2 <- sample(1:20, replace = F)
x <- data.frame(x1,x2)

km3.obj <- eclust(x, FUNcluster = "kmeans", k=3, nstart=20)
km5.obj <- eclust(x, FUNcluster = "kmeans", k=5, nstart=20)

#part b & c
km4.obj <- eclust(x, FUNcluster = "kmeans", k=4, nstart=50)
km4.obj$tot.withinss
km5.obj <- eclust(x, FUNcluster = "kmeans", k=5, nstart=50)
km5.obj$tot.withinss
km6.obj <- eclust(x, FUNcluster = "kmeans", k=6, nstart=50)
km6.obj$tot.withinss

#part d

wss <- numeric(10)
set.seed(1)
  for(i in 1:10){
  km <- eclust(x,FUNcluster = "kmeans", k=i, nstart = 50)
    wss[i] <- km$tot.withinss
}

print(wss)

plot(1:10, wss,type="b", pch=19,
     xlab = "Number of Clusters",
    ylab = "Total Within-cluster sum of Squares",
    main = "Elbow Plot")
#part e
set.seed(1)

fviz_nbclust(
  x,
  kmeans,
  method = "gap_stat",
  k.max  = 10,
  nboot  = 50
)+labs(subtitle="Gap Stat")


#Question 4

library(ISLR)
library(factoextra)


data(iris)

head(iris)

features_iris <- iris[,-5]
predictor_iris <- iris[,5]

wss <- numeric(10)
for (i in 1:10){
  set.seed(1)
km1_10.obj <- eclust(features_iris,k=i,FUNcluster="kmeans", nstart = 50)

wss[i] <- km1_10.obj$tot.withinss
}
print(wss)
plot(1:10, wss,type="b", pch=19,
     xlab = "Number of Clusters",
    ylab = "Total Within-cluster sum of Squares",
    main = "Elbow Plot")

set.seed(1)

fviz_nbclust(
  features_iris,
  kmeans,
  method = "gap_stat",
  k.max  = 10,
  nboot  = 50
)+labs(subtitle="Gap Stat")



#Question 5
library(factoextra)
raw <- read.csv("C:/Visual_Studio/GithubRepo/MATH_4323/MATH-4323-Statisical-Learning/HW/HW5/Ch10Ex11.csv",header=F)
data <- t(raw)

dim(data)
wss <- numeric(10)
for (i in 1:10){
km.obj <- eclust(data, k=i, FUNcluster = "kmeans", nstart = 50)
wss[i] <- km.obj$tot.withinss
}
plot(1:10, wss, type="b", pch =19,
     xlab ="Number of Clusters",
     ylab = "Total Within-cluster sum of Squares",
     main = "Elbow Plot")

fviz_nbclust(
  data,
  kmeans,
  method="gap_stat",
  k.max=10,
  nboot=20
)+labs(subtitle = "Gap Stat")
