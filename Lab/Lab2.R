install.packages('ISLR')

library(ISLR)

head(Caravan)

dim(Caravan)

attach(Caravan)

summary(Caravan)
348/582
#Question 1
summary(Caravan[,1:5])

library(class)
?knn

X.train<- Caravan[,-86]
X.test<- Caravan[,-86]
y.train<- Caravan$Purchase
y.test<-Caravan$Purchase

set.seed(1)
knn.pred<-knn(train = X.train, test= X.test, cl = y.train, k=1)
knn.pred
head(knn.pred,n=10)
     
mean(knn.pred != y.test)
#KNN where k =1
set.seed(1)
knn.pred <- knn(train = X.train,X.test, cl = y.train, k=1)
mean(knn.pred != y.test)

#KNN where k = 3
set.seed(1)
knn.pred <- knn(train = X.train, test=X.test, cl = y.train,k=3)
mean(knn.pred != y.test)
summary(knn.predict)

#KNN where k=10
set.seed(1)
knn.pred <- knn(train =X.train, test=X.test, cl = y.train, k=10)
mean(knn.pred != y.test)
