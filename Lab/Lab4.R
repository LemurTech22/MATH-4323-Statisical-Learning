library(ISLR)
library(class)


n <-nrow(Caravan)

RNGkind(sample.kind ="Rounding")

set.seed(1)

train <-sample(1:n,4822)

x.train <- Caravan[train, -86]
x.test <- Caravan[-train,-86]
y.train <- Caravan$Purchase[train]
y.test <- Caravan$Purchase[-train]

dim(x.train)

dim(x.test)


set.seed(1)
knn.pred <-knn(train = x.train, test = x.test, cl = y.train, k=1000)

mean(knn.pred != y.test)


table(knn.pred, y.test)
935 missclassified: 65  



set.seed(1)
knn.pred <-knn(train = x.train, test = x.test, cl = y.train, k=3)

mean(knn.pred != y.test)
table(knn.pred, y.test)




set.seed(1)
for(j in 1:10){
  train <- sample(1:n, 4822)
  x.train <- Caravan[train, -86]
  x.test <- Caravan[-train, -86]
  y.train <- Caravan[train]
  y.test <- Caravan[-train]
  
  set.seed(1)
  knn.pred <- knn(train = x.train,
                  test = x.test,
                  cl = y.train, 
                  k=1)
  print(table(knn.pred, y.test))
}

set.seed(1)
knn.cv.pred <- knn.cv(train = Caravan[,-86],
                      cl = Caravan$Purchase,
                      k=1)
table(knn.cv.pred, Caravan$Purchase)

set.seed(1)
knn.cv.pred <- knn.cv(train = Caravan[,-86],
                      cl = Caravan$Purchase,
                      k=3)
table(knn.cv.pred, Caravan$Purchase)



