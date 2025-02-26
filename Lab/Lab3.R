library(ISLR)

dist(Caravan[1:4,-86])
#Task 1
customer.mat <-matrix(c(80000,50,
                        82000,50,
                        80000,18),
                        ncol=2,
                        byrow=T)
customer.mat
dist(customer.mat)
#Task 2
scale_0=scale(customer.mat)

dist(scale_0)


#Task3
customer_2.mat <-matrix(c(80000,50,
                        82000,50,
                        80000,18,
                        70000,35,
                        60000,25),
                      ncol=2,
                      byrow=T)
scale_1=scale(customer_2.mat)
dist(scale_1)

#Task 4

library(ISLR)
X.train.standardized = scale(Caravan[,-86])
X.test.standardized = X.train.standardized
var(Caravan[,1])
var(Caravan[,2])

mean(Caravan[,1])
sd(Caravan[,1])

mean(X.train.standardized[,1])
sd(X.train.standardized[,1])

#task 5
library()
for(K in c(1,3,10){
  set.seed(1)
  knn.pred <- knn(train = X.train.standardized,
                  test = X.test.standardized,\
                  cl = y.train,
                  k = K)
}