library(e1071)
RNGkind(sample.kind = 'Rounding')
set.seed(1)
x1 <- rnorm(20)
x2 <- rnorm(20)
x <- cbind(x1,x2)
y <- c(rep(-1,10), rep(1,10))

clrs <- c(rep("blue", 10), rep("red",10))
plot(x, col = clrs)

x[y==1,] <- x[y==1,]+1 #color is blue and red 
plot(x,col =clrs)

clrs <- c(rep("green", 10), rep("black",10))#switches color
plot(x,col = clrs)

data = data.frame(x=x, y= as.factor(y))

svmfit=svm(y~., data = data,
           kernel='linear',
           cost = 10, 
           scale=FALSE)

plot(svmfit, data)

svmfit$index
summary(svmfit)

svmfit <- svm(y ~., data = data,
              kernel = "linear",
              cost = .1,
              scale = FALSE)
plot(svmfit, data)
svmfit$index
summary(svmfit)

#ls(model)
