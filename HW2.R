install.packages("mlbench")

library(mlbench)
library(caret)
library(class)

data("Ionosphere")#imports the dataset

df = Ionosphere
#Question3 A
head(df)

tail(df)

table(df$Class)
prop.table(table(df$Class))

summary(df)

#Graphical Summaries
plot(df)

boxplot(df,main = "Boxplot of Ionosphere")

barplot(table(Ionosphere$Class), main="Class Distribution", col = c('Blue', 'Orange'))

pairs(df[,3:32])


#Question 3.b

df = df[,-c(2)] #Removes the 2nd column
head(df)#checks if the column has been deleted

df$Class <- as.factor(df$Class) #converts the last column into numeric

set.seed(1)
trainIndex <- createDataPartition(df$Class, p=.8, list = FALSE) #creates a index

train <- df[trainIndex,]
test <- df[-trainIndex,]

x.train <- train[,-ncol(train)]
y.train <- train$Class

x.test <- test[,-ncol(test)]
y.test <- test$Class

set.seed(1)

knn.pred <- knn(train = x.train, test=x.test, cl=y.train, k=1) #creates a KNN with k=1

mean(knn.pred != y.test) #gets the error

#Confusion Matrix
table(knn.pred, y.test)

#3.D

set.seed(4323)
trainIndex <- createDataPartition(df$Class, p=.7, list = FALSE) #creates a index

train <- df[trainIndex,]
test <- df[-trainIndex,]

x.train <- train[,-ncol(train)]
y.train <- train$Class

x.test <- test[,-ncol(test)]
y.test <- test$Class

set.seed(4323)

knn.pred <- knn(train = x.train, test=x.test, cl=y.train, k=3) #creates a KNN with k=3

mean(knn.pred != y.test) #gets the error

#Confusion Matrix
table(knn.pred, y.test)


set.seed(4323)
trainIndex <- createDataPartition(df$Class, p=.7, list = FALSE) #creates a index

train <- df[trainIndex,]
test <- df[-trainIndex,]

x.train <- train[,-ncol(train)]
y.train <- train$Class

x.test <- test[,-ncol(test)]
y.test <- test$Class

set.seed(4323)

knn.pred <- knn(train = x.train, test=x.test, cl=y.train, k=5) #creates a KNN with k=5

mean(knn.pred != y.test) #gets the error

#Confusion Matrix
table(knn.pred, y.test)

set.seed(4323)
trainIndex <- createDataPartition(df$Class, p=.7, list = FALSE) #creates a index

train <- df[trainIndex,]
test <- df[-trainIndex,]

x.train <- train[,-ncol(train)]
y.train <- train$Class

x.test <-test[,-ncol(test)]
y.test <- test$Class

set.seed(4323)

knn.pred <- knn(train = x.train, test=x.test, cl=y.train, k=7) #creates a KNN with k=7

mean(knn.pred != y.test) #gets the error

#Confusion Matrix
table(knn.pred, y.test)


#Question 4
# Load necessary libraries
library(ISLR)
library(caret)
library(class)


Auto_df = Auto

median_mpg <- median(Auto$mpg)


Auto$mpg01 <- ifelse(Auto$mpg > median_mpg, 1, 0)

# Create a new data frame containing mpg01 and the other variables
Auto_new <- data.frame(Auto)

# View the first few rows of the new dataset
head(Auto_new)

pairs(Auto_new)

boxplot(Auto_new)

barplot(Auto_new$mpg01)

plot(Auto_new$weight,Auto_new$mpg01)
plot(Auto_new$displacement,Auto_new$mpg01)
plot(Auto_new$horsepower,Auto_new$mpg01)
# Set seed for reproducibility
set.seed(1)

# Create the partition index
trainIndex <- createDataPartition(Auto_new$mpg01, p = .7, list = FALSE)

# Split the dataset into training and testing sets
train <- Auto_new[trainIndex, ]  # Training data
test <- Auto_new[-trainIndex, ]

# Prepare the training features and response variable
x.train <- train[,c('horsepower', 'weight', 'displacement')]  # Features (all columns except the last)
y.train <- train$mpg01             # Response variable from training data

# Prepare the testing features and response variable
x.test <- test[, c('horsepower', 'weight', 'displacement')]      # Features (all columns except the last)
y.test <- test$mpg01               # Response variable from testing data

# Run KNN
set.seed(1)
#k=1
knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 1)

mean(knn.pred !=y.test)

set.seed(1)
#k=1
knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 3)

mean(knn.pred !=y.test)

set.seed(1)
#k=5
knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 5)

mean(knn.pred !=y.test)

set.seed(1)
#k=7
knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 7)

mean(knn.pred !=y.test)

#The lowest test error we recieved when k = 1,3,5,7,9 is when k=3 at a test error of .086 and k=5 at a test error of 0.086

#e
x.train.scale = scale(x.train)
x.test.scale = scale(x.test)

# Run KNN
knn.pred.scale <- knn(train = x.train.scale, test = x.test.scale, cl = y.train, k = 1)

mean(knn.pred.scale !=y.test)
set.seed(1)
knn.pred.scale <- knn(train = x.train.scale, test = x.test.scale, cl = y.train, k = 3)

mean(knn.pred.scale !=y.test)

knn.pred.scale <- knn(train = x.train.scale, test = x.test.scale, cl = y.train, k = 5)

mean(knn.pred.scale !=y.test)

knn.pred.scale <- knn(train = x.train.scale, test = x.test.scale, cl = y.train, k = 7)

mean(knn.pred.scale !=y.test)


#When K = 7, the test error is the most smallest!

#Question #5

#Dont need the training and testing sets!

median_mpg <- median(Auto$mpg)

Auto$mpg01 <- ifelse(Auto$mpg > median_mpg, 1, 0)
x = Auto[,c('horsepower', 'weight', 'displacement')]
y = Auto$mpg01

#When K =1
set.seed(1)
knn.cv.pred <- knn.cv(train = x, cl = y,k=1)
mean(knn.cv.pred != y)

set.seed(1)
#K=3
knn.cv.pred <- knn.cv(train = x, cl = y,k=3)
mean(knn.cv.pred != y)
set.seed(1)
#k=5
knn.cv.pred <- knn.cv(train = x, cl = y,k=5)
mean(knn.cv.pred != y)
set.seed(1)
#k=7

set.seed(1)
x.scale = scale(Auto[,c('horsepower', 'weight','displacement')])


#k=3 scaled values
set.seed(1)
knn.cv.pred.scale = knn.cv(train = x.scale, cl=y, k=3)

mean(knn.cv.pred.scale != y)
