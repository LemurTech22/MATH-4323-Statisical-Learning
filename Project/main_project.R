install.packages("tidyverse")
install.packages("ggplot")
install.packages("corrplot")
install.packages("e1071")
install.packages("class")
#Install Packages

#library Loading
library(ggplot2)
library(corrplot)
library(tidyverse)
library(e1071)
library(class)

#data loading

data <- read.csv("Project/synthetic_fraud_dataset.csv")

print("Five elements in the dataset")
head(data)

print("Summary of Data")
summary(data)

print(paste("There are",sum(is.na(data)),"null values"))

print(paste("There are",sum(duplicated(data)),"duplicated Values"))

#remove duplicated rows
data <- data[!duplicated(data), ]

#removed unnecessary columns
df2 <- data[ , !(names(data) %in% c("Transaction_ID", "User_ID"))]

ggplot(df2, aes(x=Transaction_Type))+
  geom_bar()

ggplot(df2, aes(x=Transaction_Type,y=Daily_Transaction_Count))+geom_boxplot()

data$Fraud_Label <- as.factor(data$Fraud_Label)

#selects the numeric columns in the dataset.
numeric_data <- df2[sapply(df2, is.numeric)]

head(numeric_data)
#added in plots
hist(numeric_data$Avg_Transaction_Amount_7d)
hist(numeric_data$Failed_Transaction_Count_7d, xlab="Average Failed Transactions 7D. ", ylab="Number of failed Transactions", main= "Number of Failed Transactions")
hist(data$Is_Weekend, xlab="Weekend", main="Number of Transactions on the Weekend")
hist(numeric_data$Card_Age, xlab="Card Age", main="Card Age in months")
hist(numeric_data$Risk_Score, xlab = "Risk Score", main = "Risk Scores")

barplot(table(data$Is_Weekend, data$Fraud_Label), beside = TRUE, xlab="Fraud Label (0 = Weekday, 1 = Weekend)", ylab="Weekend", main="Fraud on the weekend.")

boxplot(data$Failed_Transaction_Count_7d~ data$Fraud_Label, xlab="Fraud Label (0 = Not Fraud, 1 = Weekend)", ylab = "Failed Transactions", main = "Failed Transactions Fraud")
boxplot(numeric_data$Account_Balance~data$Fraud_Label, xlab="Fraud Label (0 = Not Fraud, 1 = Weekend)",ylab="Account Balances", main= "Balances on Fraud")
boxplot(data$Risk_Score~data$Fraud_Label, xlab = "Fraud Label (0 = Not Fraud, 1 = Weekend)", ylab="Risk Score", main = "Risk Score detecting Fraud")

#pairs(numeric_data)

#correlation matrix
correlation_matrix <-cor(numeric_data, use="complete.obs")
print(correlation_matrix)

#heatmap
heatmap(correlation_matrix, main="Correlation Heatmap", col=topo.colors(10), symm=TRUE)
corrplot(correlation_matrix, method="circle", type="lower", tl.col="black", tl.cex=0.8)

#correlation test
#cor_test <- cor.test(numeric_data$Transaction_Amount, numeric_data$Account_Balance)
#print(cor_test)

# Exclude self-correlations (diagonal values)
strong_correlations <- which(abs(correlation_matrix) > 0.5 & row(correlation_matrix) != col(correlation_matrix), arr.ind = TRUE)
strong_correlations <- strong_correlations[strong_correlations[, 1] < strong_correlations[, 2], ]
print(strong_correlations)

# Display variable pairs with high correlation values
if (is.null(dim(strong_correlations)) || nrow(strong_correlations) == 0) {
  cat("No variable pairs found with absolute correlation > 0.5\n")
} else {
  # Loop through and print correlations
  for (i in 1:nrow(strong_correlations)) {
    var1 <- rownames(correlation_matrix)[strong_correlations[i, 1]]
    var2 <- colnames(correlation_matrix)[strong_correlations[i, 2]]
    correlation_value <- correlation_matrix[var1, var2]
    cat(paste(var1, "and", var2, "have a correlation of", round(correlation_value, 2), "\n"))
  }
}


#plot(data$Failed_Transaction_Count_7d,data$Risk_Score)
#splitting data up!

# 1. Split
set.seed(123)

train_idx <- sample(1:nrow(numeric_data), 0.8 * nrow(numeric_data))
x <- numeric_data[, -which(names(numeric_data) == "Fraud_Label")]
y <- numeric_data$Fraud_Label

train_x <- x[train_idx, ]
test_x  <- x[-train_idx, ]
train_y <- y[train_idx]
test_y  <- y[-train_idx]

#plot(x)


# Scale
scaled_train_x <- scale(train_x)
head(scaled_train_x)
#plot(scaled_train_x)
colnames(scaled_train_x) <- colnames(train_x)

train_data <- data.frame(scaled_train_x, Fraud_Label = as.factor(train_y))


# Scale test data properly
train_mean <- attr(scaled_train_x, "scaled:center")
train_sd   <- attr(scaled_train_x, "scaled:scale")

scaled_test_x <- scale(test_x, center = train_mean, scale = train_sd)
test_data <- data.frame(scaled_test_x)

# Train SVM
#compare linear kernel

svmfit_linear <- svm(Fraud_Label ~ ., data = train_data, kernel = "linear", gamma = .3)

#radial kernel
svmfit_radial <- svm(Fraud_Label ~ ., data = train_data, kernel = "radial", gamma = .3)

# Plot
plot(svmfit_linear, train_data, Failed_Transaction_Count_7d ~ Risk_Score,
     xlab = "Risk Score", ylab = "Failed Transactions", main = "SVM Model with Linear Kernel")

plot(svmfit_radial, train_data, Failed_Transaction_Count_7d ~ Risk_Score,
     xlab = "Risk Score", ylab = "Failed Transactions", main = "SVM Model with Radial Kernel")

# Predict
predictions_linear <- predict(svmfit_linear, newdata = test_data)
predictions_radial <- predict(svmfit_radial, newdata = test_data)

# Evaluate
test_y <- as.factor(test_y)

conf_matrix_linear <- table(Predicted = predictions_linear, Actual = test_y)
conf_matrix_radial <- table(Predicted = predictions_radial, Actual = test_y)
print("Confusion Matrix: SVM with Linear Kernel:")
print(conf_matrix_linear)

print("Confusion Matrix: SVM with Radial Kernel:")
print(conf_matrix_radial)

accuracy_linear <- mean(predictions_linear == test_y)
accuracy_radial <- mean(predictions_radial == test_y)
print(paste("Accuracy for SVM Linear Kernel: ",accuracy_linear))
print(paste("Accuracy for SVM Radial Kernel: ", accuracy_radial))

#Build KNN model

# Run KNN
knn_predictions <- knn(train = scaled_train_x, test = scaled_test_x, cl = train_y, k = 5)  # k = 5

# Evaluate
conf_matrix_knn <- table(Predicted = knn_predictions, Actual = test_y)
print(conf_matrix_knn)

knn_accuracy <- mean(knn_predictions == test_y)
print(paste("KNN Accuracy:", round(knn_accuracy * 100, 2), "%"))

accuracies <- sapply(1:20, function(k) {
  knn_predictions <- knn(train = scaled_train_x, test = scaled_test_x, cl = train_y, k = k)
  mean(knn_predictions == test_y)
})

#KNN finding the best K
best_k <- NA
best_accuracy <- 0
accuracies <- c()

for (i in 1:20){
  set.seed(123+i)
  train_idx <- sample(1:nrow(numeric_data), 0.8 * nrow(numeric_data))
  x <- numeric_data[, -which(names(numeric_data) == "Fraud_Label")]
  y <- numeric_data$Fraud_Label
  
  train_x <- x[train_idx, ]
  test_x  <- x[-train_idx, ]
  train_y <- y[train_idx]
  test_y  <- y[-train_idx]
  scaled_train_x <- scale(train_x)
  scaled_test_x <- scale(test_x, center = attr(scaled_train_x, "scaled:center"),
                         scale = attr(scaled_train_x, "scaled:scale"))
  train_data <- data.frame(scaled_train_x, Fraud_Label = as.factor(train_y))
  
  set.seed(123)
  knn.pred<- knn(train = scaled_train_x, test=scaled_test_x, cl = train_y, k=i)
  print(table(knn.pred, test_y))
  
  cm<- table(knn.pred, test_y)
  accuracy <- sum(diag(cm))/sum(cm)
  accuracies[i] <- accuracy
  if (accuracy > best_accuracy){
    best_accuracy <- accuracy
    best_k <- i
  }
  cat("k =", i, "| Accuracy =", round(accuracy, 4), "\n")
}
cat("\nBest k:", best_k, "with accuracy:", round(best_accuracy, 4), "\n")
