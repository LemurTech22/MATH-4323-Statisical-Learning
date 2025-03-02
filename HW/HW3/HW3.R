#HW 3
#1
#Hyperplane eq  x1 - 2*x2 = 0

# Define test points
library(ggplot2)

# Define the range for X2
x2_vals <- seq(-4, 4, length.out = 100)

# Solve for X1 using X1 = 2 * X2
x1_vals <- 2 * x2_vals  

# Create a dataframe for the hyperplane
hyperplane_df <- data.frame(X1 = x1_vals, X2 = x2_vals)

# Define test points
test_points <- data.frame(
  X1 = c(0, 2, 0),
  X2 = c(2, 1, -2),
  Label = c("i", "ii", "iii")
)

# Plot the hyperplane and test points
ggplot() +
  geom_line(data = hyperplane_df, aes(x = X1, y = X2), color = "blue", size = 1.2) +  # Hyperplane
  geom_point(data = test_points, aes(x = X1, y = X2), color = "red", size = 3) +   # Test points
  geom_text(data = test_points, aes(x = X1, y = X2, label = Label), vjust = -1, size = 5) +  # Label points
  xlim(-4, 4) + ylim(-4, 4) +
  xlab(expression(X[1])) + ylab(expression(X[2])) +
  ggtitle(expression("Hyperplane: " ~ X[1] - 2 * X[2] ~ "= 0")) +
  theme_minimal()


#2
# Load necessary library (if you don't have it, install with install.packages("ggplot2"))
library(ggplot2)

# 1. Create data for the hyperplane
x1 <- seq(-5, 5, length.out = 100)  # Range of x1 values
x2 <- 2 - x1                      # x2 values based on the equation x1 + x2 - 2 = 0
hyperplane_data <- data.frame(x1, x2)

# 2. Create data for the points
points_data <- data.frame(
  x1 = c(1, -1, 2),
  x2 = c(1, -1, 1),
  class = c("on", "below", "above")  # Assign classes based on calculations
)

# 3. Create the plot using ggplot2
ggplot() +
  # Plot the hyperplane
  geom_line(data = hyperplane_data, aes(x = x1, y = x2), color = "blue", linewidth = 1) +
  # Plot the points
  geom_point(data = points_data, aes(x = x1, y = x2, color = class), size = 4) +
  # Customize the plot
  labs(
    title = "Hyperplane and Points",
    x = "X1",
    y = "X2",
    color = "Class"
  ) +
  xlim(-5, 5) +  # Adjust x-axis limits
  ylim(-5, 5) +  # Adjust y-axis limits
  theme_bw() # Optional: Use a black and white theme for better clarity


#3
#4A.
library(ISLR)
library(e1071)

df = Auto

median_mpg = median(df$mpg)

df$mpg01 = ifelse(Auto$mpg > median_mpg,1,0)

df$mpg <- NULL
head(df)
#B

df$name <- NULL#remove feature due to irrelevance

RNGkind(sample.kind ="Rounding")
set.seed(1)

df$mpg01 <- as.factor(df$mpg01)
cost_values <- c(0.001, 0.01, 0.1, 1, 5, 10, 100)

cv_errors <- numeric(length(cost_values))
training_idx = sample(1:nrow(df), .7*nrow(df))
train_data <- df[training_idx,]
test_data <- df[-training_idx,]

model <- tune(svm, mpg01~., data = train_data, kernel = "linear",
                ranges = list(cost = cost_values),
                tunecontrol = tune.control(cross = 5))


best_model <- model$best.model
best_cost <- model$best.parameters$cost
cv_errors <- model$performances$error

results <- data.frame(Cost = cost_values, CrossValidationError = cv_errors)
print(results)

#part c

final <- svm(mpg01~.,data =train_data, kernel = "linear", cost = best_cost)

print(final)

predictions <- predict(best_model, test_data)

conf_matrix <- table(Predicted = predictions, Actual = test_data$mpg01)
print(conf_matrix)

false_positives <- conf_matrix[2,1]
false_negatives <- conf_matrix[1,2]

print(paste("False Negatives: ",false_negatives))
print(paste("False Positives: ", false_positives))

#D
train_data$mpg01 <- as.factor(train_data$mpg01)
train_data$cylinders <- as.numeric(train_data$cylinders)
train_data$origin <- as.numeric(train_data$origin)

summary(train_data)
plot(final, train_data, displacement ~ horsepower)

#PLOT THE OPTIMAL SUPPORT VECTOR
#5
library(ISLR)
library(e1071)
df = OJ
head(df)


?OJ
#Question 5.a
set.seed(1)
train_idx <- sample(1:nrow(df),800)
train_data <- df[train_idx,]
test_data <- df[-train_idx,]


#5.b
model <- tune(svm, Purchase ~ ., data = train_data, kernel = "linear",
                ranges = list(cost = 0.01),
                tunecontrol = tune.control(cross = 5))

summary(model$best.model)
best_model = model$best.model

#5.c
predictions <- predict(best_model, test_data)
conf_matrix <- table(Predicted = predictions, Actual = test_data$Purchase)
print(conf_matrix)


accuracy = (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)

accuracy

testing_error = 1 - accuracy

print(paste("Accuracy: ", round(accuracy,4)))
print(paste("Testing Error: ", round(testing_error,4)))

#5.d
library(caret)
set.seed(1)
train_idx <- createDataPartition(df$Purchase, p = 800/nrow(df), list = FALSE)
train_data <- df[train_idx, ]
test_data <- df[-train_idx, ]

model_best_cost <- tune(svm,Purchase ~., data = train_data, kernel = "linear",
                        ranges = list(cost = c(0.01, 0.1, 1, 5, 10)),
                        tunecontrol = tune.control(cross=5))

summary(model_best_cost)

print(model_best_cost$best.parameters)

best_cost <- model_best_cost$best.parameters$cost

best_svm <- svm(Purchase ~., data = train_data, kernel="linear", cost = best_cost)
#training error rate
train_pred <- predict(best_svm, train_data)
train_conf_matrix <- table(Predicted = train_pred, Actual = train_data$Purchase)
train_accuracy <- (train_conf_matrix[1,1] + train_conf_matrix[2,2])/sum(train_conf_matrix)
train_error <- 1-train_accuracy

#testing error rate
test_pred <- predict(best_svm, test_data)
test_conf_matrix <- table(Predicted = test_pred, Actual = test_data$Purchase)
test_accuracy <- (test_conf_matrix[1,1]+test_conf_matrix[2,2])/sum(test_conf_matrix)
test_error <- 1- test_accuracy

print(paste("Best Cost: ", best_cost))
print(paste("Training Accuracy: ", round(train_accuracy,4)))
print(paste("Testing Accuracy: ", round(train_error,4)))

print(paste("test Accuracy: ", round(test_accuracy,4)))
print(paste("Testing Error: ", round(test_error,4)))