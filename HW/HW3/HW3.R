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

#2A
# Load ggplot2
library(ggplot2)

# Define center and radii
center_x <- -1
center_y <- 2
radii <- c(3, 4)

# Repeat the center coordinates to match the length of radii
center_x_rep <- rep(center_x, length(radii))
center_y_rep <- rep(center_y, length(radii))

# Plot setup
plot(NA, xlim = c(-6, 4), ylim = c(-3, 7), xlab = "X1", ylab = "X2", asp = 1, main = "Concentric Circles")

# Draw circles
symbols(center_x_rep, center_y_rep, circles = radii, add = TRUE, inches = FALSE, lwd = 2)

# Mark center
points(center_x, center_y, col = "red", pch = 19, cex = 1.5)
#2b
# Generate points for circles
theta <- seq(0, 2*pi, length.out = 300)
small_x <- center_x + radii[1] * cos(theta)
small_y <- center_y + radii[1] * sin(theta)

large_x <- center_x + radii[2] * cos(theta)
large_y <- center_y + radii[2] * sin(theta)

# Fill the regions using polygon()
polygon(c(large_x, rev(small_x)), c(large_y, rev(small_y)), col = rgb(0,0,1,0.3), border = NA)  # Annular region (green)
polygon(small_x, small_y, col = rgb(0,1,1,0.3), border = NA)  # Inside small circle (blue)
polygon(c(large_x, rev(large_x + 10)), c(large_y, rev(large_y + 10)), col = rgb(1,0,0,0.3), border = NA)  # Outside large circle (red)

# Draw circles
symbols(rep(center_x, length(radii)), rep(center_y, length(radii)), circles = radii, add = TRUE, inches = FALSE, lwd = 2)

# Mark center
points(center_x, center_y, col = "Red", pch = 19, cex = 1.5)

# Add legend
legend("bottomright", legend = c("Inside Small Circle", "Between Circles", "Outside Large Circle"),
       fill = c(rgb(0,0,1,0.3), rgb(0,1,0,0.3), rgb(1,0,0,0.3)), border = NA)

text(-2, 3, expression((1 + X[1])^2 + (2 - X[2])^2 <= 9), cex = 1.2, col = "black")
text(0, 5, expression((16 >=1 + X[1])^2 + (2 - X[2])^2 >9), cex = 1.2, col = "purple")
text(2, 6, expression((1 + X[1])^2 + (2 - X[2])^2 >16), cex = 1.2, col = "black")

#3
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
data <- data.frame(x1 = c(1,3,4,2,4,4,1,1),
                   x2 = c(4,4,3,2,2,4,2,3),
                  Class = c("Blue", "Red", "Red", "Blue", "Red", "Red", "Blue", "Blue"))

ggplot(data,aes(x=x1, y=x2, color = Class))+geom_point(size=3)+scale_color_manual(values=c("Blue" = "blue", "Red" = "red"))+
  labs(title = "Maximal Margin Classifier with Decision Boundary",x=expression(X[1], y=expression(X[2]))+
  theme_minimal()+xlim(0,5)+ylim(0,5))
#3.2
# Define decision boundary function based on extracted coefficients
decision_boundary <- function(x) {
  return(-2 * x + 8)
}

# Plot the dataset and the decision boundary
ggplot(data, aes(x = x1, y = x2, color = Class)) +
  geom_point(size = 3) +  # Plot points
  stat_function(fun = decision_boundary, color = "black", linetype = "dashed") +  # Decision boundary
  scale_color_manual(values = c("Blue" = "blue", "Red" = "red")) +  # Custom colors
  labs(title = "Maximal Margin Classifier with Decision Boundary",
       x = expression(X[1]), y = expression(X[2])) +
  theme_minimal() +
  xlim(0, 5) + ylim(0, 5)  # Set axis limits

#3.3
upper_margin <- function(x){-2*x+9}
lower_margin <- function(x){-2*x+7}

#3.d
# Extract support vectors and rename columns to match dataset
library(ggplot2)
library(e1071)
library(dplyr)

# Define dataset with correct column names
data <- data.frame(
  x1 = c(1,3,4,2,4,4,1,1),
  x2 = c(4,4,3,2,2,4,2,3),
  Class = as.factor(c("Blue", "Red", "Red", "Blue", "Red", "Red", "Blue", "Blue"))
)

# ✅ Train the SVM Model
svm_model <- svm(Class ~ x1 + x2, data = data, kernel = "linear", cost = 1e5, scale = FALSE)

# ✅ Extract Support Vectors
support_vectors <- as.data.frame(svm_model$SV)
colnames(support_vectors) <- c("x1", "x2")  # Rename columns to match data

# ✅ Assign Class Labels to Support Vectors using dplyr::left_join
support_vectors <- left_join(support_vectors, data, by = c("x1", "x2"))

# ✅ Fix `labs()` Syntax
ggplot(data, aes(x = x1, y = x2, color = Class)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Blue" = "blue", "Red" = "red")) +
  labs(title = "Maximal Margin Classifier with Decision Boundary",
       x = expression(X[1]), y = expression(X[2])) +  # ✅ Fix here
  theme_minimal() +
  xlim(0,5) + ylim(0,5)

# ✅ Define decision boundary and margin functions
decision_boundary <- function(x) { -2 * x + 8 }
upper_margin <- function(x) { -2 * x + 9 }
lower_margin <- function(x) { -2 * x + 7 }

# ✅ Plot dataset with decision boundary and support vectors
ggplot(data, aes(x = x1, y = x2, color = Class)) +
  geom_point(size = 3) +
  geom_point(data = support_vectors, aes(x = x1, y = x2), size = 5, shape = 1, stroke = 1.5) +  # Support vectors
  stat_function(fun = decision_boundary, color = "black", linetype = "dashed", size = 1.2) +  # Decision boundary
  stat_function(fun = upper_margin, color = "black", linetype = "dotted") +  # Upper margin
  stat_function(fun = lower_margin, color = "black", linetype = "dotted") +  # Lower margin
  scale_color_manual(values = c("Blue" = "blue", "Red" = "red")) +
  labs(title = "Maximal Margin Classifier with Support Vectors",
       x = expression(X[1]), y = expression(X[2])) +
  theme_minimal() +
  xlim(0, 5) + ylim(0, 5)
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
plot(final, train_data, horsepower ~ weight)
plot(final, train_data, acceleration ~ weight)
plot(final, train_data, weight ~ acceleration)
plot(final, train_data, acceleration ~ horsepower)


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