library(ggplot2)
library(corrplot)

data <- read.csv("Project/synthetic_fraud_dataset.csv")

print("Five elements in the dataset")
head(data)

print("Summary of Data")
summary(data)

is.null(data)

print(paste("There is",sum(is.na(data)),"null values"))

print(paste("There is",sum(duplicated(data)),"duplicated Values"))

#removed unnecessary columns
df2 <- data[ , !(names(data) %in% c("Transaction_ID", "User_ID"))]

ggplot(df2, aes(x=Transaction_Type))+
  geom_bar()

ggplot(df2, aes(x=Transaction_Type,y=Daily_Transaction_Count))+geom_boxplot()


#selects the numeric columns in the dataset.
numeric_data <- df2[sapply(df2, is.numeric)]

head(numeric_data)

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
print(strong_correlations)

# Display variable pairs with high correlation values
if (nrow(strong_correlations) == 0) {
  cat("No variable pairs found with absolute correlation > 0.5\n")
} else {
  # Display variable pairs with high correlation values
  for (i in 1:nrow(strong_correlations)) {
    var1 <- rownames(correlation_matrix)[strong_correlations[i, 1]]
    var2 <- colnames(correlation_matrix)[strong_correlations[i, 2]]
    correlation_value <- correlation_matrix[var1, var2]
    cat(paste(var1, "and", var2, "have a correlation of", round(correlation_value, 2), "\n"))
  }
}

