library(ggplot2)
data <- read.csv("Project/synthetic_fraud_dataset.csv")
head(data)

hist(as.numeric(data$Transaction_Amount),
     main = "Histogram of Transaction Amounts",
     xlab = "Transaction Amount",
     col = "lightblue",
     border = "black")

boxplot(as.numeric(Transaction_Amount) ~ Fraud_Label, data = data,
        main = "Transaction Amount by Fraud Status",
        xlab = "Fraud (0 = No, 1 = Yes)",
        ylab = "Transaction Amount",
        col = c("lightgreen", "red"))

plot(as.numeric(data$Transaction_Amount), as.numeric(data$Transaction_Distance),
     main = "Amount vs Transaction Distance",
     xlab = "Transaction Amount",
     ylab = "Transaction Distance",
     pch = 19,
     col = "purple")
