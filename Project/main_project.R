library(ggplot2)
install.packages("corrplot")
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


