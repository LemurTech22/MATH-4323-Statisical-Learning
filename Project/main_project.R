library(ggplot2)
data <- read.csv("Project/synthetic_fraud_dataset.csv")

head(data)

summary(data)

is.null(data)

barplot()

#removed unnecessary columns
df2 <- data[ , !(names(data) %in% c("Transaction_ID", "User_ID"))]

barplot(df2$Transaction_Amount)

ggplot(df2, aes(x=Transaction_Type))+
  geom_bar()

ggplot(df2, aes(x=Transaction_Type,y=Daily_Transaction_Count))+geom_boxplot()

