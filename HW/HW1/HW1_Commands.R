#Question 4
summary(Credit)

#Question 2 The Columns in the Credit Dataset containing numerical values are Income, Limit, Rating, Cards, Age, Education, Balance. The Categorical Variables in the Credit Dataset are Own, Student, Married and Region.
?Credit

#Question 3
Credit_num_df = subset(Credit, select = -c(Own, Student, Married, Region))
summary(Credit_num_df)#checks if the columns have been deleted
pairs(Credit_num_df)


#Question 4
plot(Credit$Student, Credit$Balance)


#Question 5
table(Credit$high)
boxplot(Credit$Balance ~ Credit$high, main = "Balance by High Rating", xlab = "High (Yes/No)", ylab = "Balance")

#Question 6