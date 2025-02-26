1(a) The scenario is a Classification and a Inference. the number of samples in the scenario is 20 and number of variables is total 14 variables.
1(b)
1(c)
1(d)


2(a)
2(b)
2(c)

3
#Question 4.1
library(readr)
Credit_df <- read_csv("HW/HW1/Credit.csv")
View(Credit)
summary(Credit_df)

#Question 4.2 The Columns in the Credit_df Dataset containing numerical values are Income, Limit, Rating, Cards, Age, Education, Balance. The Categorical Variables in the Credit_df Dataset are Own, Student, Married and Region.
?Credit_df

#Question 4.3
Credit_df_num_df = subset(Credit_df, select = -c(Own, Student, Married, Region))
summary(Credit_df_num_df)#checks if the columns have been deleted
pairs(Credit_df_num_df)



#Question 4.4
Credit_df$Student = as.factor(Credit_df$Student)
str(Credit_df$Student)

plot(Credit_df$Balance ~ Credit_df$Student, 
        main = "Balance by Student Status",
        xlab = "Student Status",
        ylab = "Balance")

#Question 4.5
Credit_df$high = ifelse(Credit_df$Rating>680, "Yes", "No")
table(Credit_df$high)
boxplot(Credit_df$Balance ~ Credit_df$high, main = "Balance by High Rating", xlab = "High (Yes/No)", ylab = "Balance")

#Question 4.6 Ask Professor!!!
par(mfrow = c(2,2))
hist(Credit_df$Income, breaks = 5, main = "Income (5 bins)", xlab = "Income")
hist(Credit_df$Income, breaks =10, main = "Income (10 bins)", xlab = "Income")
hist(Credit_df$Income, breaks =15, main = "Income (15 bins)", xlab = "Income")
hist(Credit_df$Income, breaks =20, main = "Income (20 bins)", xlab = "Income")
par(mfrow=c(1,1))
#Question 5
library(MASS)
Boston
?Boston
#Answer: 
The Boston Data contains 506 rows with 14 columns. The meaning behind lstat indicate the status of the population based percent. Ptratio is the pupil-teacher ration by town. chas is a categorical variable and it indicate the bounds is surrounds a river
Medv is the average value of occupied homes in $1000.
max(Boston$tax)-min(Boston$tax)

Boston_df = Boston[-c(50:100)]

summary(Boston_df)
range(Boston_df$crim)
pairs(Boston_df)

#5.g ASK Professor
summary(Boston_df$crim)
high_crim <- Boston_df[Boston_df$crim > 10,]
high <- Boston_df[which.max(Boston_df$crim), ]
low <- Boston_df[which.min(Boston_df$crim),]

print(high_crim)
print(high)
print(low)


summary(Boston$tax)


high_tax <- Boston_df[which.max(Boston_df$tax),]
low_tax <- Boston_df[which.min(Boston_df$tax),]

print(high_tax)
print(low_tax)


summary(Boston$ptratio)

high_ptratio <- Boston_df[which.max(Boston_df$ptratio),]
low_ptratio <- Boston_df[which.min(Boston_df$ptratio),]

print(high_ptratio)
print(low_ptratio)

#Question 5.h
num_bounding <- sum(Boston_df$chas ==1)
num_bounding

#Question 5.i
pairs(Boston_df)
