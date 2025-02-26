x <- c(1,3,2,5)
x


x = c(1,6,2)
x 
y= c(1,4,3)
#Question 1
x+y

length(x)
length(y)

#Question2
x*y

x = matrix(data = c(1,2,3,4), nrow= 2, ncol = 2)

x

matrix(c(1,2,3,4),2,2,byrow = TRUE)
#Question 3
(matrix(c(1,2,3,4),2,2,byrow = TRUE))**-1

x = rnorm(50)
y = x+rnorm(50, mean=50, sd=.1)
#Question 4 
x = rnorm(10)
y = x+rnorm(10, mean=10, sd=.1)
y

x = rnorm(10)
y = x+rnorm(10, mean=10, sd=.1)
y
#The Results are not the same


#Question 5
set.seed(1)
rnorm(5)
rnorm(5)
set.seed(1)
rnorm(5)
#The sequences 1 and 3 are the same

set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

x = rnorm(100)
y = rnorm(100)
plot(x,y)
plot(x, y, xlab = "This is the x-axis", ylab = "this is the y-axis", main ="Plot of X vs Y")

x = seq(1,10)
x
x = 1:10
x
x = seq(-pi, pi, length=50)
x
#Answer 6 : 
[1] -3.14159265 -3.01336438 -2.88513611
[4] -2.75690784 -2.62867957 -2.50045130
[7] -2.37222302 -2.24399475 -2.11576648
[10] -1.98753821 -1.85930994 -1.73108167
[13] -1.60285339 -1.47462512 -1.34639685
[16] -1.21816858 -1.08994031 -0.96171204
[19] -0.83348377 -0.70525549 -0.57702722
[22] -0.44879895 -0.32057068 -0.19234241
[25] -0.06411414  0.06411414  0.19234241
[28]  0.32057068  0.44879895  0.57702722
[31]  0.70525549  0.83348377  0.96171204
[34]  1.08994031  1.21816858  1.34639685
[37]  1.47462512  1.60285339  1.73108167
[40]  1.85930994  1.98753821  2.11576648
[43]  2.24399475  2.37222302  2.50045130
[46]  2.62867957  2.75690784  2.88513611
[49]  3.01336438  3.14159265


A = matrix(1:16, 4,4)
A

A[2,3]
#Question 7 Answer: 
A[3,]



#Question 8