data(iris)

head(iris)

iris

plot(Sepal.Width ~ Sepal.Length, data=iris,
     col = c("red", "blue", "green")[as.integer(Species)],
     pch = c(1,2,5)[as.integer(Species)])
legend(x="bottomright", legend=c("setosa", "versicolor", "virginica"),
       col= c("red","blue", "green"),
        pch = c(1,2,5))
install.packages("rgl")
library(rgl)
plot3d(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length,
type = "s",
size = 1,
xlab = "Sepal.Length",
ylab = "Sepal.Width",
zlab = "Petal.Length",
col = c("red","blue","green")[as.integer(iris$Species)])
legend3d("top",
pch=1,
cex = 0.8,
horiz = TRUE,
legend = levels(iris$Species),
col = c("red","blue","green"))

install.packages("GGally")
library(GGally)
ggpairs(iris[,1:4])

pc.out <- prcomp(iris[,-5], scale = T)
pc.out
pc.out$sdev
pc.out$sdev^2
sum(pc.out$sdev^2)
sum(pc.out$sdev^2)


pc.out$sdev^2/sum(pc.out$sdev^2)
0.729624454 +0.228507618
library(tidyverse)

ggpairs(as.tibble(pc.out$x))



biplot(pc.out)