
library(factoextra)
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

x<-data.frame(x)

km.out <- eclust(x,
                 FUNcluster = "kmeans",
                  k=2,
                  nstart=20)

km.out$cluster


table(km.out$cluster)

apply(x[km.out$cluster==1,],2,mean)
apply(x[km.out$cluster==2,],2,mean)
plot(x,col=(4-km.out$cluster),
     main="k-Means Clustering Results with K=2",
     xlab = "",ylab="",
     pch=20,cex=2)

km.out

km.out$totss
km.out$withinss
km.out$tot.withinss
km.out$betweenss

km.out$totss-km.out$tot.withinss




km.out <- eclust(x,
                 FUNcluster="kmeans",
                  k=3,
                  nstart=20)
km.out

plot(x,col=km.out$cluster+1,
     main="K-means Clustering Results with K=3",
      xlab="",ylab="",
      pch=20,cex=2)
km.out


km.out=eclust(x, FUNcluster = "kmeans", k=3,
nstart=1)
km.out$tot.withinss
km.out=eclust(x, FUNcluster = "kmeans", k=3,
nstart=20)
km.out$tot.withinss

km.out=eclust(x, FUNcluster = "kmeans",
nstart=20,
nboot=50)
km.out

fvisz_nbclust(x,kmeans)