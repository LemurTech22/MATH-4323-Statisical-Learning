library(factoextra)
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
x=data.frame(x)


hc.complete <- hclust(dist(x),method = "complete")

plot(hc.complete)

hc.complete.K <- cutree(hc.complete, k=2)
hc.complete.K

plot(x, col=(hc.complete.K+1),
     main="Hierarchical Clustering, K=2, Complete Linkage",
     xlab="", ylab="",
     pch=20, cex=2)


hc.complete.K <- cutree(hc.complete, k=4)
hc.complete.K

plot(x, col=(hc.complete.K+1),
     main="Hierarchical Clustering, K=4, Complete Linkage",
     xlab="", ylab="",
     pch=20, cex=2)

hc.single <- hclust(dist(x),
                    method="single")
plot(hc.single)

hc.single.K <- cutree(hc.single, k=2)
hc.single.K

plot(x,col=(hc.single.K+1), main = "Hierarchal Clustering,
     K=2, Single Linkage",
     xlab = "", ylab="",
     pch =20, cex=2)

hc.average <- hclust(dist(x),
                    method="average")
plot(hc.average)

hc.average.K <- cutree(hc.average, k=2)
hc.average.K

plot(x,col=(hc.average.K+1))

hc.out <- eclust(x, FUNcluster = "hclus",hc_method="complete")


fviz_nbclust(x,hcut,hc_method="complete")

hc.out <- eclust(x, FUNcluster="hclust",
                 hc_method ="complete")
print(hc.out$silinfo$avg.width)

hc.out=eclust(x,FUNcluster = "hclust",hc_method="single")

print(hc.out$silinfo$avg.width)

hc.out=eclust(x,FUNcluster = "hclust",hc_method="average")

print(hc.out$silinfo$avg.width)
