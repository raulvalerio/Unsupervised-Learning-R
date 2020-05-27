## We compare three different unsupervised clustering algorithms
## using three different datasets

library(dbscan)  # For dbscan and hdbscan

##---------- Part 1----------------


data("iris")
table(iris[,5])

pairs(iris[, -5], col = iris[, 5], pch = 19)  # 3 different groups

dist(iris[1:8,1:4])


## dbscan

dbscan::kNNdistplot(iris[,-5], k =  3)  # k: number fo neighbors
abline(h = 0.5, lty = 2)

clustdb1 = dbscan(iris[,1:4],0.55,3 ) #dbscan(data, eps, minpts for core point )
clustdb1

clustdb1$cluster

plot(iris[,1:2],col=clustdb1$cluster+1 )  # sepal.lenght vrs sepal witdh
table(iris[,5],clustdb1$cluster)


## hdbscan
clusth1 = hdbscan(iris[,1:4],3)  # hdbscan( data, number of minimum points for cluster)
clusth1
clusth1$cluster

par(mfrow=c(1,2))
plot(clusth1$hc,main="Dendrogram for HDBSCAN -  Iris")    # # dendrogram
plot(clusth1)   ## simplified tree
par(mfrow=c(1,1))

plot(iris[,1:2],col= clusth1$cluster+1 )
table(iris[,5],clusth1$cluster)


## kmeans
## set.seeds(3)   -> we could use it

library(factoextra)
mydata<-scale(iris[,1:4])
fviz_nbclust(mydata, kmeans, method = "gap_stat")   # determine optimal number of clusters
clustkm1 <- kmeans(iris[,1:4],3 ) #kmeans(data, number of clusters )
clustkm1

clustkm1$cluster

plot(iris[,1:2],col=clustkm1$cluster+1 )
table(iris[,5],clustkm1$cluster)

## Comparative

par(mfrow=c(2,2))  # two columns, two rows
plot(iris[,1:2],col=iris[,5], main="Original sets - Iris" )
plot(iris[,1:2],col=clustdb1$cluster+1,main="DBSCAN" )
plot(iris[,1:2],col= clusth1$cluster+1,main="HDBSCAN" )
plot(iris[,1:2],col=clustkm1$cluster+1,main="K-means" )
par(mfrow=c(1,1))


pairs(iris[, -5], col= iris[, 5], pch = 19,main="Original - Iris")  # 3 different groups
pairs(iris[, -5], col= clustdb1$cluster+1,main="DBSCAN" )
pairs(iris[, -5], col= clusth1$cluster+1,main="HDBSCAN" )
pairs(iris[, -5], col= clustkm1$cluster,main="K-means" )

##Cluster validation:  external or internal?

####### -------- Part 2 ------------------ 

data("moons")        ## Moons

dist(moons[1:10,])


## dbscan
dbscan::kNNdistplot(moons, k =  4)  # k: number fo neighbors
abline(h = 0.35, lty = 2)

clustdb2 = dbscan(moons,0.4,4 ) #dbscan(data, eps, minpts for )
clustdb2

clustdb2$cluster

plot(moons,col=clustdb2$cluster+1 )


## hdbscan

clusth2 = hdbscan(moons,7)  # hdbscan( data, number of minimum points for cluster)
clusth2
clusth2$cluster

plot(moons,col= clusth2$cluster+1 )


par(mfrow=c(1,2))
plot(clusth2$hc,main="Dendrogram for HDBSCAN -  Moons")    # # dendrogram
plot(clusth2)   ## simplified tree
par(mfrow=c(1,1))


## kmeans

library(factoextra)
mydata<- scale(moons)
fviz_nbclust(mydata, kmeans, method = "gap_stat")   # determine optimal number of clusters

clustkm2 = kmeans(moons,5 ) #kmeans(data, number of clusters )
clustkm2

clustkm2$cluster

plot(moons,col=clustkm2$cluster+1 )


## Comparative

par(mfrow=c(2,2))  # two columns, two rows
plot(moons, main="Original sets - Moons" )
plot(moons,col=clustdb2$cluster+1,main="DBSCAN" )
plot(moons,col= clusth2$cluster+1,main="HDBSCAN" )
plot(moons,col=clustkm2$cluster+1,main="K-means" )
par(mfrow=c(1,1))


## ----------  Part 3 -------
data("DS3")

clustdb3 = dbscan(DS3, 8, 10)
clusth3  = hdbscan(DS3,10)
clustkm3 = kmeans(DS3,6)

par(mfrow=c(2,2), new=FALSE)  # two columns, two rows
plot(DS3, main="Original sets - DS3" )
plot(DS3,col=clustdb3$cluster+1,main="DBSCAN" )
plot(DS3,col= clusth3$cluster+1,main="HDBSCAN" )
plot(DS3,col=clustkm3$cluster+1,main="K-means" )
par(mfrow=c(1,1))
