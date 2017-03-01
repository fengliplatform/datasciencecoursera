# c4w3

# Hierachical clustering - !!Really useful for high dimentional dataset with heatmap!!
# bottom up method. find close points, merge and find next closest point, merge to get super point

# require 1. distance definition, 2 merge approch
# produces: tree

# distance
# 1. Euclidean distance: direct line from a to b. sqrt of lattitude differences square and longitude differences
# 2. Manhattan distance: estimat of the direct line. "travel along the city blocks"

#### resolve a plotting error
#> plot(c(1,2,3,4), c(10,20,30,40))
#Error in plot.new() : figure margins too large
#> par(mfrow=c(1,1))
#> plot(c(1,2,3,4), c(10,20,30,40))

# example
set.seed(1234)
p <- par() # save current par setting to be able to get back after folowing plotting and changes
par(mar=c(0,0,0,0))
par(p)

x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
x

#rep(c(1,2,1), each=4)
#rep(c(1,2,1), 4)

y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.3)
y

plot(x, y, col="blue", pch=19, cex=2)
text(x+0.05, y+0.05, labels=as.character(1:12))

df <- data.frame(x=x, y=y)
d <- dist(df)
d
class(d)
hcluster <- hclust(d)
class(hcluster)
plot(hcluster)

# heatmap()
set.seed(134)
data.m <- as.matrix(df)[sample(1:12),]
data.m
heatmap(data.m)

par(p)

##############
#
set.seed(12345)
p <- par()
par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400), nrow=40)
dataMatrix
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

heatmap(dataMatrix)

# add a pattern
set.seed(678910)
for (i in 1:40) {
  coinFlip <- rbinom(1, size=1, pro=0.5)
  if(coinFlip) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each=5)
  }
}
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

heatmap(dataMatrix)

#
hh <- hclust(dist(dataMatrix))
plot(hh)

dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, xlab="row mean", ylab="row", pch=19)
plot(colMeans(dataMatrixOrdered), xlab="col", ylab="col mean", pch=19)


# demension reduction
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, xlab="Row", ylab="First left sigular vector", pch=19)
plot(svd1$u[,1], xlab="Column", ylab="First right sigular vector", pch=19)


par(p)
par(mfrow=c(1,1))








