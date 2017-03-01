#C5w1

library(kernlab)
data(spam)
dim(spam)
names(spam)
str(spam)

set.seed(3435)
trainIndicator <- rbinom(4601, size=1, prob=0.5)
table(trainIndicator)
class(trainIndicator)
trainIndicator

trainSpam <- spam[trainIndicator == 1, ]
testSpam <- spam[trainIndicator == 0, ]

par(mar=c(4,4,1,1))

# plot categary
table(trainSpam$type)
boxplot(trainSpam$capitalAve)
boxplot(log10(trainSpam$capitalAve + 1))
#plot(trainSpam$capitalAve, trainSpam$type)
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type) # avoid log10(0)

# plot pair
plot(trainSpam[, 1:2])
plot(trainSpam[, 1], trainSpam[,2])
plot(trainSpam$make, trainSpam$address)

plot(log(trainSpam[, 1:4]))
plot(log(trainSpam[, 1:4] + 1))

# clustering
df <- data.frame(a=c(1:3), b=c(4:6))
df
t(df)

hCluster <- hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

#hCluster2 <- hclust(dist(trainSpam[,1:57]))
#plot(hCluster2)

hCluster3 <- hclust(dist(t(log10(trainSpam[,1:57]+1))))
plot(hCluster3)


# build model

fit <- glm(type ~ charDollar, data=trainSpam, family="binomial")

  
# predict
p <- predict(fit, testSpam)
p













