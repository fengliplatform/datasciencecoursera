#c6 2 time

#variance

# Following exmaple is to generate population and draw samples to estimate 
# population distribution mean and variance from sample mean distribution.

# Generate a population with mean = 0 and sd = 1(variance = 1) using rnorm,
popu <- rnorm(nosim * n)
mean.population  <- 0
sd.population <- 1
var.population <- sd.population ^ 2
popu

# resample for 1000 times
# we split them to 1000 rows representing 1000 samples.
nosim <- 1000
# each sample has size of 10
n <- 10
mt <- matrix(popu, nrow=nosim)
dim(mt)
head(mt)

# calculate means for each sample
sample.means <- apply(mt, 1, mean)
sample.means

sample.vars <- apply(mt, 1, var)
sample.vars
# in theory: expected value of sample variances is population variance
population.var <- mean(sample.vars)
population.var
# 1.02

# 1. mean of sample means to estimate mean of population (0)
mean.sample.means <- mean(sample.means)
mean.sample.means
# 0

# 2 variance of sample means to estimate variance of population
# 2.1 calculate sd for the distribution of sample means
sd.sample.means <- sd(sample.means)
sd.sample.means
# 0.3212
# var.sample.means <- sd.sample.means ^ 2
var.sample.means <- var(sample.means)
var.sample.means
# 0.1031
# according to formula: variance of sample means = variance of population / n
# popolation sd=1. so population variance is 1 ^ 2, 1.
# so from fomular
1 / 10
# 0.1

# the other way
var.sample.means <- var.population / n
sd.sample.means <- sqrt(var.sample.means)
sd.sample.means
#0.31
# we call sd as statistical standard error
# se formula
se.sample.means <- sd.population / sqrt(n)
se.sample.means
# 0.31

# so, sd of sample mean is the same as se of the sample mean



####################
# different variance with same mean
library(ggplot2)
xvals <- seq(-10, 10, by = .01)
dat <- data.frame(
    y = c(
        dnorm(xvals, mean = 0, sd = 1),
        dnorm(xvals, mean = 0, sd = 2),
        dnorm(xvals, mean = 0, sd = 3),
        dnorm(xvals, mean = 0, sd = 4)
    ),
    x = rep(xvals, 4),
    factor = factor(rep(1 : 4, rep(length(xvals), 4)))
)
ggplot(dat, aes(x = x, y = y, color = factor)) + geom_line(size = 2) 

#
library(ggplot2)
nosim <- 10000; 
dat <- data.frame(
    x = c(apply(matrix(rnorm(nosim * 10), nosim), 1, var),
          apply(matrix(rnorm(nosim * 20), nosim), 1, var),
          apply(matrix(rnorm(nosim * 30), nosim), 1, var)),
    n = factor(rep(c("10", "20", "30"), c(nosim, nosim, nosim))) 
)
ggplot(dat, aes(x = x, fill = n)) + geom_density(size = 2, alpha = .2) + 
    geom_vline(xintercept = 1, size = 2) 

# roll dies 
dat <- data.frame(
    x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                       nosim), 1, var),
          apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
                       nosim), 1, var),
          apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
                       nosim), 1, var)
    ),
    size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + 
    geom_histogram(alpha = .20, binwidth=.3, colour = "black") 
g <- g + geom_vline(xintercept = 2.92, size = 2)
g + facet_grid(. ~ size)


# plot son's height
install.packages("UsingR")
library(UsingR)
data(father.son)
x <- father.son$sheight
n<-length(x)

g <- ggplot(data = father.son, aes(x = sheight)) 
g <- g + geom_histogram(aes(y = ..density..), fill = "lightblue", binwidth=1, colour = "black")
g <- g + geom_density(size = 2, colour = "black")
g
round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)),2)

##############
# binomial trail

# draw from 8 balls. all red ball and while ball. probability of greater than 6 of red ball.
# the same the probability of greater than 6 of white ball.
pbinom(6, size=8, prob=0.5, lower.tail=FALSE)


##############
#Asymptotics

# random normal distribution numbers
n <- 10000
means <- cumsum(rnorm(n))/(1:n)
library(ggplot2)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g

# coin flip
means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g

# confident interval
rbinom(10, prob=0.5, size=100)
?rbinom


## khanacademy
# https://www.khanacademy.org/math/statistics-probability/confidence-intervals-one-sample/estimating-population-mean/v/confidence-interval-1
p.le.100 <- pnorm(100, mean=112, sd=40)
p.le.124 <- pnorm(124, mean=112, sd=40)
p.between.100.124 <- p.le.124 - p.le.100
p.between.100.124

