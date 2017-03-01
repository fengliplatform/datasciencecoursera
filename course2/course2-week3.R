# course2-week3

# apply family

#lapply
x <- list(a=1:5, b=rnorm(10))
x
lapply(x, mean)
sapply(x, mean)

y <- 1:4
lapply(y, runif)


z <- 1:4
lapply(z, runif, min=0, max=10)

x <- list(a=matrix(1:4, 2,2), b=matrix(1:6, 3,2))
x
lapply(x, function(elt) elt[,1])

# apply
# to matrix row/col
x <- matrix(rnorm(200), 20,10)
x
# get col means
apply(x, 2, mean)
# get row menas
apply(x, 1, mean)
# get row sums
apply(x, 1, sum)

rowSums(x)
rowMeans(x)
colSums(x)
colMeans(x)

#
x <- matrix(rnorm(200), 20, 10)
apply(x, 1, quantile, probs=c(0.25, 0.75))


a <- array(rnorm(2*2*10), c(2,2,10))
a
class(a)
apply(a, c(1,2), mean)

#
mapply(rep, 1:4, 4:1)

noise <- function(n, mean, sd) {
  rnorm(n, mean, sd)
}
noise(5, 1, 2)

mapply(noise, 1:5,1:5,2)

# tapply: apply to part of the data
x <- c(rnorm(10), runif(10), rnorm(10,1))
x
f <- gl(3, 10)
f
tapply(x, f, mean)
tapply(x, f, mean, simplify=FALSE)
tapply(x, f, range)

split(x, f)
lapply(split(x,f), mean)


####
library(datasets)
head(airquality)

s <- split(airquality, airquality$Month)
s
lapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R","Wind","Temp")]))
sapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R","Wind","Temp")], na.rm=TRUE))

#
x <- rnorm(20)
f1 <- gl(2, 5)
f1
f2 <- gl(5, 2)
f2
interaction(f1,f2)

split(x, list(f1,f2))
str(split(x, list(f1,f2)))
str(split(x, list(f1,f2), drop=TRUE))


## debug
# warning
log(-1)

printmessage <- function(x) {
  if(is.na(x)) {
    print("x is missing value")
  } else  if (x > 0) {
    print("x is greater than zeor!")
  } else {
    print("x is less than zeor!")
  }
  invisible(x)
}
xx <- printmessage(5)
xx

printmessage(NA)

#1 traceback
mean(yyy)
traceback()

lm(y~x)
traceback()

#2 debug
debug(lm)
lm(y~x)

#3 recover
options(error=recover)
read.csv("nosuchfile")


#quiz
library(datasets)
data(iris)
str(iris)
head(iris)
s <- split(iris, iris$Species)
df <- s$virginica
df
means <- colMeans(df[,c("Sepal.Length", "Sepal.Width")])
round(means[1])
df[,"Sepal.Length", drop=F]
apply(df[,"Sepal.Length",drop=F], 2, mean)


apply(iris[, 1:4], 2, mean)


#
library(datasets)
data(mtcars)

tapply(mtcars$mpg, mtcars$cyl, mean)

sapply(split(mtcars$mpg, mtcars$cyl), mean)

head(mtcars)

means <- tapply(mtcars$hp, mtcars$cyl, mean)
round(abs(means[1] - means[3]))


debug(ls)
ls()


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
}


# sys.time
system.time(readLines("http://www.jhsph.edu"))
#> system.time(readLines("http://www.jhsph.edu"))
#user  system elapsed 
#0.047   0.000   1.502 


hilbert <- function(n) {
  i <- 1:n
  1 / outer(i-1, i, "+")
}
x <- hilbert(1000)
system.time(svd(x))
#> system.time(svd(x))
#user  system elapsed 
#4.154   0.000   3.668


#Rprof()

set.seed(1)
rpois(5, 2)


set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e


library(datasets)
data("airquality")
str(airquality)
Rprof()
fit <- lm( airquality$Ozone ~ airquality$Solar.R + airquality$Wind)
Rprof(NULL)
summaryRprof()














