# coursera
# DS
# R

#week1 quiz

setwd("~/course/ds_coursera")
mydf <- read.csv("hw1_data.csv")
names(mydf)

head(mydf,2)
dim(mydf)
mydf[152:153,]

mydf[47,"Ozone"]
complete.cases(mydf[,"Ozone"])

mydf2 <- mydf[is.na(mydf$Ozone), ]
dim(mydf2)

mydf3 <- mydf[!is.na(mydf$Ozone), ]
mydf3
mean(mydf3$Ozone)

mydf4 <- mydf3[mydf3$Ozone > 31,]
mydf4
mydf5 <- mydf4[mydf4$Temp > 90, ]
mydf5
mean(mydf5$Solar.R)

mydf6 <- mydf[mydf$Month == 6,]
mydf6
mean(mydf6$Temp)

mydf7 <- mydf[mydf$Month == 5,]
mydf7
max(mydf7$Ozone[!is.na(mydf7$Ozone)])

x <- 4
y <- if (x>3) { 10 } else {0}
y

for ( i in 1:10) {
  print(i)
}

x <- c("a", "b", "c", "d")
seq_along(x)
length(x)

for (i in 1:4) {
  print(x[i])
}

x <- matrix(1:6, 2,3)
x
seq_len(nrow(x))
for (i in seq_len(nrow(x))) {
  for (j in seq_len(ncol(x))) {
    print(x[i,j])
  }
}

?seq_along

seq(1, 10)
seq_along(10)
seq_len(10)

count <- 0
while (count < 10) {
  print(count)
  count <- count + 1
}

z <- 5
while (z >= 3 && z <= 10) {
  print(z)
  coin <- rbinom(1,1,0.5)
  if (coin == 1) {
    z <- z + 1
  } else {
    z <- z - 1
  }
}

add2 <- function(x, y) {
  x + y
}
add2(3, 5)

above10 <- function(x) {
  use <- x > 10
  x[use]
}
x <- c(1, 5, 12, 20)
above10(x)

above <- function(x, n = 10) {
  use <- x > n
  x[use]
}
x <- 1:20
above(x, 2)
above(x)

colmean <- function(x, removeNA=TRUE) {
  nc <- ncol(x)
  means <- numeric(nc)
  for (i in 1:nc) {
    means[i] <- mean(x[,i], na.rm = removeNA)
  }
  means
}
df <- data.frame(a=1:20, b=21:40)
df
colmean(df)
data(airquality)
colmean(airquality)
colmean(airquality, FALSE)


f <- function(a, b) {
  a^2
}
f(2)

search()


x <- Sys.time()
x
p <- as.POSIXlt(x)
p
class(p)
names(p)
names(unclass(p))
unclass(p)

?unclass

cube <- function(x, n) {
  x^3
}

cube(3)

x <- 1:10
if(x > 5) {
  x <- 0
}


f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

z <- 10
f(3)

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}
y



