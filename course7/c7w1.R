# C7 W1
# 
library(ggplot2)
library(UsingR)

data(father.son)
str(father.son)
ggplot(father.son, aes(x=sheight)) + geom_histogram(fill="blue", color="black",binwidth=1) +
    geom_vline(xintercept=mean(father.son$sheight), size=2) +
    geom_density(father.son$sheight)


## scatter plot of parent/child height with frequency
library(dplyr)
#table(galton$child, galton$parent)
freqData <- as.data.frame(table(galton$child, galton$parent))
#freqData
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" ) # size of the points
#g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))  # color of the points
#g <- g + scale_colour_gradient(low = "lightblue", high="white") 

g <- g + geom_smooth(method="lm", formula=y~x)
g

## regression to the mean
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
rho
#mean(y) / mean(x)

library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
g = g + geom_abline(intercept = 0, slope = 1)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g


##
library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g


## Quiz
# Q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mu <- mean(x)
mu <- 0.0025
mu <- 0.1471
mu <- 0.3
mu <- 1.077

sum(w * ((x - mu) ^ 2))

# Q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
x.sd <- sd(x)
y.sd <- sd(y)

cor(x,y)
cor(y,x)

cor <- cor(x,y)
cor

slope <- cor * (y.sd / x.sd)
slope
# -1.723

ggplot(data.frame(x=x,y=y), aes(x, y)) + geom_point() +
    geom_abline(intercept=0, slope=slope)
lmfit <- lm(y ~ x)

summary(lmfit)
##
# Q3
data(mtcars)
head(mtcars)
lmfit <- lm(mpg ~ wt, mtcars)
summary(lmfit)

# Q4
# Consider data with an outcome (Y) and a predictor (X). The standard deviation of the predictor is one half that of the outcome. The correlation between the two variables is .5. What value would the slope coefficient for the regression model with Y as the outcome and X as the predictor?
# cor(x,y) = 0.5, sd(x)/sd(y) = 0.5
# slope=cor(x,y) * (sd(y) / sd(x))
# 1


# Q5
# Students were given two hard tests and scores were normalized to have empirical mean 0 and variance 1. The correlation between the scores on the two tests was 0.4. What would be the expected score on Quiz 2 for a student who had a normalized score of 1.5 on Quiz 1?
# Quiz1 has score1, Quiz2 has score2
# cor(score1, score2) = 0.4, bacause the data is normalized, so slope=cor=0.4
#  slope = score2 / score1 = 0.4
# score2 = score1 * 0.4 = 1.4 * 0.4 = 0.6

# Q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
mean(x)
sd(x)
(x - mean(x) ) / sd(x)

# Q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lmfit <- lm(y ~ x)
summary(lmfit)

x2 <- x - mean(x)
y2 <- y - mean(y)
lmfit2 <- lm(y2 ~ x2)
summary(lmfit2)

# Q8
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)









