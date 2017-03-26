#c7w2

library(datasets)
data("diamond")

# fit lm price ~ carat
fit <- lm(price ~ carat, data=diamond)
coef(fit)
#(Intercept)       carat 
#-259.6259   3721.0249
g <- ggplot(diamond, aes(x=carat, y=price)) + geom_point() + geom_smooth(method="lm")
g

# center carat
#diamond$carat2 <- diamond$carat - mean(diamond$carat)

fit2 <- lm(price ~ carat2, data=diamond)
coef(fit2)
#(Intercept)      carat2 
#500.0833   3721.0249
g2 <- ggplot(diamond, aes(x=carat2, y=price)) + geom_point() + geom_smooth(method="lm")
g2

# carat * 10
#diamond$carat3 <- diamond$carat * 10

#fit3 <- lm(price ~ I(carat * 10), data=diamond)
fit3 <- lm(price ~ carat3, data=diamond)
coef(fit3)
#  (Intercept) I(carat * 10) 
# -259.6259      372.1025
g3 <- ggplot(diamond, aes(x=carat3, y=price)) + geom_point() + geom_smooth(method="lm")
g3


# prediction
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx
# [1]  335.7381  745.0508 1005.5225

predict(fit, data.frame(carat=newx))
# 1         2         3 
# 335.7381  745.0508 1005.5225


### residuals
library(datasets)
data("diamond")

x <- diamond$carat
y <- diamond$price
n <- length(y)

fit <- lm(y ~ x)
coef(fit)
# calcualte residuals
e <- resid(fit)
e
# another way to calculate rsiduals
yhat <- predict(fit)
e2 <- y - yhat
e2
# they should be equal
max(e - e2)
summary(e-e2)
sum(e * x)


# plot residuals
plot(x, y)
abline(lm(y~x), lwd=2)

for (i in 1:n) {
    lines(c(x[i],x[i]), c(y[i], yhat[i]), col = "red")
}

# residuals plot
x <- runif(100, 0,6)
y <- x + rnorm(100, mean = 0, sd = 0.001 * x)
g <- ggplot(data.frame(x=x, y=y), aes(x=x, y=y))
g <- g + geom_smooth(method = "lm", col = "black")
g <- g + geom_point(size=6, col="red", alpha=0.2)
g

g2 <- ggplot(data.frame(x=x, y=resid(lm(y~x))), aes(x=x, y=y))
g2 <- g2 + geom_hline(yintercept=0)
g2 <- g2 + geom_point(size=6, col="red", alpha=0.2)
g2


##

library(datasets)
data("diamond")

g <- ggplot(diamond, aes(x=price, y=carat))
g <- g + geom_point(size=6, col="red", alpha=0.2)
g <- g + geom_smooth(method="lm", col="black")
g
fit <- lm(price ~ carat, data=diamond)
resi <- resid(fit)
diamond$e <- resi
head(diamond)
g2 <- ggplot(diamond, aes(x=carat, y=e))
g2 <- g2 + geom_hline(yintercept=0)
g2 <- g2 + geom_point(size=6, color="red", alpha=0.2)
g2

##################
# Q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y~x)
fit
summary(fit)
summary(fit)$sigma
summary(fit)$coefficients
sumcoe <- summary(fit)$coefficients

beta0 <- sumcoe[1,1]
se.beta0 <- sumcoe[1,2]
beta1 <- sumcoe[2,1]
beta1
se.beta1 <- sumcoe[2,2]
se.beta1
n <- 9
df <- fit$df

# beta0 quantile
sumcoe[1,1] + c(-1, 1) * qt(0.975, df=fit$df) * sumcoe[1,2]

# beta1 quantile
sumcoe[2,1] + c(-1, 1) * qt(0.975, df=fit$df) * sumcoe[2,2]

#Q1
beta1.pvalue <- sumcoe[2,4]

# Q2
summary(fit)$sigma

## Q3
library(datasets)
data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
fit.mtcars <- lm(y~x)
fit.mtcars
summary(fit.mtcars)

# construct a 95% confident interval
predict(fit.mtcars, data.frame(x=mean(x)), interval="confidence")

## Q4
?mtcars
# wt coefficients is -5.3445 which means the mpg change per 1000lb wt change.

## Q5
# construct a 95% prediction interval
predict(fit.mtcars, newdata = data.frame(x = 3), interval = ("prediction"))


##Q6
# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs). A “short” ton is defined as 2,000 lbs. Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight. Give the lower endpoint.
fit.mtcars2 <- lm(y ~ I(x/2))
summary(fit.mtcars2)
sumCoef2 <- coef(summary(fit.mtcars2))
(sumCoef2[2,1] + c(-1, 1) * qt(.975, df = fit.mtcars2$df) * sumCoef2[2, 2])

## Q7




























