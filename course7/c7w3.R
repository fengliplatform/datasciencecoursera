#c7w3

require(datasets)
data(swiss)
?swiss

dim(swiss)
str(swiss)
head(swiss)

require(stats); require(graphics)
pairs(swiss, panel = panel.smooth, main = "swiss data",
      col = 3 + (swiss$Catholic > 50))
summary(lm(Fertility ~ . , data = swiss))
par(mfrow=c(1,1))
p <- par()
par(mar=c(5.2,4.1,4.1,2.1))
par() <- p


# Q1
require(datasets)
data(mtcars)

str(mtcars)
table(mtcars$cyl)

mtcars$cyl <- as.factor(mtcars$cyl)
fit <- lm(mpg ~ cyl + wt, data=mtcars)
summary(fit)$coefficient
# summary(fit)$coefficient[3,1]

#Q2
fit2 <- lm(mpg ~ cyl, data=mtcars)
summary(fit2)$coefficient

# Q3
data(mtcars)

fit <- lm(mpg ~ factor(cyl) + wt, data=mtcars)
summary(fit)$coefficient

fit3 <- lm(mpg~ factor(cyl)*wt, data=mtcars)
summary(fit3)$coefficients

anova(fit,fit3, test="Chisq")

#fit_inter <- lm(mpg ~ factor(cyl) * wt, data = mtcars)
#anova(fit, fit_inter, test = "Chisq")

# Q4
data(mtcars)
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)


# Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit5 <- lm(y ~ x)
summary(fit5)
hatvalues(fit5)

# Q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit6 <- lm(y ~ x)
dfbetas(fit6)

# Q7
# add one more var to adjust can revise the trend of previous var to 
# targeting var






















