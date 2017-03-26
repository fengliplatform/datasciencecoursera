# c7w4

?shuttle
head(shuttle)
str(shuttle)

# Q1
# Consider the space shuttle data ?shuttle in the MASS library. Consider 
# modeling the use of the autolander as the outcome (variable name use). 
# Fit a logistic regression model with autolander (variable auto) use 
# (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). 
# Give the estimated odds ratio for autolander use comparing head winds, 
# labeled as "head" in the variable headwind (numerator) to tail winds (denominator).
shuttle$auto <- as.numeric(shuttle$use == "auto")
str(shuttle)
table(shuttle$auto)

#glmfit <- glm(auto ~ wind, data=shuttle, family="binomial")
#summary(glmfit)

glmfit <- glm(auto ~ wind-1, data=shuttle, family="binomial")
summary(glmfit)

coef(glmfit)
coef(summary(glmfit))
coef <- coef(summary(glmfit))
windhead.coef <- coef[1,1]
windhead.coef
windtail.coef <- coef[2,1]
windtail.coef

exp(windhead.coef)
exp(windtail.coef)

exp(windhead.coef) / exp(windtail.coef)

# Q2
# Consider the previous problem. Give the estimated odds ratio for autolander use 
# comparing head winds (numerator) to tail winds (denominator) adjusting for wind 
# strength from the variable magn.

glmfit <- glm(auto ~ wind + magn - 1, data=shuttle, family="binomial")
summary(glmfit)

coef <- coef(summary(glmfit))
windhead.coef <- coef[1,1]
windhead.coef
windtail.coef <- coef[2,1]
windtail.coef

exp(windhead.coef) / exp(windtail.coef)

# Q3
# If you fit a logistic regression model to a binary variable, for example use of 
# the autolander, then fit a logistic regression model for one minus the outcome 
# (not using the autolander) what happens to the coefficients?

glmfit <- glm(auto ~ wind - 1, data=shuttle, family="binomial")
summary(glmfit)
glmfit <- glm(I(1-auto) ~ wind - 1, data=shuttle, family="binomial")
summary(glmfit)

glmfit <- glm(auto ~ wind, data=shuttle, family="binomial")
summary(glmfit)

# Q4
# Consider the insect spray data InsectSprays. Fit a Poisson model using spray as 
# a factor level. Report the estimated relative rate comapring spray A (numerator) 
# to spray B (denominator).
data("InsectSprays")
str(InsectSprays)
head(InsectSprays)

# A is reference as intercept, all values are after subtracting A
poisson.fit <- glm(count ~ spray, data=InsectSprays, family="poisson")
poisson.fit

# by using spray-1, coef will be original value against 0 Intersept.
poisson.fit <- glm(count ~ spray-1, data=InsectSprays, family="poisson")
poisson.fit

coef.A <- coef(summary(poisson.fit))[1,1]
coef.A
coef.B <- coef(summary(poisson.fit))[2,1]
coef.B

exp(coef.A) / exp(coef.B)

# Q5
# Consider a Poisson glm with an offset, t. So, for example, a model of the form 
# glm(count ~ x + offset(t), family = poisson) where x is a factor variable 
# comparing a treatment (1) to a control (0) and t is the natural log of a 
# monitoring time. What is impact of the coefficient for x if we fit the model 
# glm(count ~ x + offset(t2), family = poisson) where 2 <- log(10) + t? In other 
# words, what happens to the coefficients if we change the units of the offset 
# variable. (Note, adding log(10) on the log scale is multiplying by 10 on the 
# original scale.)


# The coefficient estimate is multiplied by 10.

# Q6
# Using a knot point at 0, fit a linear model that looks like a hockey stick with 
# two lines meeting at x=0. Include an intercept term, x and the knot point term. 
# What is the estimated slope of the line after 0?
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

lm(y~x)
plot(x, y)

x2 <- 0:5
y2 <- c(0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
lm(y2 ~ x2)














