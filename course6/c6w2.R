# c6w2

# random number of normal distribution
n <- 10000
means <- cumsum(rnorm(n))/(1:n)
means
library(ggplot2)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g

?cumsum
cumsum(1:10)

# flip a coin
n <- 10000
means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g


# difference compare is T distribution.
# t interval: e +- qt * se
# e: mean of the difference 
# qt: quantile of t under specified interval: 
# for example, qt(0.975, 10-1): given 95% interval, 10 subjects, calculate quantile points
# se: standard error - sd / sqrt(n)

# difference compare has two types:
# 1. paired - the same subjects doing multiple tests to compare the test results
# 2. unpaired - two groups of subjects doing different tests respectively and compare results

# 1. paired example
# 10 subjects do test1 and test2, compare the results(difference)
data(sleep)
head(sleep)
str(sleep)
summary(sleep)
sleep
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
difference
mn <- mean(difference); s <- sd(difference); n <- 10
mn
s

# E +- TQ * se. This is t interval
n <- 10
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)

# use t.test. There are three same ways to do to get 95% interval
t.test(difference) # do a t test on this sample to estimate population
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)

?qt
qt(0.975, 9-1)
# 2.30

#
?t.test

# 2a. unpaired example with assumption of constant var between groups
# two group of people one received treatment. one received placebo(no treatment)
# mean.y - mean.x +- tq (nx + ny -2) * sp * sqrt(1/nx + 1/ny)
# sp^2 <- [(nx-1) * sx ^2 + (ny -1) * sy^2] / (nx + ny - 2)




# 2b. unpaired example with unqual variances between groups


x <- c(15.6,16.2,22.5,20.5,16.4,19.4,16.6,17.9,12.7,13.9)
t.test(x,alternative="less", mu=20, conf.level=0.99)
?t.test

##  t test using t.test for father/son height
library(UsingR)
data(father.son)
dim(father.son)
str(father.son)
qt(0.90, 1077)
qt(0.95, 1077)
#1.65
qt(0.972, 1077)
#1.91

t.test(father.son$sheight - father.son$fheight, mu=0)


# Q1
#In a population of interest, a sample of 9 men yielded a sample average brain volume of 
#1,100cc and a standard deviation of 30cc. What 
#is a 95% Student's T confidence interval for the mean brain volume in this new population?

1100 + c(-1,1) * qt(0.975,8) * 30/sqrt(9)
qt(0.975, 8)
t.test(0.975, mu=1100, sd=30)


# Q2
#A diet pill is given to 9 subjects over six weeks. The average difference in weight 
# (follow up - baseline) is -2 pounds. What would the standard deviation of the difference 
# in weight have to be for the upper endpoint of the 95% T confidence interval to touch 0?

# t interval
# E +- TQ * se
# E +- TQ * sd / sqrt(n)
# upper point of 95% T confidence interval is quantile of 97.5%
#
n <- 9
mn < -2

tq <- qt(0.975, n-1)
tq

#lower.point <- mn - tq * sd / sqrt(n)
#upper.point <- mn + tq * sd / sqrt(n) --- touch 0
# -2 + tq * sd /sqrt(n) = 0
sd <- 2 * sqrt(n) /tq
sd    
# 2.6

# Q3
# In an effort to improve running performance, 5 runners were either given a protein 
# supplement or placebo. Then, after a suitable washout period, they were given the 
# opposite treatment. Their mile times were recorded under both the treatment and placebo, 
# yielding 10 measurements with 2 per subject. The researchers intend to use a T test and 
# interval to investigate the treatment. 
# Should they use a paired or independent group T test and interval?

# paired

# Q4
# In a study of emergency room waiting times, investigators consider a new and the standard 
# triage systems. To test the systems, administrators selected 20 nights and randomly 
# assigned the new triage system to be used on 10 nights and the standard system on the 
# remaining 10 nights. They calculated the nightly median waiting time (MWT) to see a 
# physician. The average MWT for the new system was 3 hours with a variance of 0.60 while 
# the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% 
# confidence interval estimate for the differences of the mean MWT associated with the new 
# system. Assume a constant variance. What is the interval? Subtract in this order 
# (New System - Old System).
  
# unequal variance question
# -2+c(-1,1)*qt(.975,18)*sqrt(2/10)*sqrt(((9*.68+9*.6)/18))

n <- 10
mean.new <- 3
var.new <- 0.6
mean.old <- 5
var.old <- 0.68

e <- mean.new - mean.old
e
qt <- qt(0.975, n-1)
qt    
    
se <- sqrt(0.6/10 + 0.68/10)    
se    

e + qt * se
e - qt * se

# Q5
# Suppose that you create a 95% T confidence interval. You then create a 90% interval using 
# the same data. What can be said about the 90% interval with respect to the 95% interval?
# narrower
# #Probability of being stricly greater than 3 heads (4 or 5)
# pbinom(3, size = 5, prob = 0.5, lower.tail = FALSE)


# Q6
# To further test the hospital triage system, administrators selected 200 nights and randomly 
# assigned a new triage system to be used on 100 nights and a standard system on the 
# remaining 100 nights. They calculated the nightly median waiting time (MWT) to see a 
# physician. The average MWT for the new system was 4 hours with a standard deviation of 
# 0.5 hours while the average MWT for the old system was 6 hours with a standard deviation 
# of 2 hours. Consider the hypothesis of a decrease in the mean MWT associated with the new 
# treatment.

# What does the 95% independent group confidence interval with unequal variances suggest 
#vis a vis this hypothesis? 
#(Because there’s so many observations per group, just use the Z quantile instead of the T.)
#2+c(-1,1)*qt(.975,98)*sqrt(2/100)*sqrt(((99*(.5)^2+99*2)/98))


#Q7
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. 
# Subjects’ body mass indices (BMIs) were measured at a baseline and again after having 
# received the treatment or placebo for four weeks. The average difference from follow-up 
# to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for 
# the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 
# for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI over 
# the four week period appear to differ between the treated and placebo groups? Assuming 
# normality of the underlying data and a common population variance, calculate the relevant 
# 90% t confidence interval. Subtract in the order of (Treated - Placebo) with the smaller 
# (more negative) number first.
# -4+c(-1,1)*qt(.95,16)*sqrt(2/9)*sqrt((8*(1.8)^2+8*(1.5)^2)/16)
