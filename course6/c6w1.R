install.packages("swirl")

library(swirl)

install_from_swirl("Statistical Inference")

swirl()

?pnorm

# Q2
# Suppose that diastolic blood pressures (DBPs) for men aged 35-44 
# are normally distributed with a mean of 80 (mm Hg) and a standard 
# deviation of 10. About what is the probability that a random 35-44 
# year old has a DBP less than 70?
pnorm(70, mean=80, sd=10, lower.tail=TRUE)
#0.16

# Question 3:
# Brain volume for adult women is normally distributed with a mean of 
# about 1,100 cc for women with a standard deviation of 75 cc. 
# What brain volume represents the 95th percentile?
qnorm(0.95, mean=1100, sd=75, lower.tail=T)
#1223
1100 +1.645 * 75
#1223

# Q4
# Refer to the previous question. Brain volume for adult women is 
# about 1,100 cc for women with a standard deviation of 75 cc. 
# Consider the sample mean of 100 random adult women from this 
# population. 
# What is the 95th percentile of the distribution of that sample mean?
75 ^ 2 /100
1100 + 1.645 * sqrt(56.25)
#1112

# sample mean distribution: variance
pupulation.var <- 75 ^ 2
sample.mean.var <- pupulation.var / 100
# sample mean distribution: mena = population mean
sample.mean.mean <- 1100
# sample mean distribution: quantile
qnorm(0.95, mean=sample.mean.mean, sd=sqrt(sample.mean.var))

# 1112

# Q5
# You flip a fair coin 5 times, about what's the probability of getting 4 or 5 heads?
?pbinom
pbinom(3, size=5, p=0.5, lower.tail = FALSE)

# Q6
# The respiratory disturbance index (RDI), a measure of sleep disturbance, for a 
# specific population has a mean of 15 (sleep events per hour) and a standard deviation 
# of 10. They are not normally distributed. Give your best estimate of the probability 
# that a sample mean RDI of 100 people is between 14 and 16 events per hour?
#var of the sameple mean: 
var.sample.mean <- 10 ^2 / 100
sd.sample.mean <- sqrt(var.sample.mean)
p16.sample.mean <- pnorm(16, mean=15, sd=sd.sample.mean, lower.tail=T)
p16.sample.mean
p14.sample.mean <- pnorm(14, mean=15, sd=sd.sample.mean, lower.tail=T) 
p14.sample.mean
# possibility between 14 and 16
p16.sample.mean - p14.sample.mean

# Q7
# Consider a standard uniform density. The mean for this density is .5 and the variance 
# is 1 / 12. You sample 1,000 observations 
# from this distribution and take the sample mean, what value would you expect it to be near?

# Q8
# The number of people showing up at a bus stop is assumed to be
# Poisson with a mean of 5 people per hour. You watch the bus
# stop for 3 hours. About what's the probability of viewing 10 or fewer people?
?ppois()
ppois(10, lambda=5*3, lower.tail=TRUE)
