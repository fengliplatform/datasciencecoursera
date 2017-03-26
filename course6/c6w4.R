# c6w4

# quiz
# Q1
# A pharmaceutical company is interested in testing a potential blood pressure 
# lowering medication. Their first examination considers only subjects that received 
# the medication at baseline then two weeks later. The data are as follows (SBP in mmHg)

#Subject	Baseline	Week 2
#1	140	132
#2	138	135
#3	150	151
#4	148	146
#5	135	130
# Consider testing the hypothesis that there was a mean reduction in blood pressure? 
# Give the P-value for the associated two sided T test.

#(Hint, consider that the observations are paired.)

subject <- c(1,2,3,4,5)
baseline <- c(140,138,150,148,135)
week2 <- c(132,135,151,146,130)
examinations <- data.frame(subject, baseline, week2)
examinations
test <- t.test(x = examinations$baseline, y = examinations$week2, alt = "two.sided", 
               paired = TRUE)
test
pval <- test$p.value
round(pval,3)
# 0.087
# cannot reject H0

# another way to do
#qt(0.975,4)
# 2.77
x <- c(-8, -3, 1, -2, -5)
m <- mean(x)
s <- sd(x)
t <- (m - 0) / (s / sqrt(5)) 
t
# 2.27
# can not reject H0: mu=0 (no change)


# Q2
qt <- qt(0.975, 8)
# 2.3
se <- 30 / sqrt(9)
# 10

# A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard 
# deviation of 30cc. What is the complete set of values of μ0 that a test of H0:μ=μ0 
# would fail to reject the null hypothesis in a two sided 5% Students t-test?

# This is the inversion of a one sided hypothesis test. It yields confidence bounds. 
# (Note inverting a two sidded test yields confidence intervals.) Think about the 
# derivation of the confidence interval.

#Assuming underlying data is iid gaussian

#\(\frac{\bar X - μ}{S / \sqrt{n}}\)

#follows Gosset’s \(t\) distibution with \(n-1\) degrees of freedom

#Interval \(\bar X \pm t_{n-1} S / \sqrt{n}\), where \(t_{n-1}\) is the relevant quantile

n <- 9
μ <- 1100
σ <- 30
quantile = 0.975 # is 95% with 2.5% on both sides of the range
confidenceInterval = μ + c(-1, 1) * qt(quantile, df=n-1) * σ / sqrt(n)
confidenceInterval

# Q3
# Researchers conducted a blind taste test of Coke versus Pepsi. Each of four people 
# was asked which of two blinded drinks given in random order that they preferred. 
# The data was such that 3 of the 4 people chose Coke. Assuming that this sample is 
# representative, report a P-value for a test of the hypothesis that Coke is preferred 
# to Pepsi using a one sided exact test.
n <- 4
x <- 3
?binom.test
test <- binom.test(x=x, n=n, alt="greater")
test
round(test$p.value,2)

# Q4
# Infection rates at a hospital above 1 infection per 100 person days at risk are 
# believed to be too high and are used as a benchmark. A hospital that had previously 
# been above the benchmark recently had 10 infections over the last 1,787 person days 
# at risk. About what is the one sided P-value for the relevant test of whether the hospital 
# is *below* the standard?
?poisson.test
rate <- 1/100
errors <- 10
days <- 1787
test <-  poisson.test(errors, T = days, r = rate, alt="less")
test
round(test$p.value,2)


# Q5
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. 
# Subjects’ body mass indices (BMIs) were measured at a baseline and again after having 
# received the treatment or placebo for four weeks. The average difference from follow-up 
# to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for 
# the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 
# for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI appear 
# to differ between the treated and placebo groups? Assuming normality of the underlying data 
# and a common population variance, give a pvalue for a two sided t test.

n_y <- 9 # subjects treated
n_x <- 9 # subjects placebo
σ_y <- 1.5# kg/m2 std.dev. treated 
σ_x <- 1.8# kg/m2 std.dev. placebo 
μ_y <- -3#  kg/m2 average difference treated
μ_x <- 1#  kg/m2 average difference placebo

# calculate pooled standard deviation
σ_p <- (((n_x - 1) * σ_x^2 + (n_y - 1) * σ_y^2)/(n_x + n_y - 2))
pval <- pt((μ_y - μ_x) / (σ_p * (1 / n_x + 1 / n_y)^.5), df=n_y + n_x -2)
pval


# Q6
# Brain volumes for 9 men yielded a 90% confidence interval of 1,077 cc to 1,123 cc. 
# Would you reject in a two sided 5% hypothesis test of
# H0:μ=1,078?
sample.mean <- (1123 + 1077) / 2
sample.mean
# as we know 1123 is the 95% upper quantile
# 1100 + qt(0.95, 8) * se = 1123
se = 23 / qt(0.95, 8)
se
# go to H0
1078 + qt(0.975, 8) * se
# 1106
# H0:mu 1078 < 1106, so can not reject. H0


# Q7
qt(0.95, 99)
# Researchers would like to conduct a study of 100 healthy adults to detect a four year mean 
# brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss 
# in this population is .04 mm3. About what would be the power of the study for a 5% one 
# sided test versus a null hypothesis of no volume loss?

n <- 100 #subject
μ <- 0.01# m^3 brain volume loss mean
σ <- 0.04# m^3 brain volume loss std. dev.
p <- 0.05 # sign level

pow <- power.t.test(n=n, delta=μ, sd=σ , sig.level=p, type="one.sample", alt="one.sided")$power
round(pow, 2)


#Q8
# Researchers would like to conduct a study of n healthy adults to detect a four year mean 
# brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss 
# in this population is .04 mm3. About what would be the value of n needed for 90% power of 
# type one error rate of 5% one sided test versus a null hypothesis of no volume loss?

μ <- 0.01# m^3 brain volume loss mean
σ <- 0.04# m^3 brain volume loss std. dev.
p <- 0.05 # sign level
pow <- 0.9 #power

n <- power.t.test(power=pow, delta=μ, sd=σ , sig.level=p, type="one.sample", alt="one.sided")$n
ceiling(n/10)*10


# Q1
# H0:μd=0 versus H0:μd≠0 where μd is the mean difference between followup and baseline.
bl <- c(140, 138, 150, 148, 135)
fu <- c(132, 135, 151, 146, 130)
t.test(fu, bl, alternative = "two.sided", paired = TRUE)
#Paired t-test
#data: fu and bl
#t = -2.262, df = 4, p-value = 0.08652
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#    -7.5739 0.7739
#sample estimates:
#    mean of the differences
#-3.4

# equivalent
t.test(fu - bl, alternative = "two.sided")
#One Sample t-test
#data: fu - bl
#t = -2.262, df = 4, p-value = 0.08652
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#    -7.5739 0.7739
#sample estimates:
#    mean of x
#-3.4

# if the test is one sided
-t.test(fu, bl, alternative = "less", paired = TRUE)
# Paired t-test
#data: fu and bl
#t = -2.262, df = 4, p-value = 0.04326
#alternative hypothesis: true difference in means is less than 0
#95 percent confidence interval:-Inf -0.1951
#sample estimates:
#    mean of the differences
#-3.4

# Q2
1100 + c(-1, 1) * qt(0.975, 8) * 30/sqrt(9)

# Q3
#Let p be the proportion of people who prefer Coke. Then, we want to test
#H0:p=.5 versus Ha:p>.5. Let X be the number out of 4 that prefer
#Coke; assume X∼Binomial(p,.5).
#Pvalue=P(X≥3)=choose(4,3)0.5^30.5^1+choose(4,4)0.5^40.5^0

pbinom(2, size = 4, prob = 0.5, lower.tail = FALSE)
# or
choose(4, 3) * 0.5^4 + choose(4, 4) * 0.5^4


#Q4
# H0:λ=0.01 versus Ha:λ<0.01. X=11, t=1,787 and assume X∼H0Poisson(0.01×t)

ppois(10, lambda = 0.01 * 1787)

# Q5

#Q6
# No, you would fail to reject. The 95% interval would be wider than the 90% interval. 
# Since 1,078 is in the narrower 90% interval, it would also be in the wider 95% interval. 
# Thus, in either case it's in the interval and so you would fail to reject.

# Q7
pnorm(1.645 * 0.004, mean = 0.01, sd = 0.004, lower.tail = FALSE)

#Q8

ceiling((4 * (qnorm(0.95) - qnorm(0.1)))^2)

#Q9
# As you require less evidence to reject, i.e. your α rate goes up, 
# you will have larger power.

































