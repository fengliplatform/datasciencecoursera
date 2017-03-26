# Course project

# Part 1
hist(rnorm(1000))

hist(runif(1000))

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)


hist(mtcars$mpg)
lines(density(mtcars$mpg))
