# c4 w2
# lesson 1: lattice plot

library(lattice)
library(datasets)
xyplot(Ozone~Wind, airquality)

airquality$Month <- as.factor(airquality$Month)
xyplot(Ozone~Wind | Month, airquality, layout=c(5,1))

x <- rnorm(100)
f <- rep(0:1, 50)
y <- x + f - f*x +rnorm(100, sd=0.5)
f <- factor(f, labels=c("Group 1","Group 2"))
xyplot(y~x|f, layout=c(2,1))

xyplot(y~x|f, panel = function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.abline(h=median(y), lty=2)
})

xyplot(y~x|f, panel = function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.lmline(x,y,col=2)
})

## ggplot2
library(ggplot2)
data(mpg)
str(mpg)
head(mpg)
dim(mpg)
qplot(displ, hwy, data=mpg)
qplot(displ, hwy, data=mpg, color=drv)
qplot(displ, hwy, data=mpg, geom=c("point", "smooth"))
qplot(displ, hwy, data=mpg, color=drv, geom=c("point", "smooth"))
qplot(displ, hwy, data=mpg, color=drv) + geom_smooth(method="lm")

qplot(displ, hwy, data=mpg, color=drv)
qplot(displ, hwy, data=mpg, facets=.~drv) # row . column
qplot(displ, hwy, data=mpg, facets=.~drv) + geom_smooth(method="lm") # row . column
qplot(hwy, data=mpg, facets=drv~., binwidth=2)


qplot(hwy, data=mpg,fill=drv)
qplot(hwy, data=mpg, geom="density")
#qplot(hwy, data=mpg, fill=drv, geom="density")
qplot(hwy, data=mpg, color=drv, geom="density")



#ggplot
qplot(displ, hwy, data=mpg, facets=.~drv) + geom_smooth(method="lm")
qplot(displ, hwy, data=mpg, facets=.~drv, geom=c("point", "smooth"))
qplot(displ, hwy, data=mpg, facets=.~drv, geom=c("point", "smooth"), method="lm")


ggplot(mpg, aes(displ,hwy))
g <- ggplot(mpg, aes(displ,hwy))
summary(g)

ggplot(mpg, aes(displ,hwy)) + geom_point()
class(mpg$displ)
#ggplot(mpg, aes(displ,hwy)) + geom_line()
#ggplot(mpg, aes(displ,hwy)) + geom_bar()

ggplot(mpg, aes(displ,hwy)) + geom_point() + geom_smooth()
ggplot(mpg, aes(displ,hwy)) + geom_point() + geom_smooth(method="lm")
ggplot(mpg, aes(displ,hwy)) + geom_point() + geom_smooth(method="lm") + facet_grid(.~drv)

#annotation
#xlab, ylab, ggtitle, theme()
ggplot(mpg, aes(displ,hwy)) + geom_point(color="blue", size=4, alpha=1/2)
ggplot(mpg, aes(displ,hwy)) + geom_point(aes(color=drv), size=4, alpha=1/2)
ggplot(mpg, aes(displ,hwy)) + geom_point() + labs(title="Title") + labs(x="X label", y="y label")

ggplot(mpg, aes(displ,hwy)) + geom_point() + geom_smooth(method="lm", size=4, linetype=4, se=FALSE)

ggplot(mpg, aes(displ,hwy)) + geom_point(color="blue", size=4, alpha=1/2) + theme_bw()


# categraize continous variable
head(mpg)
str(mpg)
cutpoints <- quantile(mpg$displ, seq(0,1,length=4), na.rm=TRUE) # cut to 4 parts
cutpoints
#0% 33.33333% 66.66667%      100% 
#  1.6       2.5       4.0       7.0
summary(mpg$displ)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.600   2.400   3.300   3.472   4.600   7.000

mpg$new <- cut(mpg$displ, cutpoints)
str(mpg)
#$ new         : Factor w/ 3 levels "(1.6,2.5]","(2.5,4]",..:1111222
head(mpg)
# A tibble: 6 × 12
#manufacturer model displ  year   cyl      trans   drv   cty   hwy    fl   class       new
#<chr> <chr> <dbl> <int> <int>      <chr> <chr> <int> <int> <chr>   <chr>    <fctr>
#  1         audi    a4   1.8  1999     4   auto(l5)     f    18    29     p compact (1.6,2.5]
#2         audi    a4   1.8  1999     4 manual(m5)     f    21    29     p compact (1.6,2.5]
#3         audi    a4   2.0  2008     4 manual(m6)     f    20    31     p compact (1.6,2.5]
#4         audi    a4   2.0  2008     4   auto(av)     f    21    30     p compact (1.6,2.5]
#5         audi    a4   2.8  1999     6   auto(l5)     f    16    26     p compact   (2.5,4]
#6         audi    a4   2.8  1999     6 manual(m5)     f    18    26     p compact   (2.5,4]

levels(mpg$new)

g <- ggplot(mpg, aes(displ, hwy))
g+geom_point(alpha=1/3)
 +facet_wrap(new~drv, nrow=3, ncol=3)


### quiz
library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)


#Annotation of plots in any plotting system involves adding points, lines, or text to the plot, 
#in addition to customizing axis labels or adding titles. 
#Different plotting systems have different sets of functions for annotating plots in this way.

library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
print(p)

library(datasets)
data(airquality)
#airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

library(ggplot2)
library(ggplot2movies)
g <- ggplot(movies, aes(votes, rating))
print(g)
library(ggplot2)
install.packages("ggplot2movies")
library(ggplot2movies)
g <- ggplot(movies, aes(votes, rating)) + geom_point()
print(g)
qplot(votes, rating, data = movies) + geom_smooth()























