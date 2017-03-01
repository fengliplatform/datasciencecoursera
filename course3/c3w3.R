# c3w3

set.seed(12345)
x <- data.frame("var1"=sample(1:5), "var2"=sample(6:10), "var3"=sample(11:15))
x
# random the lines
x <- x[sample(1:5),]
x
x$var2[c(1,3)] <- NA
x

x[,1]
x[,"var1"]
x[1:2,"var2"]

x[x$var1 <= 3 & x$var3 > 11,]
x[x$var1 <= 3 | x$var3 > 11,]

x[which(x$var2 > 8),]
x[x$var2 > 8,]

#> x[which(x$var2 > 8),] # doesn't return NAs in var2
#var1 var2 var3
#2    5    9   11
#> x[x$var2 > 8,]
#var1 var2 var3
#NA     NA   NA   NA
#NA.1   NA   NA   NA
#2       5    9   11

# sort
sort(x$var1)
sort(x$var1, decreasing=TRUE)
sort(x$var2) # na is removed
sort(x$var2, na.last=TRUE)

x[order(x$var1,x$var2),]

library(plyr)
arrange(x, var1)
arrange(x, desc(var1))
arrange(x, var1,var2)

x$var5 <- rnorm(5)
x
x <- cbind(rnorm(5), x, rnorm(5))
x
any(is.na(x))
apply(x, 2, function(x) sum(is.na(x)))
# rnorm(5)     var1     var2     var3     var5 rnorm(5) rnorm(5) 
#       0        0        2        0        0        0        0 

colSums(is.na(x))
#rnorm(5)     var1     var2     var3     var5 rnorm(5) rnorm(5) 
#     0        0        2        0        0        0        0

all(x > 0)

#############
setwd("~/course/ds_coursera/c3w1")

if (!file.exists("data")) {
  dir.create("data")
}


fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accesstype=DOWNLOAD"
download.file(url=fileUrl, destfile="./data/restaurants.csv", method="curl")
list.files("./data")
restdata <- read.csv("./data/restaurants.csv")
dim(restdata)
names(restdata)
str(restdata)
head(restdata,3)
tail(restdata)
summary(restdata)
quantile(restdata$councilDistrict)
quantile(restdata$councilDistrict, na.rm = TRUE)
quantile(restdata$councilDistrict, probs=c(0.25,0.5))

table(restdata$zipCode, useNA="ifany")
table(restdata$zipCode, restdata$councilDistrict, useNA="ifany")

sum(is.na(restdata$zipCode))
any(is.na(restdata))
all(restdata$zipCode > 0)

colSums(is.na(restdata))
all(colSums(is.na(restdata)) == 0)

restdata$zipCode == 21212
table(restdata$zipCode == 21212)

#
data("UCBAdmissions")
DF <- as.data.frame(UCBAdmissions)
dim(DF)
DF
summary(DF)
table(DF$Admit)
table(DF$Admit, DF$Gender)
xt <- xtabs(Freq ~ Admit + Gender, data=DF)
xt

object.size(DF)
print(object.size(df), units= "Mb")

# create new variables
s1 <- seq(1,10, by=2);s1
s2 <- seq(1,10, length=3); s2
x <- c(1, 5, 8, 20, 4, 200); s <- seq(along = x)
s

# create bolean variable: TRUE/FALSE condition
restdata$nearme <- restdata$neighborhood %in% c("Homeland")
head(restdata)
table(restdata$nearme)
restdata$wrongzip <- ifelse(restdata$zipCode <0, TRUE, FALSE)
table(restdata$wrongzip)

# create categorical variable: cut, cut2
qt <- quantile(restdata$zipCode)
class(qt)
restdata$zipgroups <- cut(restdata$zipCode, breaks=quantile(restdata$zipCode))
table(restdata$zipgroups)
install.packages("Hmisc")
library("Hmisc")
restdata$zipgroups2 <- cut2(restdata$zipCode, g=4)
table(restdata$zipgroups2)

# change to factor
restdata$zipCode <- factor(restdata$zipCode)
str(restdata$zipCode)

# more factor
yesno <- sample(c("yes","no"), size=10, replace=TRUE)
yesno
class(yesno)
yesno2 <- factor(yesno)
yesno2
yesno3 <- factor(yesno, levels=c("yes","no"))
yesno3
as.numeric(yesno3)

restdata2 <- mutate(restdata, cut2(restdata$zipCode,g=4))
restdata2
restdata

# reshaping
library(reshape2)
head(mtcars)
rownames(mtcars)
colnames(mtcars)

mtcars$carname <- rownames(mtcars)
head(mtcars)
carMelt <- melt(mtcars, id=c("carname","gear","cyl"), measure.vars=c("mpg","hp"))
head(carMelt)
tail(carMelt)
carMelt[carMelt$carname == "Mazda RX4",]

#mean(mtcars$mpg[mtcars$cyl == 4])
cyldata <- dcast(carMelt, cyl ~ variable)
cyldata

cyldata2 <- dcast(carMelt, cyl~variable, mean)
cyldata2

# average
head(InsectSprays)
InsectSprays
tapply(InsectSprays$count, InsectSprays$spray, sum)

spIns <- split(InsectSprays$count, InsectSprays$spray)
spIns
result <- lapply(spIns, sum)
unlist(result)

sapply(spIns, sum)

ddply(InsectSprays, .(spray), summarize, sum=sum(count))
# not working

#################### dplyr
library(dplyr)
chicago <- readRDS("./data/chicago.rds")
dim(chicago)
names(chicago)
str(chicago)

head(select(chicago, city:dptp))
head(select(chicago, -(city:dptp)))

i <- match("city", names(chicago))
i
j <- match("dptp", names(chicago))
j
head(chicago[,-(i:j)])

chicago.f <- filter(chicago, pm25tmean2 > 30)
head(chicago.f)
any(is.na(chicago.f))

head(chicago)
str(chicago)
chicago2 <- chicago[complete.cases(chicago[,"pm25tmean2"]),]
head(chicago2)
chicago2 <- chicago[!is.na(chicago$pm25tmean2),]
head(chicago2)

chicago.f2 <- chicago2[chicago2$pm25tmean2 > 30,]
head(chicago.f2)

#arrange
chic.arr <- arrange(chicago, date)
head(chic.arr)
chic.arr <- arrange(chicago, desc(date))
head(chic.arr)

# rename
chic.rename <- rename(chicago.f, pm25=pm25tmean2, dewpoint=dptp)
head(chic.rename)

#mutate
chic.mutate <- mutate(chic.rename, pm25detrend = pm25-mean(pm25, na.rm=TRUE))
head(chic.mutate)


# group_by
chicago <- mutate(chicago, tmpcat = factor(1*(tmpd>80), labels=c("cold","hot")))
head(chicago)
str(chicago)
table(chicago$tmpcat)
hotcold <- group_by(chicago, tmpcat)
head(hotcold)
class(hotcold)

summarize(hotcold, pm25=mean(pm25tmean2,na.rm=TRUE), o3=max(o3tmean2), no2=median(no2tmean2))

as.POSIXlt("1987-01-01")$year

chicago <- mutate(chicago, year = as.POSIXlt(date)[["year"]] + 1900)
head(chicago)
chicago.year <- group_by(chicago, year)
summarize(chicago.year, pm25=mean(pm25tmean2, na.rm=TRUE), o3=max(o3tmean2), no2=median(no2tmean2))

############ merge data
setwd("~/course/ds_coursera")
if (!file.exists("./data")) {
  dir.create("./data")
}
list.files("./data")

fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
download.file(fileUrl1, destfile="./data/reviews.csv",method="curl")
reviews <- read.csv("./data/reviews.csv")
fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile="./data/solutions.csv",method="curl")
solutions <- read.csv("./data/solutions.csv")

head(reviews)
head(solutions)

names(reviews)
names(solutions)

mergedata <- merge(reviews, solutions, by.x="solution_id", by.y="id", all = TRUE)
names(mergedata)
head(mergedata)
intersect(names(reviews), names(solutions))

## quiz
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile="./data/idaho.csv", metho="curl")
idaho <- read.csv("./data/idaho.csv")
names(idaho)
dim(idaho)
library(dplyr)


# logical vector
idaho <- mutate(idaho, agricultureLogical = ACR == 3 & AGS == 6)
which(idaho$agricultureLogical == TRUE)
#> which(idaho$agricultureLogical == TRUE)
#[1]  125  238  262  470

# read pic
myurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
z <- tempfile()
download.file(myurl,z,mode="wb")
pic <- readJPEG(z, native=TRUE)
class(pic)
names(pic)

quantile(pic, probs=c(0.3,0.8))
#> quantile(pic, probs=c(0.3,0.8))
#30%       80% 
#  -15258512 -10575416 

#file.remove(z) # cleanup

# GDP, EDU
gdpurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
eduurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(gdpurl, destfile="./data/gdp.csv", method="curl")
download.file(eduurl, destfile="./data/edu.csv", method="curl")
gdp <- read.csv("./data/gdp.csv")
edu <- read.csv("./data/edu.csv")
gdp <- rename(gdp, ranking=Gross.domestic.product.2012)

names(gdp)
names(edu)
head(gdp)
head(edu)
dim(gdp)
dim(edu)
gdp$X
edu$CountryCode

gdp <- gdp[-(1:4),]
dim(gdp)
head(gdp, 200)
gdp <- gdp[-(191:330),]
gdp
str(gdp)
rownames(gdp)
colnames(gdp)
gdp[,1]


gdp2 <- gdp

gdp2$ranking2 <- c(1:190)
str(gdp2$ranking2)

gdp2
gdp2 <- arrange(gdp2, desc(ranking2))
gdp2
length(unique(gdp$X))
gdp_code <- unique(gdp$X)
sort(gdp_code)
length(unique(edu$CountryCode))
edu_code <- unique(edu$CountryCode)
sort(edu_code)
j <- 0
for (i in gdp_code) {
  if (i %in% edu_code) {
    j <- j +1
  } else {
    print(i)
  }
}
j
#189


mdata <- merge(gdp2, edu, by.x="X", by.y="CountryCode", all=TRUE)
dim(mdata)
names(mdata)

mdata2 <- arrange(mdata, desc(ranking2))
mdata2

mdata2 <- select(mdata2, 1,4,5,11)
mdata2

#
names(mdata)
table(mdata$Income.Group)
mdata_g <- group_by(mdata, Income.Group)
summarize(mdata_g, m_income=mean(ranking2, na.rm=TRUE))

# cut 
mdata$cut_ranking <- cut(mdata$ranking2, breaks=quantile(mdata$ranking2, na.rm=TRUE))

head(mdata)
str(mdata$Income.Group)
summary(mdata$Income.Group)
country <- mdata[mdata$Income.Group =="Lower middle income", 1]

mdata[,"ranking2"]
gdp2 <- arrange(gdp2, ranking2)
top38 <- gdp2[1:38,1]
j<-0
for (i in country) {
  if(i %in% top38) {
    j <- j + 1
  }
}
j
#5


