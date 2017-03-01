#c3w4

setwd("~/course/ds_coursera")
if(!file.exists("./data")) {
  dir.create("./data")
}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accesstype=DOWNLOAD"
download.file(fileUrl, destfile="./data/cameras.csv", method="curl")
list.files("./data")
cameradata <- read.csv("./data/cameras.csv")

names(cameradata)
tolower(names(cameradata))

splitnames <- strsplit(names(cameradata), "\\.")
splitnames

mylist <- list(letters=c("A","b","c"), numbers=1:3, matrix(1:25, ncol=5))
mylist
mylist[1]
mylist[[1]]

splitnames[[6]][1]
firstprameter <- function(x) {x[1]}
sapply(splitnames, firstprameter)

#
fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
download.file(fileUrl1, destfile="./data/reviews.csv",method="curl")
reviews <- read.csv("./data/reviews.csv")
fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile="./data/solutions.csv",method="curl")
solutions <- read.csv("./data/solutions.csv")

reviews
solutions

names(solutions)
sub("_","", names(solutions))

#finding values
grep("Alameda", cameradata$intersection)
# 4,5,36
cameradata[4, "intersection"]
grep("Alameda", cameradata$intersection, value=TRUE)
grep("Jeff", cameradata$intersection)

grepl("Alameda", cameradata$intersection)
table(grepl("Alameda", cameradata$intersection))

alameda_data <- cameradata[grepl("Alameda", cameradata$intersection),]
alameda_data

library(stringr)
nchar("this is a string")
substr("JeffStreet", 1,7)
paste("string1", "string2", sep="-")

# date
d1 <- date()
d1
class(d1)

d2 <- Sys.Date()
d2
class(d2)
format(d2, "%a %b %d")
# [1] "Mon Feb 20"

# %a: weekday- Fri, %A: Friday
# %b: month-Feb, %B: Feburary, %m: (0,1,2,..., 12)
# %d: day(0-31), 
#  %y, %Y: year
weekdays(d2)
months(d2)
julian(d2)


x <- c("1jan1960", "2jan1960")
y <- as.Date(x, "%d%b%Y")
y
y[1] - y[2]

as.numeric(y[1]-y[2])


#lubridate
library(lubridate)
a <- ymd("20170220")
class(a)
#Date

wday(a)
wday(a, label=TRUE)


mdy("3/20/2013")

ymd_hms("1977-02-03 3:35:50")
ymd_hms("1977-02-03 3:35:50", tz="America/Toronto")
Sys.timezone(location=TRUE)

# data resources
data.un.org
data.gov
data.gov/opendatasites
gapminder.org
asdfree.com
infochimps.com
kaggle

# dataset from data scientists
Hilary Mason
Peter Skomoroch
Jeff hammerbacher
Gregory P-S

#
Stanford Large network data()
UCI ML
KDD Nugets datasets
CMU statlib
Gene expression omnibus
ArXiv data
Public dataset on AWS

# API
TwitterR
rOpenSci
RFacebook
RGoogleMap

# quiz
list.files("./data")
idaho <- read.csv("./data/idaho.csv")
mynames <- names(idaho)
strsplit(mynames, "wgtp")[123]
#"" 15

####
gdpurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(gdpurl, destfile="./data/gdp.csv", method="curl")
gdp <- read.csv("./data/gdp.csv")
gdp <- rename(gdp, ranking=Gross.domestic.product.2012)

gdp <- gdp[-(1:4),]
dim(gdp)
head(gdp, 200)
gdp <- gdp[-(191:330),]
gdp

names(gdp)
head(gdp)
gdp$gdp <- gsub("," ,"", gdp$X.3)
head(gdp)
gdp$gdp <- as.numeric(gdp$gdp)

mean(gdp$gdp, na.rm=TRUE)

grep("^United", gdp$X.2)

# quiz 4
eduurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(eduurl, destfile="./data/edu.csv", method="curl")
edu <- read.csv("./data/edu.csv")
head(edu)

notes <- edu$Special.Notes
grep("^Fiscal year end: June",notes, value=TRUE)

# q5 - get stock data from AWS about NASDAQ/NYSE
install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
class(sampleTimes)
sampleTimes
dates <- format(sampleTimes, "%Y %A")
dates <- as.character(dates)
length(grep("2012", dates))
dates[1]
length(grep("2012 Monday", dates))


############################## course project
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/project.zip", method="curl")

# read readme
# 30 volunteers, 6 activities
# two devices: accelerometer (get data linear acceleration), gyroscope (get data of anular velicity)
# each record:
# 

setwd("~/course/ds_coursera/data/c3-project/UCI_HAR_Dataset")
list.files("./")

trainfile <- "./train/X_train.txt"
lines <- readLines(trainfile)
#class(lines)
lines <- sub("^ +","",lines)
lines <- gsub(" +"," ",lines)
lines <- strsplit(lines, split=" ")
lines <- unlist(lines)
lines.m <-matrix(lines, ncol=561, byrow=TRUE)
#class(lines.m)
#dim(lines.m)
train.df <- as.data.frame(lines.m)
#dim(train.df)
#7352 561
#names(train.df)
#length(train.df[1,])
#length(train.df[7352,])
any(is.na(train.df))

testfile <- "./test/X_test.txt"
lines <- readLines(testfile)
#class(lines)
lines <- sub("^ +","",lines)
lines <- gsub(" +"," ",lines)
lines <- strsplit(lines, split=" ")
lines <- unlist(lines)
lines.m <-matrix(lines, ncol=561, byrow=TRUE)
#class(lines.m)
#dim(lines.m)
test.df <- as.data.frame(lines.m)
#dim(test.df)
#2947 561
#names(test.df)
#length(test.df[1,])
#length(test.df[2947,])
any(is.na(test.df))

full.df <- rbind(train.df, test.df)
dim(full.df)
str(full.df)
full.df <- as.numeric(full.df)

# read feature names
featurefile <- "./features.txt"
feature.df <- read.table(featurefile, header=FALSE)
class(feature.df)
dim(feature.df)
feature.names <- feature.df[,2]
#feature.names
class(feature.names)
feature.names <- as.character(feature.names)
#feature.names

#names(train.df) <- feature.names
#names(test.df) <- feature.names
#names(full.df) <- feature.names

######## q2
mean.sd.position <- grep("mean()|std()", feature.names)
mean.sd.position
mean.sd.names <- grep("mean()|std()", feature.names,value=TRUE)
mean.sd.names
class(mean.sd.position)
full.df.mean.sd <- select(full.df, mean.sd.position)
dim(full.df.mean.sd)
full.df.mean.sd.bak <- full.df.mean.sd
names(full.df.mean.sd) <- mean.sd.names

######## q3
trainactivityfile <- "./train/y_train.txt"
trainactivity.label.df <- read.table(trainactivityfile, header=FALSE)
trainactivity.label.df
names(trainactivity.label.df) <- "activity.label"

testactivityfile <- "./test/y_test.txt"
testactivity.label.df <- read.table(testactivityfile, header=FALSE)
testactivity.label.df
names(testactivity.label.df) <- "activity.label"

activity.label.df <- rbind(trainactivity.label.df, testactivity.label.df)
dim(activity.label.df)
activity.label.df.bak <- activity.label.df

#full.df <- cbind(full.df.mean.sd, activity.df)
#dim(full.df)
#names(full.df)

activitynamefile <- "./activity_labels.txt"
activity.name.df <- read.table(activitynamefile, header=FALSE)
activity.name.df

activity.df <- merge(activity.label.df, activity.name.df, by.x="activity.label", by.y="V1")
activity.df
table(activity.df$V2)
table(activity.label.df$activity.label)

full.df <- cbind(full.df, activity.name=activity.df$V2)
names(full.df)
table(full.df$activity.name)

# handle subject
trainsubjectfile <- "./train/subject_train.txt"
trainsubject.df <- read.table(trainsubjectfile, header=FALSE)
dim(trainsubject.df)

testsubjectfile <- "./test/subject_test.txt"
testsubject.df <- read.table(testsubjectfile, header=FALSE)
dim(testsubject.df)

subject.df <- rbind(trainsubject.df, testsubject.df)
dim(subject.df)

full.df <- cbind(full.df, subject=subject.df$V1)
names(full.df)
#full.df <- full.df[,-80]
str(full.df)

######## q5
full.df$activity.name <- as.character(full.df$activity.name)
indx <- sapply(full.df, is.factor)
indx

full.df[indx] <- lapply(full.df[indx], function(x) as.numeric(as.character(x)))
str(full.df)
full.df$activity.name <- as.factor(full.df$activity.name)


mtcars %>% 
  group_by_(.dots=c("mpg","hp","wt")) %>% 
  summarize(x=mean(gear))

mean.sd.names <- gsub("-", "", mean.sd.names)
mean.sd.names
class(mean.sd.names)
mean.sd.names <- c(mean.sd.names, "activity_name")
mean.sd.names <- c(mean.sd.names, "subject")
mean.sd.names
names(full.df) <- mean.sd.names
names(full.df)


new.df <- full.df %>%
  group_by_(.dots=c("activity_name", "subject")) %>%
  summarize_each(funs(mean(., na.rm=TRUE)))

new.df

write.table(new.df, file="meandata2.txt", sep=" ", row.names=FALSE)





















