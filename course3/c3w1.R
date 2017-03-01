# raw data to tidy data
# data source: https://data.baltimorecity.gov/Transportation/Baltimore-Fixed-Speed-Cameras/dz54-2aru

setwd("~/course/ds_coursera/c3w1")

if (!file.exists("data")) {
  dir.create("data")
}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accesstype=DOWNLOAD"
download.file(url=fileUrl, destfile="./data/cameras.csv", method="curl")
list.files("./data")

dateDownloaded <- date()
dateDownloaded


#df <- read.table("./data/cameras.csv", sep=",", header=TRUE)
df <- read.csv("./data/cameras.csv")
class(df)
dim(df)
head(df)
str(df)
names(df)

#### read xlsx
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accesstype=DOWNLOAD"
download.file(url=fileUrl, destfile="./data/cameras.xlsx", method="curl")

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(url=fileUrl, destfile="./data/ng.xlsx", method="curl")
list.files("./data")

dateDownloaded <- date()
dateDownloaded

# 1. install Java JDK:
# sudo apt-get update
# sudo apt-get install openjdk-7-jdk
# sudo R CMD javareconf
# 2. do following link 
# $ sudo ln -s /usr/lib/jvm/java-7-openjdk-amd64 /usr/lib/jvm/default-java
# 3. restart rstudio
# 1) sudo rstudio-server stop
# 2) export LD_LIBRARY_PATH=/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/jre/lib/amd64/default
# 3) sudo rstudio-server start

install.packages("rJava")
library(rJava)
install.packages("xlsx")
library(xlsx)

#df <- read.table("./data/cameras.csv", sep=",", header=TRUE)
df <- read.xlsx("./data/ng.xlsx",sheetIndex=1, header=TRUE)
class(df)
dim(df)
head(df)
str(df)
names(df)
df2 <- df[18:23, 7:15]
df2

dat <- read.xlsx("./data/ng.xlsx", sheetIndex=1, rowIndex = 18:23, colIndex=7:15, header=TRUE)
#dat <- read.xlsx("./data/ng.xlsx", sheetIndex=1, rowIndex = 1:94, colIndex=1:26, header=TRUE)
dat
head(dat)
str(dat)
names(dat)
dim(dat)

sum(dat$Zip*dat$Ext,na.rm=T)



############### read XML file
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xml?accessType=DOWNLOAD"

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
library(XML)
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
# or download and read file from local
download.file(fileUrl, destfile="./data/xmldata.xml", method="curl")
list.files("./data")
doc <- xmlTreeParse("./data/xmldata.xml", useInternal = TRUE)
class(doc)
doc


############ read JSON
fileUrl <- "https://api.github.com/users/jtleek/repos"
library(jsonlite)
jsonData <- fromJSON(fileUrl)
class(jsonData)
names(jsonData)
names(jsonData$owner)

jsonData$owner$login

myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)
class(myjson)

myiris <- fromJSON(myjson)
class(myiris)
head(myiris)

## quiz
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/data1.csv", method="curl")
list.files("./data")

df <- read.csv("./data/data1.csv")
class(df)
dim(df)
names(df)

head(df[,"VAL"])
df.val <- df[, c("SERIALNO","VAL", "FES")]
df.val
dim(df.val)

str(df.val)
df.val <- df.val[df.val$VAL==24,]
df.val
df.val.rm.na <- df.val[complete.cases(df.val[,"VAL"]),]
dim(df.val.rm.na)

length(df.val[df.val==24])
length(df.val[df.val==1])
length(df.val[df.val==2])
length(df.val[df.val==3])
length(df.val[df.val==4])
length(df.val[df.val==5])
length(df.val[df.val==6])

length(df.val[is.na(df.val)])
length(df.val)

########
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl, destfile="./data/usc.csv", method="curl")
library(data.table)
DT <- fread("./data/usc.csv")
#Sys.time()
system.time(mean(DT[DT$SEX==1,]$pwgtp15));system.time(mean(DT[DT$SEX==2,]$pwgtp15))
#0.009+0.01
#Sys.time()

system.time(tapply(DT$pwgtp15,DT$SEX,mean))
#0.001
Sys.time()
system.time(DT[,mean(pwgtp15),by=SEX])
# 0.001
Sys.time()
system.time(rowMeans(DT)[DT$SEX==1]); system.time(rowMeans(DT)[DT$SEX==2])
Sys.time()
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
#0.001
Sys.time()
system.time(mean(DT$pwgtp15,by=DT$SEX))
# 0.001
Sys.time()

df <- read.csv(fileUrl)
dim(df)

