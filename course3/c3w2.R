# c3w2:  read from mySQL
#
library(RMySQL)
ucscDb <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
class(ucscDb)

result <- dbGetQuery(ucscDb, "show databases;")
result
class(result)

dbDisconnect((ucscDb))

#
hg19 <- dbConnect(MySQL(), user="genome", db="hg19", host="genome-mysql.cse.ucsc.edu")
class(hg19)

allTables <- dbListTables(hg19)
class(allTables)
length(allTables)
allTables[1:5]

dbListFields(hg19, "acembly")
dbGetQuery(hg19, "select count(*) from acembly;")
acemblyData <- dbReadTable(hg19,"acembly")
class(acemblyData)
dim(acemblyData)
head(acemblyData)

query_result <- dbGetQuery(hg19, "select * from acembly where exonCount between 2 and 11")
class(query_result)
dim(query_result)

query <- dbSendQuery(hg19, "select * from acembly where exonCount between 2 and 11")
class(query)
mydata <- fetch(query)
class(mydata)
dim(mydata)

mydata2 <- fetch(query, n=10)
dim(mydata2)

dbClearResult(query)

dbDisconnect(hg19)


#################################### HDF5
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library("rhdf5")
created <- h5createFile("example.h5")
created

created <- h5createGroup("example.h5", "foo")
created <- h5createGroup("example.h5", "bar")
created <- h5createGroup("example.h5", "foo/foo2")
created <- h5createGroup("example.h5", "foo/foo2/foo3")
h5ls("example.h5")

A <- matrix(rnorm(100), 20,5)
dim(A)
h5write(A, "example.h5", "foo/A")
h5ls("example.h5")

readA <- h5read("example.h5", "foo/A")
class(readA)
head(readA)

######### web scraping
conn <- url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
# this url is not a "file" with xml or html extension. so use "url" to connect
# this site. Otherwise, download.file() or xmlTreeParse() can be used to retrive
# the file
class(conn)
conn
htmlCode <- readLines(conn)
close(conn)
htmlCode
class(htmlCode)
xpathSApply(htmlCode, "//Title", xmlValue)

# so
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes=TRUE)
class(html)
xpathSApply(html, "//title", xmlValue)

### another way is to use httr
library("httr")
html2 <- GET(url)
class(html2)
html2
html_content <- content(html2, as="text")
class(html_content)
# convert charecter obj to html obj
parsedHtml <- htmlParse(html_content, asText=TRUE)
class(parsedHtml)
xpathSApply(html, "//title", xmlValue)

# deal with authentication
pg2 <- GET("http://httpbin.org/basic-auth/user/passwd", authenticate("user","passwd"))
pg2
class(pg2)
names(pg2)
html_content2 <- content(pg2, as="text")
class(html_content2)
html_content2


# use handle
google <- handle("http://google.com")
# handle support "authenticate("user","passwd")" so it keeps authentication.
# no need to provide auth for each GET afterwards.
google
class(google)
pg1 <- GET(handle=google, path="/")
pg1
class(pg1)
pg2 <- GET(handle=google, path="search")
pg2


## use API
#Twitter app
library(httr)
myapp <- oauth_app("twitter", 
                   key="wUuVpeT7pBdp913k8JSdMSEaG", 
                   secret="KbXLTaCvluZ9aiTNMOganrxn0f3yDXPLmW3FhWFgcSSCWbwyHk")
sig <- sign_oauth1.0(myapp, 
                     token="832641313107374080-vIWcdQe3dNRHOfurSVLPf6RrOaj34br",
                     token_secret = "JWbxMVPF1qxYJCXyFAObBJvX3c9Vbuxzh6NRHCFOPI0P4")


homeTL <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)
class(homeTL)
json1 <- content(homeTL)
class(json1)
json1
json2 <- jsonlite::fromJSON(toJSON(json1))
json2[1, 1:4]


######################################################################################
#### quiz question 1 - github application
# github application
# homepage URL: https://api.github.com/users/jtleek/repos
# callback URL: http://localhost:1410
# Client ID
# ba0c847b3bb0c84d3da9
# Client Secret
# 50a458529d6764c34b1d2ccfa458fe862cf53c4a

library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "ba0c847b3bb0c84d3da9",
                   secret = "50a458529d6764c34b1d2ccfa458fe862cf53c4a")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

# OR:
req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
stop_for_status(req)
content(req)


library(jsonlite)
jsondata<-fromJSON("https://api.github.com/users/jtleek/repos")
class(jsondata)
jsondata$created_at[jsondata$name=="datasharing"]


### quiz question 3 - check SQL statement (not really need to run code.)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
df <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv")
class(df)
dim(df)
install.packages("sqldf")
library(sqldf)


#quize question 4 - calculate number of characters of 10th, 20th... line in html file.
con = url(myurl)
htmlCode = readLines(con)
htmlCode[10]
nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])

# quiz question 5.
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
download.file(fileUrl, destfile="./data/data.for", method="curl")
# use vi to do tidy up.
df <- read.csv("./data/data.for.csv", header=FALSE)
class(df)
head(df)
str(df)
v4 <- df[,4]
class(v4)
v4
sum(v4)
