#c4w4

# get data from EPA, but the link/file have changed
# assume we have file RD_501_88101_1999-0.txt

# read 1999 data
pm0 <- read.table("RD_501_88101_1999-0.txt", sep="|", na.strings = "", comment.char="#", header=FALSE)

cnames <- readLines("RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed=TRUE)
names(pm0) <- cnames[[1]]

names(pm0) <- make.names(cnames[[1]]) # replace the spaces to be .

x0 <- pm0$Sample.Value
class(x0) # numeric vector
str(x0) # get how many
summary(x0) # get quantile

mean(is.na(x0)) # 0.11. 
# This is to get a 0,0,1,0,1,1... vector first based on the value is NA(1) or not.(0)
# calculate the mean of this vector will tell you the percentage of number of NA values.
# so 11% values are NA, are missing

# read 2012 data
pm1 <- read.table("RD_501_88101_2012-0.txt", sep="|", comment.char="#", na.strings-"", header-FALSE)
dim(pm1)
cnames <- readLines("RD_501_88101_2012-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed=TRUE)
names(pm1) <- cnames[[1]]
names(pm1) <- make.names(cnames[[1]])

x1 <- pm1$Sample.Values
class(x1)
str(x1)
summary(x1)
mean(is.na(x1))


#
summary(x0)
summary(x1)
# median x0=12, x1=7. seems overall lower values in 2012 than 1999
# max in 2012 is 905 much high outlier

# 
boxplot(x0, x1) # get outlier, not clear for main data conparison

boxplot(log10(x0), log10(x1)) # so plot log data.

#### why negative value?
negative <- x1 < 0
str(negative) # logical vector: F,T,T,F,F,F,F,F,F, T: <0, F: >= 0
sum(negative, na.rm =TRUE) # get number of T (1), which are negative.
mean(negative, na.rm =TRUE) # calculate mean which is percentage of 1s. 2% negative values

# are negative values related dates? or locations?
dates <- x1$Date
str(dates) # integer values: 20120101,...
# change to Date type
dates <- as.Date(as.character(dates), "%Y%m$d") # as.Date changes string to YMD. 
# strptime can generate POSIXct/POSIXlt timestamp based on string.
str(dates) # Date tyep: 2012-01-01,...

hist(dates, "month") # breaks = month. This is to use hist to Date type of data.
hist(dates[negative], "month")

# check monitor in NY state
site0 <- unique(subset(pm0, State.Code==36, c("County.Code", "Site.ID"))) 
# subset State.Code=36 records with Conty.Code and Site.ID columns -- 1999 data
# subset State.Code=36 records with Conty.Code and Site.ID columns -- 2012 data
site1 <- unique(subset(pm1, State.Code==36, c("County.Code", "Site.ID"))) 

site0 <- paste(site0[,1], site0[,2], sep=".")
site1 <- paste(site1[,1], site1[,2], sep=".")

both <- intersect(site0, site1)
both

pm0$county.site <- with(pm0, paste(site0[,1], site0[,2], sep=".")) # create one new column
pm1$county.site <- with(pm1, paste(site1[,1], site1[,2], sep=".")) # create one new column

cnt0 <- subset(pm0, State.COde == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.COde == 36 & county.site %in% both)

#split(cnt0, cnt0$county.site)
sapply(split(cnt0, cnt0$county.site), nrow()) # count how many records of each site 1999
#split(cnt1, cnt1$county.site)
sapply(split(cnt1, cnt1$county.site), nrow()) # count how many records of each site 2012
# choose sites 63.2008 with reasonal records, county: 63, site: 2008

# subset 1999 data and 2012 data to only get this county and site
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)


plot(pm1sub$Date, pm1sub$Sample.Value) # Data is integer type.

pm1sub$Date <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
plot(pm1sub$Date, pm1sub$Sample.Value)
pm0sub$Date <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
plot(pm0sub$Date, pm0sub$Sample.Value)

# plot the two chart in one panel
par(mfrow=c(1,2), mar=c(4,4,2,1))
plot(pm0sub$Date, pm0sub$Sample.Value)
abline(h=median(pm0$Sample.Value, na.rm=TRUE))
plot(pm1sub$Date, pm1sub$Sample.Value)
abline(h=median(pm1$Sample.Value, na.rm=TRUE))
       
# But the y axis is not the same scale: pm0 is 0-40, pm1 is 0-20. We need to put them 
# in the same Y scale which is to use ylim

# get range of pm0
rng <- range(pm0$Sameple.Value, pm1$Sample.Value, na.rm=TRUE)
plot(pm0sub$Date, pm0sub$Sample.Value, ylim=rng) # use the same ylim
abline(h=median(pm0$Sample.Value, na.rm=TRUE))
plot(pm1sub$Date, pm1sub$Sample.Value, ylim=rng)
abline(h=median(pm1$Sample.Value, na.rm=TRUE))
       

########### last analysis: check state level sample value changes
# state by state to compare average of values.
# need to calculate mean of records by state
# tapply to do this

mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm=TRUE))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm=TRUE))
str(mn1)
summary(mn1)

# create df
df0 <- data.frame(state=names(mn0), mean=mn0)
df1 <- data.frame(state=names(mn1), mean=mn1)
dim(df0) # 52 rows

mrg <- merge(df0, df1, by="state") # df0:x, df1: y
# state, mean.x, mean.y

with(mrg, plot(rep(1999, 52), mrg[, 2], xlim=c(1998, 2013))) 
# plot 1999 data. x: repeate 52 times, y: mean values.
# x will be integer 1998,1999,...2013
with(mrg, points(rep(2012, 52), mrg[, 2]))

segments(rep(1999,52), mrg[,2], rep(2012,52), mrg[,3])

#################### project
setwd("~/course/ds_coursera/course4")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#dim(NEI)
#head(NEI)
#str(NEI)
#summary(NEI)
#any(is.na(NEI))

#dim(SCC)
#head(SCC)
#str(SCC)

# q1: sum Pollutant by year: tapply
unique(NEI$Pollutant)

sumdata <- with(NEI, tapply(Emissions, year, sum))
sumdata
sumdata <- sumdata /1000
#str(sumdata)
yeardata <- names(sumdata)
yeardata
yeardata <- as.integer(yeardata)

#par("mar")
#par(mar=c(1,1,1,1))

#rng <- range(sumdata)
plot(c(1,2,3,4), c(10,20,30,40))

plot(yeardata, sumdata)
plot(yeardata, sumdata, xlab = "Year", ylab="Emissions(ton)")
plot(yeardata, sumdata, type = "l", xlab = "Year", ylab="Emissions(ton)", ylim=rng)
points(yeardata, sumdata)


title(main = "Total Emissions From PM2.5")

dev.copy(png, "plot1.png")
dev.off()

barplot(sumdata, xlab = "Year", ylab="Emissions(ton)")
#barplot(sumdata, xlab = "Year", ylab="Emissions(ton)")


total.emissions <- aggregate(Emissions ~ year, NEI, sum)

#png('plot1.png')
barplot(height=total.emissions$Emissions, names.arg=total.emissions$year,
        xlab="years", ylab=expression('total PM'[2]*' emission'),
        main=expression('Total PM'[2]*' emissions at various years'))
#dev.off()








































































