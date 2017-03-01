#
# principle of Graph
# 1. show comparison
# 2. show explanation by you
# 3. show multivariate data
# 4. integration of evidence
# 5. describe evidence with correct label etc.
# 6. contents is king

#################### week1 lesson 1

data("mtcars")
head(mtcars)
names(mtcars)

########### one dimention graph
# Five number summary
summary(mtcars$mpg)

#boxplot
boxplot(mtcars$mpg, col="blue")
abline(h=20)

#histogram
hist(mtcars$mpg, col="green", breaks=10)
rug(mtcars$mpg)
abline(v=median(mtcars$mpg), lwd=4, col="red")

#bar
table(mtcars$cyl)
barplot(table(mtcars$cyl), col = "wheat", main = "Number of cars by cylinder")

########## two dimention graph
# multiple box plot
boxplot(mtcars$mpg ~ mtcars$cyl, col="red")

par(mfrow=c(3,1), mar=c(4,4,2,1))
hist(subset(mtcars, cyl==4)$mpg)
hist(subset(mtcars, cyl==6)$mpg)
hist(subset(mtcars, cyl==8)$mpg)
par(mfrow=c(1,1), mar=c(4,4,2,1))



# multiple/overlayed 1-d (lattice/ggplot2)

# scatter plot
with(mtcars, plot(wt, mpg, col=cyl))
abline(h=20, lwd=3,lty=2)

par(mfrow=c(1,3))
with(subset(mtcars, cyl==4), plot(wt,mpg))
with(subset(mtcars, cyl==6), plot(wt,mpg))
with(subset(mtcars, cyl==8), plot(wt,mpg))
par(mfrow=c(1,1))


#smooth scatterplots

#### multiple/overlayed 2-d: coplot
#### using color, size, shape to add dimentions
#### spinning plot
#### 3-d plot

#################### week1 lesson 2
# plotting systems in R
# 1. basic plot. draw piece by piece: dot, line, title etc. series of R code
# box plot, abline...
data(cars)
with(cars, plot(speed, dist))

# 2. lattice plot
# single call (not multiple R codes): xyplot, bwplot
library(lattice)
state <- data.frame(state.x77, region=state.region)
names(state)
xyplot(Life.Exp ~ Income, data=state)
xyplot(Life.Exp ~ Income | region, data=state, layout=c(4,1))

# 3. ggplot2
# language, grammer
# can add pieces later
# condition plot
library(ggplot2)
data(mpg)
qplot(displ, hwy, data=mpg)

##### w1 l3
library(datasets)
with(airquality, plot(Wind, Ozone))
title(main="Ozone and Wind in NY city")
with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))
with(subset(airquality, Month!=5), points(Wind, Ozone, col="red"))
legend("topright", pch=1,col=c("blue","red"), legend=c("May", "Other"))

model <- lm(Ozone~Wind, airquality)
abline(model, lwd=2)

par(mfrow=c(1,3), mar=c(4,4,2,1),oma=c(0,0,2,0))
with(airquality, {
  plot(Wind, Ozone, main="Wind")
  plot(Solar.R, Ozone, main="Solar.R")
  plot(Temp, Ozone, main="Temp")
  mtext("Ozone vs Wind, Solar.R and Temp", outer=TRUE)
})
par(mfrow=c(1,1))

### w1 l4
x <- rnorm(100)
hist(x)
y <- rnorm(100)
plot(x, y)
par(mar=c(2,2,2,2))
plot(x,y)
par(mar=c(4,4,2,2))
plot(x,y)
plot(x,y, pch=2)
plot(x,y, pch=3, xlab="Weight", ylab="Height")

title("Title")
text(-2,-2, "label")
fit <- lm(y~x)
abline(fit, lwd=3, lty=2)
legend("topleft", legend="Data", pch=3)

z <- rpois(100,2)
plot(x, z)

par(mfrow=c(2,1))
plot(x,y)
plot(x,z)

par(mfrow=c(1,2))
plot(x,y)
plot(x,z)

par(mfrow=c(1,1))

x<- rnorm(100)
y <- x + rnorm(100)
plot(x,y)
g <- gl(2, 50, label=c("Male","Female"))
g
levels(g)
plot(x, y, type="n")
points(x[g == "Male"], y[g=="Male"], col="green")
points(x[g=="Female"], y[g=="Female"], col="red", pch=19)




#############

?Devices
setwd("~/course/ds_coursera/course4")

with(faithful, plot(eruptions, waiting))
title("Faithful")

with(faithful, plot(eruptions, waiting))
title("Faithful")
dev.copy(png, file="plot2.png")
dev.off()

with(faithful, plot(eruptions, waiting))
title("Faithful")
dev.copy2pdf(file="new.pdf")
dev.off()

pdf(file="plot1.pdf")
with(faithful, plot(eruptions, waiting))
title("Faithful")
dev.off()

############################ w1 project
setwd("~/course/ds_coursera/course4")

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileURL, destfile="./project.zip")
list.files("./")
unzip("./project.zip")
list.files("./")

system("head -1 household_power_consumption.txt > feb1-2.txt")
system("cat household_power_consumption.txt | grep '^1/2/2007' >> feb1-2.txt")
system("cat household_power_consumption.txt | grep '^2/2/2007' >> feb1-2.txt")

powerdata <- read.table("./feb1-2.txt", sep=";", header=TRUE, stringsAsFactors = FALSE)
class(powerdata)
dim(powerdata)
str(powerdata)
head(powerdata)

# plot 1
powerdata <- read.table("./feb1-2.txt", sep=";", header=TRUE, stringsAsFactors = FALSE)

hist(powerdata$Global_active_power, 
     main="Global Active Power", 
     col="red",
     breaks=15,
     ylim = c(0,1200),
     xlab="Global Active Power(kilowatts)",
     ylab="Frequency")
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()


# plot2
powerdata <- read.table("./feb1-2.txt", sep=";", header=TRUE, stringsAsFactors = FALSE)

powerdata$datetime <- strptime(paste(powerdata$Date,powerdata$Time,sep=" "),"%d/%m/%Y %H:%M:%S")
with(powerdata, plot(datetime, Global_active_power, type="l", xlab="", ylab="Global Active Power(kilowatts)"))
dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()


# plot 3
powerdata <- read.table("./feb1-2.txt", sep=";", header=TRUE, stringsAsFactors = FALSE)
powerdata$datetime <- strptime(paste(powerdata$Date,powerdata$Time,sep=" "),"%d/%m/%Y %H:%M:%S")
with(powerdata, plot(datetime, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering", col="black"))
with(powerdata, lines(datetime, Sub_metering_2, type="l", col="red"))
with(powerdata, lines(datetime, Sub_metering_3, type="l", col="blue"))
legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"), lty=1)

dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()

# plot 4
powerdata <- read.table("./feb1-2.txt", sep=";", header=TRUE, stringsAsFactors = FALSE)
powerdata$datetime <- strptime(paste(powerdata$Date,powerdata$Time,sep=" "),"%d/%m/%Y %H:%M:%S")

par(mfrow=c(2,2))

# first
with(powerdata, plot(datetime, Global_active_power, type="l", xlab="", ylab="Global Active Power"))

# second
with(powerdata, plot(datetime, Voltage, type="l", xlab="datetime", ylab="Voltage"))

plot(data$Time, data$Voltage, type="l",
     xlab="datetime", ylab="Voltage")
# third
with(powerdata, plot(datetime, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering", col="black"))
with(powerdata, lines(datetime, Sub_metering_2, type="l", col="red"))
with(powerdata, lines(datetime, Sub_metering_3, type="l", col="blue"))
legend("topright", 
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col=c("black","red","blue"), 
       lty=1,
       box.lwd=0)

# fourth
with(powerdata, plot(datetime, Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power"))

dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()

par(mfrow=c(1,1))


