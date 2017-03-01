# plot3.R to read data and generate plot3.png

# Download and unzip data
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileURL, destfile="./project.zip")
unzip("./project.zip")

# Extract data only including those in Feb1 and Feb2
system("head -1 household_power_consumption.txt > feb1-2.txt")
system("cat household_power_consumption.txt | grep '^1/2/2007' >> feb1-2.txt")
system("cat household_power_consumption.txt | grep '^2/2/2007' >> feb1-2.txt")

# Read data to R
powerdata <- read.table("./feb1-2.txt", sep=";", header=TRUE, stringsAsFactors = FALSE)

# perform plot3 
powerdata$datetime <- strptime(paste(powerdata$Date,powerdata$Time,sep=" "),"%d/%m/%Y %H:%M:%S")
with(powerdata, plot(datetime, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering", col="black"))
with(powerdata, lines(datetime, Sub_metering_2, type="l", col="red"))
with(powerdata, lines(datetime, Sub_metering_3, type="l", col="blue"))
legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col=c("black","red","blue"), lty=1, pt.cex=1, cex=0.7)

# generate plot3.png
dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()
