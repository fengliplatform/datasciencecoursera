# plot2.R to read data and generate plot2.png

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

# perform plot 2 and generate plot2.png
powerdata$datetime <- strptime(paste(powerdata$Date,powerdata$Time,sep=" "),"%d/%m/%Y %H:%M:%S")
with(powerdata, plot(datetime, Global_active_power, type="l", xlab="", ylab="Global Active Power(kilowatts)"))

dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()