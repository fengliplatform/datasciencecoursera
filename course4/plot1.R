# plot1.R to read data and generate plot1.png

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

# perform plot1 
hist(powerdata$Global_active_power, 
     main="Global Active Power", 
     col="red",
     ylim = c(0,1200),
     xlab="Global Active Power(kilowatts)",
     ylab="Frequency")

# generate plot1.png
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()
