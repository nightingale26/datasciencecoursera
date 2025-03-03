library(reshape2)
library(dplyr)

# creating directory, downloading and unzipping files
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile = "./powerzip.zip", method = "libcurl")
unzip(zipfile = "./powerzip.zip", exdir = "./")

#reading and filtering the data
power <- read.table("./household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?")
power$DateTime <- paste(power$Date, power$Time, sep = " ")
power$DateTime <- strptime(power$DateTime, "%d/%m/%Y %H:%M:%S")
power$Global_active_power <- as.numeric(power$Global_active_power)
power <- power %>% filter(between(DateTime, as.Date('2007-02-01'),as.Date('2007-02-03')))

#plot2
plot(x = power$DateTime, y = power$Global_active_power, type = "l", col = "black", xlab = "", ylab = "Global Active Power (kilowatts)")

#saving png
dev.copy(png, "plot2.png")
dev.off()
