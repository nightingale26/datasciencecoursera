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

#plot4
par(mfrow = c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
plot(x = power$DateTime, y = power$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
plot(x = power$DateTime, y = power$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
plot(x = power$DateTime, y = power$Sub_metering_1, type = "l", ylab = "Energy Sub Metering", xlab = "")
lines(x = power$DateTime, y = power$Sub_metering_2, type = "l", col = "red")
lines(x = power$DateTime, y = power$Sub_metering_3, type = "l", col = "blue")
legend(x = "topright", legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), col = c("black","red","blue"), lty = 1, lwd = 1.5, bty = "n")
plot(x = power$DateTime, y = power$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")

#saving png
dev.copy(png, "plot4.png")
dev.off()
