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

#plot3
plot(x = power$DateTime, y = power$Sub_metering_1, type = "l", ylab = "Energy Sub Metering", xlab = "")
lines(x = power$DateTime, y = power$Sub_metering_2, type = "l", col = "red")
lines(x = power$DateTime, y = power$Sub_metering_3, type = "l", col = "blue")
legend("topright", legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), col = c("black","red","blue"), lty = 1)

#saving png
dev.copy(png, "plot3.png", width=dev.size("px")[1], height=dev.size("px")[2])
dev.off()
