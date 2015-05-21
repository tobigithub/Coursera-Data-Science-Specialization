Sys.setlocale(category = "LC_TIME", locale = "C") # English days
setwd("/home/")
unzip("/home/exdata-data-household_power_consumption.zip")

hhp <- read.csv("~/household_power_consumption.txt", 
                sep=";", na.strings = "?")

# Combine date and time, format it and add it to the data frame
datetime <- paste(hhp$Date, hhp$Time)
datetime <- strptime(datetime, format="%d/%m/%Y %H:%M:%S")
hhp <- cbind(datetime, hhp)
# Convert Date column to date class
hhp$Date <- as.Date(hhp$Date, format="%d/%m/%Y")
# Subset: first two days of Feb 2007
hhp2 <- subset(x = hhp, subset = Date == "2007-02-01" | Date == "2007-02-02")

### PLOT
# 4 plots: Global active power, voltage, energy sub metering, 
# global reactive power
png("plot4.png")
par("mar") # 5.1 4.1 4.1 2.1
par(mar = c(4, 4, 4, 3))
par(mfrow = c(2, 2))

plot(hhp2$datetime, hhp2$Global_active_power, type = "l", xlab = " ",
     ylab = "Global active power")

plot(hhp2$datetime, hhp2$Voltage, type = "l", xlab = "datetime",
     ylab = "Voltage")

plot(hhp2$datetime, hhp2$Sub_metering_1, type="l", xlab = " ",
     ylab = "Energy sub metering")
lines(hhp2$datetime, hhp2$Sub_metering_2, col = "red", type = "l")
lines(hhp2$datetime, hhp2$Sub_metering_3, col = "blue", type = "l")
legend("topright", lwd = 2, col = c("black", "blue", "red"), 
       legend = colnames(hhp2[, 8:10]), bty = "n")

plot(hhp2$datetime, hhp2$Global_reactive_power, type = "l", xlab = "datetime",
     ylab = "Global_reactive_power")

dev.off()
