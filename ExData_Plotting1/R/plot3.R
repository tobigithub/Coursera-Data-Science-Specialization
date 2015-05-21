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
# Sub_metering time series plots
png("plot3.png")
plot(hhp2$datetime, hhp2$Sub_metering_1, type="l", xlab = " ",
     ylab = "Energy sub metering")
lines(hhp2$datetime, hhp2$Sub_metering_2, col = "red", type = "l")
lines(hhp2$datetime, hhp2$Sub_metering_3, col = "blue", type = "l")
legend("topright", lwd = 2, col = c("black", "blue", "red"), 
       legend = colnames(hhp2[, 8:10]))
dev.off()
