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
# Time series plot
png("plot2.png")
plot(hhp2$datetime, hhp2$Global_active_power, type = "l", xlab=" ", 
     ylab="Global active power (kilowatts)")
dev.off()
