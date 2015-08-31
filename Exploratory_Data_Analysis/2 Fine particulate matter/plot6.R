NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

subset <- NEI[which(NEI$fips == "24510" | NEI$fips == "06037"), ]
# Change fips and its code to readable values and make it a factor variable:
names(subset)[1] <- "City"
subset$City[subset$City == "24510"] <- "Baltimore"
subset$City[subset$City == "06037"] <- "Los Angeles County"
subset$City <- factor(subset$City)
# To select motor vehicles use type == ON-ROAD
subset <- subset[which(subset$type == "ON-ROAD"), ]

data <- aggregate(x = subset$Emissions, 
                  by = list(subset$year, subset$City), FUN = sum)
names(data) <- c("Year", "City", "PM2.5Emissions")
# > data
# Year               City PM2.5Emissions
# 1 1999          Baltimore      346.82000
# 2 2002          Baltimore      134.30882
# 3 2005          Baltimore      130.43038
# 4 2008          Baltimore       88.27546
# 5 1999 Los Angeles County     3931.12000
# 6 2002 Los Angeles County     4273.71020
# 7 2005 Los Angeles County     4601.41493
# 8 2008 Los Angeles County     4101.32100

### Since the question is asking for *changes* over time the next command
### divides all values by the emissions of 1999 (per city). Thus, changes are more
### apparent. Absolute values are a little hard to interpret since Los Angeles
### County has much higher emissions than Baltimore
data$PM2.5Emissions[1:4] <- data$PM2.5Emissions[1:4] / data$PM2.5Emissions[1]
data$PM2.5Emissions[5:8] <- data$PM2.5Emissions[5:8] / data$PM2.5Emissions[5]

require("ggplot2")
png("plot6.png")
ggplot(data = data,
       aes(x = Year, y = PM2.5Emissions, colour = City)) +
       geom_line(lwd = 2) +
       ggtitle("PM 2.5 Emissions in Baltimore City and Los Angeles County 1999 - 2008") +
       theme(legend.position = 'bottom')
dev.off()
