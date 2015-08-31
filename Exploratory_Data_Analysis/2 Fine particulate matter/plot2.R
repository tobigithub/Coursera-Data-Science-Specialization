NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 2
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
# to make a plot answering this question.

subset <- NEI[which(NEI$fips == "24510"), ]
data <- aggregate(x = subset$Emissions, by = list(subset$year), FUN = sum)
names(data) <- c("Year", "PM2.5 Emissions")
# > data
# Year PM2.5 Emissions
# 1 1999        3274.180
# 2 2002        2453.916
# 3 2005        3091.354
# 4 2008        1862.282

png("plot2.png")
plot(data, type = "l", lwd = 3, 
     main = "PM2.5 Emissions in Baltimore City 1999 - 2008")
dev.off()
