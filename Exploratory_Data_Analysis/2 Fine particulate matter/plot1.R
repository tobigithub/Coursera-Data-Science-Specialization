NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

data <- aggregate(x = NEI$Emissions, by = list(NEI$year), FUN = sum)
names(data) <- c("Year", "PM2.5 Emissions")
# > data
# Year           PM2.5 Emissions
# 1 1999         7332967
# 2 2002         5635780
# 3 2005         5454703
# 4 2008         3464206

png("plot1.png")
plot(data, type = "l", lwd = 3, 
     main = "Sum of PM2.5 Emissions in the USA 1999 - 2008")
dev.off()
