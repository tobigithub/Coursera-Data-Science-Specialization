NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

baltimore <- NEI[which(NEI$fips == "24510"), ]
# To select motor vehicles use type == ON-ROAD
baltimore <- baltimore[which(baltimore$type == "ON-ROAD"), ]

# Aggregate sums over years
data <- aggregate(x = baltimore$Emissions, 
                  by = list(baltimore$year), FUN = sum)
names(data) <- c("Year", "PM2.5 Emissions")
# > data
# Year PM2.5 Emissions
# 1 1999       346.82000
# 2 2002       134.30882
# 3 2005       130.43038
# 4 2008        88.27546

png("plot5.png")
plot(data, type = "l", lwd = 3, 
     main = "PM2.5 Emissions from motor vehicles in Baltimore 1999 - 2008",
     cex.main = 0.9)
dev.off()
