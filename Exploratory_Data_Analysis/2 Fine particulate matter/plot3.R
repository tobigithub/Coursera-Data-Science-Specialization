NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from
# 1999–2008 for Baltimore City? Which have seen increases in emissions from
# 1999–2008? Use the ggplot2 plotting system to make a plot answering this question.

subset <- NEI[which(NEI$fips == "24510"), ]
data <- aggregate(x = subset$Emissions, by = list(subset$year, subset$type), FUN = sum)
names(data) <- c("Year", "Type", "PM2.5Emissions")
# > str(data)
# 'data.frame':      16 obs. of  3 variables:
# $ Year          : int  1999 2002 2005 2008 1999 2002 2005 2008 1999 2002 ...
# $ Type          : chr  "NONPOINT" "NONPOINT" "NONPOINT" "NONPOINT" ...
# $ PM2.5Emissions: num  2108 1510 1510 1373 523 ...

require("ggplot2")
png("plot3.png")
ggplot(data = data,
       aes(x = Year, y = PM2.5Emissions, colour = Type)) +
       geom_line(lwd = 2) +
       ggtitle("PM 2.5 Emissions in Baltimore City 1999 - 2008 by type")
dev.off()
