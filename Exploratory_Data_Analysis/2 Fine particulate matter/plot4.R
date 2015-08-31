NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 4
# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

# Search for Comb ... Coal in the sector names
# These are the indices
sectors <- grep(x = SCC$EI.Sector, pattern = "Comb.*Coal")
subsetSCC <- SCC$SCC[sectors]
# Extract those rows from NEI:
subsetNEI <- NEI[which(NEI$SCC %in% subsetSCC), ]

# Aggregate sums over years
data <- aggregate(x = subsetNEI$Emissions, by = list(subsetNEI$year), FUN = sum)
names(data) <- c("Year", "PM2.5 Emissions")
# > data
# Year          PM2.5 Emissions
# 1 1999        572126.5
# 2 2002        546789.2
# 3 2005        552881.5
# 4 2008        343432.2

png("plot4.png")
plot(data, type = "l", lwd = 3, 
     main = "Sum of PM2.5 Emissions from coal combustion 1999 - 2008")
dev.off()
