#plot6.R
library(plyr)
library(ggplot2)
library(scales)

if (!exists("NEI")) {
	if(!file.exists("./data")) {
		hpcURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
		dir.create("data")
		download.file(url = hpcURL, destfile = "data/hpc.zip")
		unzip("data/hpc.zip", exdir = "data")
	}
	NEI <- readRDS("data/summarySCC_PM25.rds")
	SCC <- readRDS("data/Source_Classification_Code.rds")
	SCC <- SCC[,c("SCC", "Short.Name")]
}

# Interpret "motor vehicle" as anything under "Highway vehicle" category
########################################################################
mvSCC <- subset(SCC, grepl("highway veh", tolower(Short.Name)))
mvBalt <- subset(NEI, SCC %in% mvSCC$SCC & fips == "24510")
mvBaltByYear <- ddply(mvBalt, .(year), summarise, total = sum(Emissions))
# Normalize to 1999 = 1.
base <- mvBaltByYear[[1,"total"]]
mvBaltByYear$total <- mvBaltByYear$total / base
# Annotate with city for plot
mvBaltByYear$City <- "Baltimore"

mvLA <- subset(NEI, SCC %in% mvSCC$SCC & fips == "06037")
mvLAByYear <- ddply(mvLA, .(year), summarise, total = sum(Emissions))
# Normalize to 1999 = 1.
base <- mvLAByYear[[1,"total"]]
mvLAByYear$total <- mvLAByYear$total / base
# Annotate with city for plot
mvLAByYear$City <- "Los Angeles"

# Combine city data for plot
mvByYear <- rbind(mvBaltByYear, mvLAByYear)

# Plot. Use %age on y-axis.
p <- ggplot(mvByYear, aes(year, total)) + scale_y_continuous(labels=percent)
p <- p + geom_point(size = 3)
p <- p + facet_grid(. ~ City)
p <- p + geom_smooth(method = "loess", se = FALSE)
p <- p + ggtitle("Baltimore has seen a bigger change than LA in motor vehicle PM2.5 emissions")
p <- p + ylab("PM2.5 emissions normalized to 1999 levels") + xlab("Year")

png(file = "plot6.png", width = 720, height = 300)
print(p)
dev.off()
p <- NULL
