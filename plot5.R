# plot5.R
library(plyr)
library(ggplot2)

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
mvBaltNEI <- subset(NEI, SCC %in% mvSCC$SCC & fips == "24510")
mvBalNEIByYear <- ddply(mvBaltNEI, .(year), summarise, total = sum(Emissions))

p <- ggplot(mvBalNEIByYear, aes(year, total)) + ylim(0,400)
p <- p + geom_point(size = 4)
p <- p + geom_smooth(method = "loess", se = FALSE)
p <- p + ggtitle("In Baltimore,\nPM2.5 emissions from highway vehicles have fallen")
p <- p + ylab("Total PM2.5 emissions (ton)") + xlab("Year")

png(file = "plot5.png", width = 450, height = 300)
print(p)
dev.off()
p <- NULL
