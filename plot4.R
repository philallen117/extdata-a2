# plot4.R
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

NEI <- merge(NEI, SCC, by = c("SCC"))
coalSCC <- subset(SCC, grepl("coal", tolower(Short.Name)) &
			 	grepl("combustion", tolower(Short.Name)))
coalNEI <- merge(NEI, coalSCC, by = c("SCC")) # simple way to do membership test
coalByYear <- ddply(coalNEI, .(year), summarise, total = sum(Emissions))

p <- ggplot(coalByYear, aes(year, total)) + ylim(0,2000)
p <- p + geom_point(size = 3)
p <- p + geom_smooth(method = "loess", se = FALSE)
p <- p + ggtitle("PM2.5 emissions related to coal combustion rise following a dip")
p <- p + ylab("Total PM2.5 emissions (ton)") + xlab("Year")

png(file = "plot4.png", width = 450, height = 300)
print(p)
dev.off()
p <- NULL
