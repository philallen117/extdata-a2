## plot1.R
library("plyr")

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

totalByYear <- ddply(NEI, .(year), summarise, total = sum(Emissions))

# Fit linear model to illustrate trend
lmTotalByYear <- lm(total ~ year, totalByYear)

# Plot using low level functions to prevent false origin on y axis.

png(file = "plot1.png", width = 600, height = 600)
plot.new()
plot.window(mar=c(2, 8, 1, 1),
			xlim = c(1999,2008), xaxs = "r",
			ylim = c(0, 8000000), yaxs = "i")
axis(1, c(1999,2002,2005,2008))
axis(2, las = "1")
title(xlab="Year")
title(ylab="Total PM2.5 emissions (ton)")
title(main = "PM2.5 emissions trend from 1999 to 2008")
with(totalByYear, points(year, total, pch = 19))
abline(lmTotalByYear, col="green", lwd="2")
dev.off()
