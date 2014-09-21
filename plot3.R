# plot3.R
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

baltByTypeYear <- ddply(subset(NEI, fips == "24510"), .(type, year),
			summarise, total = sum(Emissions))

p <- ggplot(baltByTypeYear, aes(year, total))
p <- p + geom_point()
p <- p + facet_grid(. ~ type)
p <- p + geom_smooth(method = "loess", se = FALSE)
p <- p + ggtitle("Comparison of PM2.5 emissions trends by type in Baltimore")
p <- p + ylab("Total PM2.5 emissions (ton)") + xlab("Year")

png(file = "plot3.png", width = 720, height = 300)
print(p)
dev.off()
p <- NULL
