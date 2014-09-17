## plot2.R

if (!exists("NEI")) {
	if(!file.exists("./data")) {
		hpcURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
		dir.create("data")
		download.file(url = hpcURL, destfile = "data/hpc.zip")
		unzip("data/hpc.zip", exdir = "data")
	}
	NEI <- readRDS("data/summarySCC_PM25.rds")
	SCC <- readRDS("data/Source_Classification_Code.rds")
}

q2 <- ddply(subset(NEI, fips == "24510"), .(year),
			summarise, total = sum(Emissions))
m2 <- lm(total ~ year, q2)

png(file = "plot2.png", width = 600, height = 600)

plot.new()
plot.window(mar=c(2, 8, 1, 1),
			xlim = c(1999,2008), xaxs = "i",
			ylim = c(0, 4000), yaxs = "i")
axis(1, c(1999,2002,2005,2008))
axis(2, las = "1")
title(xlab="Year")
title(ylab="Total PM2.5 emissions (ton)")
title(main = "Baltimore PM2.5 trend from 1999 to 2008")
with(q2, points(year, total, pch = 19))
abline(m2, col="green", lwd="2")
dev.off()
