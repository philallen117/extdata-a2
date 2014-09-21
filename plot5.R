# plot5.R

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

# Interpret "motor vehicle" as anything under "Highway vehicle"

mvSCC <- subset(SCC, grepl("highway veh", tolower(Short.Name)))
mvBaltNEI <- subset(NEI, SCC %in% mvSCC$SCC & fips == "24510")
mvBalNEIByYear <- ddply(mvNEI, .(year), summarise, total = sum(Emissions))

p <- ggplot(q5data, aes(year, total))
p <- p + geom_point()
p <- p + geom_smooth(method = "loess", se = FALSE)
p <- p + ggtitle("Trend of highway vehicle PM2.5 emissions in Baltimore")
p <- p + ylab("Total PM2.5 emissions (ton)") + xlab("Year")

png(file = "plot5.png", width = 720, height = 300)
print(p)
dev.off()
p <- NULL
