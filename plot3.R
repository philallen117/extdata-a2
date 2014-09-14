# plot3.R
library(ggplot2)

## Get prepared data, if necessary
if (!exists("NEI")) source("getdata.R")

q3 <- ddply(subset(NEI, fips == "24510"), .(type, year),
			summarise, total = sum(Emissions))

p <- ggplot(q3, aes(year, total))
p <- p + geom_point()
p <- p + facet_grid(. ~ type)
p <- p + geom_smooth(method = "loess", se = FALSE)
p <- p + ggtitle("Trends for PM2.5 emission types in Baltimore")
p <- p + ylab("Total PM2.5 emissions (ton)") + xlab("Year")

png(file = "plot3.png", width = 720, height = 300)
print(p)
dev.off()
