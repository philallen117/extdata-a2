## plot1.R

## Get prepared data, if necessary
if (!exists("NEI")) source("getdata.R")

q1 <- ddply(NEI, .(year), summarise, total = sum(Emissions))
m1 <- lm(total ~ year, q1)

png(file = "plot1.png", width = 600, height = 600)
plot.new()
plot.window(mar=c(2, 8, 1, 1),
			xlim = c(1999,2008), xaxs = "i",
			ylim = c(0, 8000000), yaxs = "i")
axis(1, c(1999,2002,2005,2008))
axis(2, las = "1")
title(xlab="Year")
title(ylab="Total PM2.5 emissions (ton)")
title(main = "PM2.5 emissions trend from 1999 to 2008")
with(q1, points(year, total, pch = 19))
abline(m1, col="green", lwd="2")
dev.off()
