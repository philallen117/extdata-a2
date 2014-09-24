# Alternatives to ex2

# Alternative data processing - using data.table
 nei <- readRDS("summarySCC_PM25.rds")
        library(data.table)
        # Convert to a data table to get sums for each source
        dataTable<-data.table(nei)
        # Sort by year 
        setkey(dataTable, year)
        # Get sums of the emissions per year
        theSums<-dataTable[,sum(Emissions),by=key(dataTable)]
   
# And for plyr type stuff
		# Sort by year and type
        setkey(dataTable, year, type)
        # Get sums of each type of source per year
        theSums<-dataTable[,sum(Emissions),by=key(dataTable)]    
 
# And for joins, still using merge
           data_table_1 = data.table(nei, key="SCC")
            data_table_2 = data.table(scc, key="SCC")
            # Merge the two files by SSC column
            dt.merged <- merge(data_table_1, data_table_2)
            # Find all Coal Combustion related Emissions
            index1 <- with(dt.merged, grepl("comb", Short.Name,ignore.case=TRUE))
            index2 <- with(dt.merged, grepl("coal", Short.Name,ignore.case=TRUE))
            data2plot<-dt.merged[index1 & index2,]  #40440 rows         

# Very neat aggregation, here with data frames.
TotalEmissions <- aggregate(NEI$Emissions, by=list(NEI$year), FUN = sum, na.rm =TRUE) # 4 obs. of 2 variables
colnames(TotalEmissions) <- c("Year", "Total")
baltimoreTotalEmissions <- aggregate(baltimore$Emissions, by=list(as.factor(baltimore$year), baltimore$type), FUN = sum, na.rm=TRUE) # 8 obs. of 3 variables
colnames(baltimoreTotalEmissions) <- c("Year", "Type", "Total")


			
# very neat selection
# what do factors do for you?
# Just select the Baltimore data
pm25.balt <- NEI[NEI$fips==24510,]
pm25.balt$type <- as.factor(pm25.balt$type)
# Calculaye the emissions by year and type. 
tot.balt <- ddply(pm25.balt, .(year, type), summarise, sum=sum(Emissions, na.rm=T))
str(tot.balt)

# Neat way to select both LA and Balt
# Select the PM25 data for motor vehicles.
bala.mv <- pm25.bala[pm25.bala$SCC %in% mv$SCC,]
bala.mv$fips <- as.factor(bala.mv$fips)
levels(bala.mv$fips) <- c("Los Angeles", "Baltimore")
str(bala.mv)


# A different way to subset codes ... would grepl have worked, with logical and, nested inside subset?

# Find all SCC with COAL in either Level.Three or Level.Four and then 
# subset to those with SCC Level One containing "Combustion".
# scc.coal will then contain all of the coal combustion SCC data
l1 <- unique(grep("combustion",SCC$SCC.Level.One,value=TRUE, ignore.case=TRUE))
l3 <- unique(grep("coal",SCC$SCC.Level.Three,value=TRUE, ignore.case=TRUE))
l4 <- unique(grep("coal",SCC$SCC.Level.Four,value=TRUE, ignore.case=TRUE))
scc.coal <- SCC[SCC$SCC.Level.Three %in% l3 | SCC$SCC.Level.Four %in% l4,]
scc.coal <- scc.coal[scc.coal$SCC.Level.One %in% l1,]
rm(l1, l3, l4)

# Another way to subset codes
#look for "coal" in sources
Coal_in_level_3 <- grep("coal",SCC$SCC.Level.Three,ignore.case=TRUE)
Coal_in_level_4 <- grep("coal",SCC$SCC.Level.Four,ignore.case=TRUE)
coal_items = unique(c(Coal_in_level_3,Coal_in_level_4))
coal_items <- coal_items[order(coal_items)]


# Very, very simple base plot. Quite effective.
barplot(TotalEmissions$Total, width = 1, names.arg = TotalEmissions$Year, col = "blue",  ylab = "Total Emissions in Tons",  main= "PM2.5 Emissions in United States", xlab = "Year")

 
# No idea how ylab works, but expression delays computation
        with(theSums, 
            plot(year, V1, pch = 19, cex = 2,
                 col = rgb(0,1,0, alpha = 0.4),xlim=c(1999,2008),
                 main = "Total Emissions Decreasing over Time",
                 xlab = "Years", 
                 ylab=expression("Total "*PM[2.5]* ~ (In ~ tons ) ~ Emissions)))                 

# What does using theme do for you?
# Notice you don't have to assign the variable to use ggplot.

		ggplot(theSums, aes(x=year, y=V1)) +
             facet_wrap(~ type) + geom_point(col="red",size=5,alpha=.2) +
             geom_smooth(size = 1, linetype = 1, method = "lm", se = TRUE) +
             ggtitle("For Baltimore City Emissions: Non-Road/On-Road & \nNon-Point Sources are Decreasing.\n 
                         Point trending Upward over time \n") +
            theme(plot.title = element_text(lineheight=.8, face="bold",size=11)) +        
            labs(x="Years", y=expression(" "*PM[2.5]* ~ (In ~ tons )~ Emissions))            

			                ggplot(theSums, aes(x=year, y=V1)) +
                # Let's use some transparent points 
                geom_point(col="blue",alpha = 1/5,size=6) +
                 ggtitle("Baltimore City Motor Vehicle Emissions \nhave Decreased Over Time \n") +
                theme(plot.title = element_text(lineheight=.8, face="bold",size=11)) +    
                labs(x="Years", y=expression(" "*PM[2.5]* ~ (In ~ tons )~ "Emitted from Motor Vehicles")) +                
                # Add a theme to make plot look nicer
                 theme_economist()+
                    # Make the axis titles bold and a little bigger
                theme(axis.title=element_text(size=14,face="bold")) +
                    # Connect the points
                 geom_line(data=theSums, mapping=aes(x=year, y=V1), linetype=3, col="blue",size=1) 
            # Save the plot to a 500x500 pixel png file
            ggsave('plot5.png', width=5, height=5, dpi=100)
            # Close png device
            dev.off()

# Clean-up ... rm gets rid of stuff?
rm(tot.by.year, pm25.trend)

# Nice clean solution!!!
# Basic plot area.
plot(tot.balt$year, tot.balt$sum
     , main=expression("Total "*PM[2.5]*" Emissions in Baltimore 1999 to 2008")
     , xlab="Year"
     , ylab=expression("Emissions x"*10^3*" tons")
     , type="n"
     , las = 1
)

# Add the points 
points(tot.balt$year, tot.balt$sum, pch=17, cex=2, col="red")

# and then the trend line
pm25.trend <- lm(sum ~ year, tot.balt)
abline(pm25.trend, lwd = 2, lty = 2, col="blue")

# finally the legend in the top right
legend("topright"
       ,c("Trend line")
       , lty = 2 
       , lwd = 2
       , col = "blue")

# Another way you can direct base plot output, apparently
plot(Baltimore_Total_per_year,type="h",col = "red", main="PM2.5 Total Emissions in Baltimore City", lwd=30, lend=2,ylab="Kilo-Tons")
dev.print(png,file="Plot2.png",width=480,height=480)

# Very quick multi-line graph (types). Shame about default colors. Nice plyr slicing. Note also the constant 10E3
library(plyr)
Baltimore_year_source <- ddply(NEI[NEI$fips=="24510",],~year+type,summarise,sum=sum(Emissions)/10E3)
qplot(year,sum,data=Baltimore_year_source,
      color=type,main="PM2.5 Emissions in Baltimore City", 
      ylab="Kilo-Tons",
      geom=c("line"))
dev.print(png,file="Plot3.png",width=480,height=480)

# Using qplot with facet_grid. Not sure what free scales (between facet graphs) actually does here.
# And another approach to assigning / not assigning plot values.
p <-qplot(Year, Total, data = baltimoreTotalEmissions,  geom= "bar",stat="identity", width = .5, color = Year)
p + facet_grid(. ~ Type, scales= "free") + theme(legend.position = "top") 
+ labs(title="PM2.5 Emissions in Baltimore City by Type of Source") + ylab("Total Emissions in Tons")

# How to do a cumulative bar (fill=Sector)
p2 <- ggplot(cvS, aes(x = factor(Year), y=Total, fill=Sector, width = .5)) + geom_bar(stat="identity")
p2 + facet_grid(City ~ ., scales="free_y") + theme(legend.position = 'top') + labs(title = 'PM2.5 Emissions from Vehicle Sources in LA County & Baltimore City') +  ylab("Total Emissions in Tons")


