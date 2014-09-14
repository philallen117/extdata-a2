## Load and process data for assigment 1, leaving it in global variable twoDays

# Get data from web.

if(!file.exists("./data")) {
	hpcURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
	dir.create("data")
	download.file(url = hpcURL, destfile = "data/hpc.zip")
	unzip("data/hpc.zip", exdir = "data")
}

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

