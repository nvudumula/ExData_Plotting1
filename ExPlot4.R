## first read SCC code related to coal into a vector
## Using that vector read data that belongs to coal from NEI data frame
## Sum up PM2.5 emissions by year and plot

readNEI <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	NEI
}

readSCC <- function() {
	SCC <- readRDS("./data/Source_Classification_Code.rds")
	SCC
}

chartData <- function () {
	SCC = readSCC()
	scode <- SCC[grep("Coal", SCC$EI.Sector),1]
	NEI = readNEI()
	df2 <- NEI[scode %in% NEI$SCC,] ## Bring only data that belongs to Coal
	pmCoal <- with(df2, tapply(Emissions, year, sum, na.rm = T)) ## sum it up
	d2 <- data.frame(year=names(pmCoal) , Emission=pmCoal)
	d2$year <- as.Date(as.character(d2$year),"%Y")

	par(mfrow = c(1, 1), bg="gray90")
	with(d2, plot(d2[, 1], d2[,2], pch=19, , cex=1.3, col="orange", xlab="Years", ylab="Total PM2.5 pollution levels - Coal", main = "Coal related Emissions across United States"))
}

