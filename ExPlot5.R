
## Read Data and filter for Baltimore City

readNEI <- function() {  
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	neiBLT <- NEI[NEI$fips == "24510",]
	neiBLT
}

readSCC <- function() {
	SCC <- readRDS("./data/Source_Classification_Code.rds")
	SCC
}

chartMotData <- function () {
	SCC = readSCC()
	scode <- SCC[grep("Vehicles", SCC$EI.Sector),1]
	NEI = readNEI()
	df2 <- NEI[scode %in% NEI$SCC,] ## Bring only data that belongs to motor vehicles
	pmMotor <- with(df2, tapply(Emissions, year, sum, na.rm = T)) ## sum it up
	d3 <- data.frame(year=names(pmMotor) , Emission=pmMotor)
	d3$year <- as.Date(as.character(d3$year),"%Y")

	par(mfrow = c(1, 1), bg="gray90")
	with(d3, plot(d3[, 1], d3[,2], pch=19, , cex=1.3, col="orange", xlab="Years", ylab="Total PM2.5 pollution levels - Motor", main = "Motor Vehicle related Emissions in Baltimore City"))


}



