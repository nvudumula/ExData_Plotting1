
## Read Data

readNEI <- function() {  
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	NEI
}

## Read codes data

readSCC <- function() {
	SCC <- readRDS("./data/Source_Classification_Code.rds")
	SCC
}

chartMotDataBL <- function () {
	SCC = readSCC()
	scode <- SCC[grep("Vehicles", SCC$EI.Sector),1]
	NEI = readNEI()

	neiBLT <- NEI[NEI$fips == "24510",] ## filter for Baltimore City
	neiLOS <- NEI[NEI$fips == "06037",] ## filter for Los Angeles City

	df2 <- neiBLT[scode %in% neiBLT$SCC,] ## Bring only data that belongs to motor vehicles
	pmMotor1 <- with(df2, tapply(Emissions, year, sum, na.rm = T)) ## sum it up
	d4 <- data.frame(year=names(pmMotor1) , Emission=pmMotor1)
	d4$year <- as.Date(as.character(d4$year),"%Y")

	df3 <- neiLOS[scode %in% neiLOS$SCC,] ## Bring only data that belongs to motor vehicles
	pmMotor2 <- with(df3, tapply(Emissions, year, sum, na.rm = T)) ## sum it up
	d5 <- data.frame(year=names(pmMotor2) , Emission=pmMotor2)
	d5$year <- as.Date(as.character(d5$year),"%Y")

	## Draw the two charts

	par(mfrow = c(1, 2), bg="gray90")
	with(d4, plot(d4[, 1], d4[,2], pch=19, , cex=1.3, col="orange", xlab="Years", ylab="Total PM2.5 pollution levels - Motor", main = "Motor Vehicle related Emissions in Baltimore City"))
	with(d5, plot(d5[, 1], d5[,2], pch=19, , cex=1.3, col="orange", xlab="Years", ylab="Total PM2.5 pollution levels - Motor", main = "Motor Vehicle related Emissions in Los Angeles"))


}



