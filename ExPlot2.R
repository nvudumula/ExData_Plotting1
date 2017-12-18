## Read Emissions data and filter for Baltimore City
## Sum up the data by year and plot the graph

ReadandPrintBLT <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	neiBLT <- NEI[NEI$fips == "24510",]
	pmBLT <- with(neiBLT, tapply(Emissions, year, sum, na.rm = T))
	d2 <- data.frame(year=names(pmBLT) , Emission=pmBLT)
	d2$year <- as.Date(as.character(d2$year),"%Y")
	par(mfrow = c(1, 1), bg="gray90")
	opt <- options("scipen" = 20)
	with(d2, plot(d2[, 1], d2[,2], pch=19, , cex=1.3, col="orange", xlab="Years", ylab="Total PM2.5 pollution levels", main="PM 2.5 pollution totals from 1998 to 2008 in Baltimore City"))
}
