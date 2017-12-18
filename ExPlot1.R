
## Read the data and sum it to draw chart for all of united states

readAndPrintUSA <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	pmUSA <- with(NEI, tapply(Emissions, year, sum, na.rm = T))
	d1 <- data.frame(year=names(pmUSA) , Emission=pmUSA)

	par(mfrow = c(1, 1))
	opt <- options("scipen" = 20)
	with(d1, plot(d1[, 1], d1[,2], pch=19, , cex=1.3, col="orange", xlab="Years", ylab="Total PM2.5 pollution levels", main="PM 2.5 pollution totals from 1998 to 2008 - United States"))
}


