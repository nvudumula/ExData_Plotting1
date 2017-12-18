library("ggplot2")

## In this program I have followed three approaches as the data ranges are quite varied and it is somewhat difficult to say for sure the trend is decreasing or staying 
## flat with data.

## In the first method I used data as-is out of the data frame draw four charts using ggplot. I was not quite sure on-road and non-road pollution types if the trend
## decreasing or stying flat.

## In the next method I used log10 to reduce the range of values and hopefully make the trends clear. This produced clear trends and all of them were trending down.

## In the third method I crated subsets for each of the four types and calculted mean for each of those subsets at the year level
## next I combined four data sets into one data set to use the data in ggplot.
## It confirmed that for each of the four types of pollution the trend is decreasing

##-----------------------------------------------------------------------------------------------------------


## Main Data set code

readData <- function (){
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	NEI
}

chartPlain <- function(){
	NEI <- readData()
	neiBLT <- NEI[NEI$fips == "24510",]
	df <- neiBLT
	df$year <- as.Date(as.character(df$year),"%Y")
	par(mfrow = c(1, 4), bg="gray90")
	ggplot(df, aes(year, (Emissions))) + geom_point() + facet_grid(. ~ type) + geom_smooth(method = 'lm') ## Method 1
}

chartLog <- function(){
	NEI <- readData()
	neiBLT <- NEI[NEI$fips == "24510",]
	df <- neiBLT
	par(mfrow = c(1, 4), bg="gray90")
	ggplot(df, aes(year, log10(Emissions))) + geom_point() + facet_grid(. ~ type) + geom_smooth(method = 'lm') ## Method 2

}

## Third method

## Baltimore point data set

ChartMean <- function() {

	NEI <- readData()
	neiBLT <- NEI[NEI$fips == "24510",]
	df <- neiBLT

	neiBltPt <- neiBLT[neiBLT$type == "POINT",]
	pmBltPt <- with(neiBltPt, tapply(Emissions, year, mean, na.rm = T))
	d32 <- data.frame(type="Point", year=names(pmBltPt) , Emission=pmBltPt)
	d32$year <- as.Date(as.character(d32$year),"%Y")


	## Baltimore non point data set

	neiBltNp <- neiBLT[neiBLT$type == "NONPOINT",]
	pmBltNp <- with(neiBltNp, tapply(Emissions, year, mean, na.rm = T))
	d33 <- data.frame(type="Non Point", year=names(pmBltNp) , Emission=pmBltNp)
	d33$year <- as.Date(as.character(d33$year),"%Y")


	## Baltimore OnRoad data set

	neiBltOr <- neiBLT[neiBLT$type == "ON-ROAD",]
	pmBltOr <- with(neiBltOr, tapply(Emissions, year, mean, na.rm = T))
	d31 <- data.frame(type="On Road", year=names(pmBltOr) , Emission=pmBltOr)
	d31$year <- as.Date(as.character(d31$year),"%Y")
	


	## Baltimore non road data set

	neiBltNr <- neiBLT[neiBLT$type == "NON-ROAD",]
	pmBltNr <- with(neiBltNr, tapply(Emissions, year, mean, na.rm = T))
	d34 <- data.frame(type="Non Road", year=names(pmBltNr) , Emission=pmBltNr)
	d34$year <- as.Date(as.character(d34$year),"%Y")
	

	df1 <- rbind(d31, d32, d33, d34)

	par(mfrow = c(1, 4), bg="gray90")
	ggplot(df1, aes(year, (Emission))) + geom_point() + facet_grid(. ~ type) + geom_smooth(method = 'lm')
}
