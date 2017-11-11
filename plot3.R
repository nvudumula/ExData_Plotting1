
## Read data from input file and store it into Data Frame
## Write the data frame to an output file for future use
## Once output file is read, it signifiacntly improves the performance

## Please note input file is sorted on the date, so we only need to read until the desired dates and leave out the records after that

readData <- function() {
	i <- 2;
	j <- 1;
	options(stringsAsFactors = FALSE)
	df <- data.frame()
	
	f <- fread("./data/household_power_consumption.txt", sep= "\n", header = TRUE)
	rows <- nrow(f);
	colNamesList <- strsplit (colnames (f),";")
	colNames <- c(colNamesList[[1]])
	print (colNames)
	while ( i <= rows ) {
		recList <- strsplit(as.character(f[i,1]),";")
		rec <- c(recList[[1]])

		if ( (as.Date(rec[[1]][1],"%d/%m/%Y")) >= as.Date("02/01/2007","%m/%d/%Y") & (as.Date(rec[[1]][1],"%d/%m/%Y")) <= as.Date("02/02/2007","%m/%d/%Y")) {
			
			print (rec)
			df <- rbind(df,rec)	
			j <- j + 1
		}
		if ( (as.Date(rec[[1]][1],"%d/%m/%Y")) > as.Date("03/01/2007","%m/%d/%Y") ) {
			break
		}
		i <- i+1;
	}
	names(df)[] <- colNames
	print(nrow(df))
	write.table(df, file = "./data/output.csv",row.names=FALSE, na="", sep=",")
	df
}

## Function to draw the graph(s)

drawGraphPlot3 <- function() {
		
	## if the data for the desired dates exists in the output file then take it from output file
	## Otherwise, read the data from original input file

	if(file.exists("./data/output.csv")){
		df <- read.csv("./data/output.csv")
	}
	else {
		df <- readData()
	}
		
	## Add the date time field to use in graphs

	df$DT1 <-  strptime(paste (df$Date,df$Time), format="%d/%m/%Y %H:%M:%S")
	
	## Draw charts

	plot(df$DT1,df$Sub_metering_1, type="n",xlab="",ylab="Energy sub metering")
	lines(df$DT1, df$Sub_metering_1)
	lines(df$DT1, df$Sub_metering_2,col="red")
	lines(df$DT1, df$Sub_metering_3,col="blue")
	legend("topright", lwd = 1, col = c("black","blue", "red"), legend = c("sub_metering_1", "sub_metering_2","sub_metering_3"))
}

