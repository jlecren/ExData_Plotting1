## loadData: 
## Download the course project data in the folder "./data", 
## unzip the zip file and load the whole data.

loadData <- function () {
  if(!file.exists("./data")) {
    dir.create("./data")
  }
  filepath <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  destzippath <- "./data/household_power_consumption.zip"
  destfilepath <- "./data/household_power_consumption.txt"
  download.file(filepath, destzippath, method = "curl" )
  unzip(destzippath, exdir = "./data")
  house_data <- read.table(destfilepath, header = TRUE, sep = ";", na.strings = "?")
  house_data[, "Date"] <- as.Date(house_data[, "Date"], format = "%d/%m/%Y")
  return(house_data)
}

## subsetHouseData: Get the 2-days subset of the house data
# house_data: the data table contaning all the information

subsetHouseData <- function(house_data) {
  good <- complete.cases(house_data$Date)
  dat <- house_data[good, ][house_data$Date >= "2007-02-01" & house_data$Date <= "2007-02-02", ]
  dat <- within(dat, Time <- strptime(paste(Date, Time, sep=' '), "%Y-%m-%d %H:%M:%S"))
  return(dat)
}

## createPNG: create a PNG file from the given data and plot function
# filename: the output file name
# plotfunc: the function used to plot the data
# dat: the data to plot

createPNG <- function (filename, dat, plotfunc) {
  png(
    filename,
    width     = 480,
    height    = 480,
    units     = "px"
  )
  plotfunc(dat)
  dev.off()
}

## buildPlot4: build plot4 on the screen device
# dat: the data table to build the plot from
buildPlot4 <- function(dat) {
  par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  
  with(dat, {
    plot(Time , Global_active_power, xlab="", ylab = "Global Active Power", type="l")
    plot(Time , Voltage, xlab="datetime", ylab = "Voltage", type="l")
    plot(Time , Sub_metering_1, xlab="", ylab = "Energy sub metering", type="l")
    points(Time, Sub_metering_2, col = "red", type = "l")
    points(Time, Sub_metering_3, col = "blue", type = "l")
    legend("topright", col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1, bty = "n")
    plot(Time , Global_reactive_power, xlab="datetime", ylab = "Global_reactive_power", type="l")
  })
}

## makePlot4: make everything to create the PNG image for plot4

makePlot4 <- function() {
  house_data <- loadData()
  dat <- subsetHouseData(house_data)
  createPNG("plot4.png", dat, buildPlot4)
}