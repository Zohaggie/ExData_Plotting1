# Plot4.R - Downloads data zip file if required, unpacks it if required,
# then extracts the required data and create png file of chart 4
# which is a group of 4 graphs

# See readme.md for full explanation

# Prepare the environment and ensure data is avaliable
library(data.table)

# Set up the R error handler to contain a custom message upon a stop
# We will force a stop manually if zip file cannot be found

# set up a condition object for the error handler
condition <- function(subclass, message, call = sys.call(-1), ...) {
    structure(
        class = c(subclass, "condition"),
        list(message = message, call = call, ...)
    )
}

# function to handle the error and pass text to condition object
invalid_item_error <- function(text) {

    msg <- paste0("Error in run_analysis: ", text)

    condition(c("invalid_item_error", "error"),
              message = msg,
              text = text
    )
}

# Relative to current directory, create a data sub-directory (suppress warnings
# in case it already exists)
dir.create("data", showWarnings = FALSE)

# Use zip file if it already exists, either in current directory or in the
# data sub-directory
zipfile <- "exdata_data_household_power_consumption.zip"

if (!file.exists(zipfile)) {
    zipfile <- "./data/exdata_data_household_power_consumption.zip"
}

# obtain the zip file if it doesn't exist in current or data directory
if (!file.exists(zipfile)) {
    # Using http rather than https as file exists from http and therefore an
    # easier task to obtain it, also let download.file pick method with "auto".
    # This works under Windows 8.1, if on Mac or Unix a different technique may
    # need to be used...
    url <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

    download.file(url, destfile = zipfile,
                  method = "auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
}

# force stop and feed error handler if zip file still does not exist
if (!file.exists(zipfile)) {

    stop(
        invalid_item_error(
        "zip file 'exdata_data_household_power_consumption.zip' cannot be located!"
        ))
}

if (!file.exists("data/household_power_consumption.txt")) {
  # unzip the required file
  unzip(zipfile, "household_power_consumption.txt",
        junkpaths = TRUE, exdir = "./data")
}

# To make this as fast as possible, determine the actual rows I need:

# a. Get the row numbers for all dates from 1/2/2007 to 2/2/2007
skiprows = grep("^[12]/2/2007",
                readLines("data/household_power_consumption.txt"))

# b. Because of how I am treating header, set beginning one row back from 1st
# row number of all of the row numbers in skiprows
srow = skiprows[1] - 1
# c. The number of rows is the ending row minus the beginning row number
erow = skiprows[length(skiprows)] - srow

# Fetch the column classes and names via a 5 row dip into the table
chead5rows <- read.table("data/household_power_consumption.txt",
                         header = TRUE,
                         sep = ";",
                         nrows = 5)
# Column Classes
chartclass <- sapply(chead5rows, class)

# Column Names
chartheader <- colnames(chead5rows)

rm (chead5rows)

# Read just the data required, skipping to beginning, fetching just the
# required number of rows, specifying column names and classes
# In this data the NA character is a question mark.
chartdata <- read.table("data/household_power_consumption.txt",
                        header = FALSE,
                        sep = ";", na.strings = "?",
                        col.names = chartheader,
                        colClasses = chartclass,
                        skip = srow, nrows = erow
                        )

# Create DateTime column out of Date and Time columns
chartdata$DateTime <- as.POSIXct(paste(chartdata$Date, chartdata$Time),
                                 format = "%d/%m/%Y %H:%M:%S")

# Set the environment to create 4 graphs in the one chart

# Create png to satisfy requirements of graph 3, which is 3 line charts
# of 3 sub metering values overlaying one another and a legend

png(filename = "plot4.png", width=480, height=480)
par(mfrow = c(2, 2))

with (chartdata, {
      # Simple line plot of Global Active Power for graph 1
	  plot(DateTime, Global_active_power, type="l",
           ylab = "Global Active Power", xlab = "")

      # Simple line plot of Voltage for graph 2
	  plot(DateTime, Voltage, type="l",
           ylab = "Voltage", xlab = "datetime")

      # Overlaid 3 plots of sub metering for graph 3
      plot(DateTime, Sub_metering_1, type="l",
           ylab = "Energy sub metering", xlab = "")
	  par(new = TRUE)
      plot(DateTime, Sub_metering_2, type="l", col="red",
           ylim = range(c(Sub_metering_1, Sub_metering_2)),
           ylab = "", xlab = "")

      par(new = TRUE)
      plot(DateTime, Sub_metering_3, type="l", col="blue",
           ylim = range(c(Sub_metering_1, Sub_metering_3)),
           ylab = "", xlab = "")

      legend(x="topright",
             legend=c("Sub_metering_1",
                      "Sub_metering_2",
                      "Sub_metering_3"),
             fill, lty=1, bty = "n", col=c("black", "red", "blue"))

      # Simple line plot of Global Reactive Power for graph 4
	  plot(DateTime, Global_reactive_power, type="l",
           ylab = "Global_reactive_power", xlab = "datetime")

})

dev.off()

message("Chart saved as Plot4.png, see readme.md for instructions on using it.")
