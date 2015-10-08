# plot2.R
#
# Exploratory Data Analysis Course Project 1, Plot 2.
#
# 1. Reads data from the Electric power consumption dataset, treating "?" as NA.
# 2. Filters the data to select only the dates 2007-02-01 and 2007-02-02.
# 3. Constructs the second course-project plot.
# 4. Saves it to the PNG file "plot2.png" with a width of 480 pixels and a
#    height of 480 pixels.
#
#-------------------------------------------------------------------------------
#
# main() -- The main routine that creates plot2.png
#
# ------------------------------------------------------------------------------
main <- function() {
    df <- getFilteredData() #Read and filter the data
    # todo: construct and save plot


    png("plot2.png")
    plot(df$DateTime, df$Global_active_power,
                  type = "l",
                  xlab = "",
                  ylab = "Global Active Power (kilowatts)")
    dev.off()
    return(df)
}

#-------------------------------------------------------------------------------
#
# getFilteredData() -- Reads and filters the data
#
#-------------------------------------------------------------------------------
getFilteredData <- function() {
    filteredFile <- "filtered9.txt"
    if (file.exists(filteredFile)) {
        # Found a cached version of the filtered data
        df <- read.table(filteredFile, header = TRUE, sep = ",")
        df <- transform(df, DateTime = as.POSIXct(DateTime))
    }
    else {
        filename <- "household_power_consumption.txt"
        df <- read.csv2(filename, na.strings = "?", stringsAsFactors = FALSE)
        df$Date <- as.Date(strptime(df$Date, "%d/%m/%Y"))
        startDate <- as.Date("2007-02-01", "%Y-%m-%d")
        endDate <- as.Date("2007-02-02", "%Y-%m-%d")
        lv <- (df$Date >= startDate) & (df$Date <= endDate)
        df <- df[lv, ]
        df$DateTime <- as.POSIXct(paste(df$Date, df$Time), tz = "UTC")
        df <- transform(df,
                  Global_active_power = as.numeric(Global_active_power),
                  Global_reactive_power = as.numeric(Global_reactive_power),
                  Voltage = as.numeric(Voltage),
                  Global_intensity = as.numeric(Global_intensity),
                  Sub_metering_1 = as.numeric(Sub_metering_1),
                  Sub_metering_2 = as.numeric(Sub_metering_2),
                  Sub_metering_3 = as.numeric(Sub_metering_3) )
        df$Date <- NULL
        df$Time <- NULL

        row.names(df) <- 1:nrow(df)

        # Save a cached version of the file
        write.table(df, file = filteredFile, sep = ",")
    }
    return(df)
}
