library(dplyr)
library(DBI)
library(dbplyr)
library(ggplot2)
library(lubridate)
library("plotly")

# Read the household power text file
ReadyData_ND <- read.delim(file.choose("C:/Users/Santosh/Desktop/Ubiqum/RStudio/Domfain Research and Exploratory Data Analysis/household_power_consumption.txt"),
                        sep = ";")
glimpse(ReadyData_ND)

# Check for any NA's
anyNA(ReadyData_ND)

# Creating a new attribute for Date and Time
ReadyData_New <- cbind(ReadyData_ND, paste(ReadyData_ND$Date,ReadyData_ND$Time), stringsasFactors=FALSE)
head(ReadyData_New)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(ReadyData_New)[10] <-"DateTime"

## Move the DateTime attribute within the dataset
ReadyData_New <- ReadyData_New[,c(ncol(ReadyData_New), 1:(ncol(ReadyData_New)-1))]
head(ReadyData_New)

## Convert DateTime from POSIXlt to POSIXct 
ReadyData_New$DateTime <- as.POSIXct(ReadyData_New$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(ReadyData_New$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(ReadyData_New)

# Convert all of attributes datatype into Numeric expect Date and time
dat$x = as.numeric(as.character(dat$x))
ReadyData_New$Global_active_power <- as.numeric(as.character(ReadyData_New$Global_active_power))
ReadyData_New$Global_reactive_power <- as.numeric(as.character(ReadyData_New$Global_reactive_power))
ReadyData_New$Voltage <- as.numeric(as.character(ReadyData_New$Voltage))
ReadyData_New$Global_intensity <- as.numeric(as.character(ReadyData_New$Global_intensity))
ReadyData_New$Sub_metering_1 <- as.numeric(as.character(ReadyData_New$Sub_metering_1))
ReadyData_New$Sub_metering_2 <- as.numeric(as.character(ReadyData_New$Sub_metering_2))
ReadyData_New$Sub_metering_3 <- as.numeric(as.character(ReadyData_New$Sub_metering_3))

# Checking for the data type
str(ReadyData_New)

# Creating a new individual parameter for time and date
ReadyData_New$year <- year(ReadyData_New$DateTime)  #year
ReadyData_New$quarter <- quarter(ReadyData_New$DateTime) #quarter
ReadyData_New$month <- month(ReadyData_New$DateTime) #month
ReadyData_New$week <- week(ReadyData_New$DateTime) #week
ReadyData_New$day <- day(ReadyData_New$DateTime) #day
ReadyData_New$hour <- hour(ReadyData_New$DateTime) #hour
ReadyData_New$minute <- minute(ReadyData_New$DateTime) #minute
ReadyData_New$weekday = weekdays(ReadyData_New$DateTime)
str(ReadyData_New)

## Plot all of sub-meter 1
plot(ReadyData_New$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(ReadyData_New, year == 2008 & week == 2)

## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(ReadyData_New, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))







