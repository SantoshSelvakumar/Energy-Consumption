library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(tibble)
library(plotly)

missing_data <- read_csv2("C:\Users\Santosh\Desktop\Ubiqum\RStudio\Missing_Domain_Research_Exploratory_Data_Analysis\household_power_consumption.txt")
glimpse(missing_data)

# convert data type to numeric (character > numeric)
missing_data$Global_active_power <- as.numeric(missing_data$Global_active_power)
missing_data$Global_reactive_power <- as.numeric(missing_data$Global_reactive_power)
missing_data$Global_intensity <- as.numeric(missing_data$Global_intensity)
missing_data$Sub_metering_1 <- as.numeric(missing_data$Sub_metering_1)
missing_data$Sub_metering_2 <- as.numeric(missing_data$Sub_metering_2)
missing_data$Sub_metering_3 <- as.numeric(missing_data$Sub_metering_3)

# re-order date: d/m/Y to Y/m/d
missing_data$Date <- as.Date(missing_data$Date,format='%d/%m/%Y')

# Combine Date and Time attribute values in a new attribute column
missing_data <- cbind(missing_data, paste(missing_data$Date,missing_data$Time), stringsAsFactors=FALSE)

# Give the new attribute in the 10th column a header name 
colnames(missing_data)[10] <-"DateTime"

# Move the DateTime attribute within the dataset
missing_data <- missing_data[,c(ncol(missing_data), 1:(ncol(missing_data)-1))]

# Convert DateTime from POSIXlt to POSIXct 
missing_data$DateTime <- as.POSIXct(missing_data$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
attr(missing_data$DateTime, "tzone") <- "GMT"

# Create "year/month/week/day/weekdays/hour" attributes with lubridate 
missing_data$year <- year(missing_data$DateTime)
missing_data$month <- month(missing_data$DateTime)
missing_data$week <- week(missing_data$DateTime)
missing_data$day <- day(missing_data$DateTime)
missing_data$wday <- wday(missing_data$DateTime)
missing_data$weekdays <- weekdays(missing_data$DateTime)
missing_data$hour <- hour(missing_data$DateTime)
missing_data$minute <- minute(missing_data$DateTime)
missing_data$quarter <- quarter(missing_data$DateTime)
missing_data$Date <- NULL
missing_data$Time <- NULL

missing_data_new <- missing_data %>% 
  mutate(Sub_metering_total = Sub_metering_1 + Sub_metering_2 + Sub_metering_3) %>% 
  mutate(GAP = Global_active_power * 1000/60) %>% 
  mutate(NotincludedAP = (Global_active_power * 1000/60) - (Sub_metering_1 + Sub_metering_2 + Sub_metering_3))

glimpse(missing_data)

summary(missing_data)

missing_data_replace_NA <- function(col) {
  for (row in 10081:length(col)) { 
    if(is.na(col[row]) & !is.na(col[row+1])) {
      col[row] <- ((col[row-1])+(col[row+1]))/2
    } else if(is.na(col[row])) {
      col[row] <- mean(col[(row-10080):(row-1)])}
  }
  return(col)
}

#define columns to apply the function just once
relevant_columns2 <- c("Global_active_power","Global_reactive_power","Voltage","Global_intensity","GAP","Sub_metering_1","Sub_metering_2","Sub_metering_3", "Sub_metering_total", "NotincludedAP")
# apply the function to the columns
missing_data_new[relevant_columns2] <- lapply(missing_data_new[relevant_columns2], function(x) missing_data_replace_NA(col = x))




