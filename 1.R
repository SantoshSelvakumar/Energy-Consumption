# Install the RMySQL package
#install.packages(c("RMySQL","dplyr","DBI","dbplyr","ggplot2","modelDB","tidypredict"))
library(RMySQL)
library(dplyr)
library(DBI)
library(dbplyr)
library(ggplot2)
library(lubridate)

# Step #1 
## Plot all of sub-meter 1
# plot(combinedDF_All$Sub_metering_1)
# 
# #-Subset data by quarter and summarise total usage across the 3 submeters
# combinedDF_qtr <- combinedDF_RN %>%
#   group_by(year(DateTime), quarter(DateTime)) %>%
#   summarise(Kitchen=round(sum(`Kitchen`/1000), 3),
#             Laundry_room=round(sum(`Laundry_room`/1000), 3),
#             Water_heater_AC=round(sum(`Water_heater_AC`/1000), 3))
# 
# #-Look at top several rows of new quarterly data set 
# head(combinedDF_qtr)
# 
# # Renaming the attributes
# combinedDF_qtr$year <- combinedDF_qtr$`year(DateTime)`
# combinedDF_qtr$quarter <- combinedDF_qtr$`quarter(DateTime)`
# 
# head(combinedDF_qtr)
# ## Subset the second week of 2008 - All Observations
# houseWeek <- filter(combinedDF_qtr, year == 2008)
# 
# ## Plot subset houseWeek
# plot(houseWeek$Sub_metering_1)

combinedDF_RN$year <- year(combinedDF_RN$DateTime)  #year
combinedDF_RN$quarter <- quarter(combinedDF_RN$DateTime) #quarter
combinedDF_RN$month <- month(combinedDF_RN$DateTime) #month
combinedDF_RN$week <- week(combinedDF_RN$DateTime) #week
combinedDF_RN$day <- day(combinedDF_RN$DateTime) #day
combinedDF_RN$hour <- hour(combinedDF_RN$DateTime) #hour
combinedDF_RN$minute <- minute(combinedDF_RN$DateTime) #minute
combinedDF_RN$weekday = weekdays(combinedDF_RN$DateTime)

str(combinedDF_RN)
head(combinedDF_RN)
tail(combinedDF_RN)
summary(combinedDF_RN)

# Visualize the Data 
## Plot all of sub-meter 1
plot(combinedDF_RN$Kitchen)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(combinedDF_RN, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Kitchen)

## Subset the 9th day of January 2008 - All observations
# houseDay <- filter(yourData, year == 2008 & month == 1 & day == 9)
houseDay <- filter(combinedDF_RN, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Kitchen, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Laundry_room, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Water_heater_A, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(combinedDF_RN, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Laundry_room, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Water_heater_AC, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Create a visualization with plotly for a Week of your choosing.
# Use all three sub-meters and make sure to label. Experiment with granularity. 

## Subset the sixteenth week of 2010 - All Observations and for Kitchen attribute
houseWeek_16Kitchen <- filter(combinedDF_RN, year == 2010 & week == 16)
## Plot subset houseWeek
plot(houseWeek_16$Kitchen)

## Subset the sixteenth week of 2010 - All Observations and for 
houseWeek_16Laundry <- filter(combinedDF_RN, year == 2010 & week == 16)
## Plot subset houseWeek
plot(houseWeek_16Laundry$Laundry_room)

## Subset the sixteenth week of 2010 - All Observations and for 
houseWeek_16Waterheater <- filter(combinedDF_RN, year == 2010 & week == 16)
## Plot subset houseWeek
plot(houseWeek_16Laundry$Water_heater_AC)

# Create a visualization for a time period of your choice. Both "Day" and "Week" highlight typical patterns in a home. 
# What might be another period of time that could provide insights? 
# Use plotly and experiment with granularity until you find the visualization that maximizes information gain for the viewer. 

house_Day_Week <- filter(combinedDF_RN, year == 2010 & week == 16 & day == 7)
## Plot subset houseWeek
plot(house_Day_Week)

g <- ggplot(combinedDF_RN, aes(x = Kitchen, y = year) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon") + 
      xlim(1, 6) + ylim(40, 100))
ggplotly(g)

# Prepare to analyze the data -------------------------------------------------------------------------------------------
combined_0309 <- combinedDF_RN

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(combined_0309, weekday == "Monday" & hour == 20 & minute == 1)
View(house070809weekly)

## Create TS object with SubMeter3 (Water Heater and AC)
tsSM3_070809weekly <- ts(house070809weekly$Water_heater_AC, frequency=52, start=c(2007,1))
View(tsSM3_070809weekly)

## Plot sub-meter 3(Water_heater_AC) with autoplot (you may need to install these packages)
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3(Water_heater_AC) with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Water Heater and AC")

## Plot sub-meter 3 (Water_heater_AC) with plot.ts
plot.ts(tsSM3_070809weekly)

# Sub-meter 1 (Kitchen)with your choice of frequency and time period
## Subset to one observation per week on Tuesday at 7:00pm for 2007, 2008 and 2009
combined_0309 <- combinedDF_RN

house070809weekly_KIT <- filter(combined_0309, weekday == "Wednesday" & hour == 19 & minute == 1)
View(house070809weekly_KIT)

## Create TS object with SubMeter1 (Kitchen)
tsSM3_070809weekly_KIT <- ts(house070809weekly_KIT$Kitchen, frequency=52, start=c(2007,1))
View(tsSM3_070809weekly_KIT)

## Plot sub-meter 1(Kitchen) with autoplot (you may need to install these packages)
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809weekly_KIT)

## Plot sub-meter 1(Kitchen) with autoplot - add labels, color
autoplot(tsSM3_070809weekly_KIT, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Kitchen")

# Sub-meter 2 (Laundry_room)with your choice of frequency and time period
## Subset to one observation per week on Tuesday at 7:00pm for 2007, 2008 and 2009
combined_0309 <- combinedDF_RN

house070809weekly_LR <- filter(combined_0309, weekday == "Saturday" & hour == 19 & minute == 1)
View(house070809weekly_LR)

## Create TS object with SubMeter2 (Water Heater and AC)
tsSM3_070809weekly_LR <- ts(house070809weekly_LR$Laundry_room, frequency=52, start=c(2007,1))
View(tsSM3_070809weekly_LR)

## Plot sub-meter 2(Water_heater_AC) with autoplot (you may need to install these packages)
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809weekly_LR)

## Plot sub-meter 2(Water_heater_AC) with autoplot - add labels, color
autoplot(tsSM3_070809weekly_LR, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Laundry Room")

#--------------- PENDING -----------------
# Create a visualization for a time period of your choice. Both "Day" and "Week" highlight typical patterns in a home. 
# What might be another period of time that could provide insights? 
# Use plotly and experiment with granularity until you find the visualization that maximizes information gain for the viewer. 

## Subset the 13th Dec of 2009 - All observations
houseDay_Dec13 <- filter(combined_0309, year == 2009 & month == 12 & day == 13) && filter(combined_0309, year == 2009 & month == 12 & day == 13)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')


# Optional Work 
# 
# Produce pie chart visualizations that are likely to provide insight, e.g.,
# Percentage of total use at various times of day by each sub-meter.
# Percentage of total power use over a day by each sub-meter.
# Percentage of total power use over an entire year by each sub-meter.
# Produce any other visualizations that you believe may provide insight.

# Forecasting a time series ----------------------------------------------------------------------------------------------
## Apply time series linear regression to the sub-meter 3 ts object (Water Heater and AC)
# and use summary to obtain R2 and RMSE from the model you built
library(forecast)
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3 (Water Heater and AC). 
# Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

# # Submeter 2 - Laundry Room
library(forecast)
fitSM5 <- tslm(tsSM3_070809weekly_LR ~ trend + season) 
summary(fitSM5)

## Create the forecast for sub-meter 2 (Water Heater and AC). 
# Forecast ahead 20 time periods 
forecastfitSM5 <- forecast(fitSM5, h=20)
## Plot the forecast for sub-meter 2. 
plot(forecastfitSM5)

## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM5c <- forecast(fitSM5, h=20, level=c(80,90))

## Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM5c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM5c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM5c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

# # Submeter 1 - Kitchen
library(forecast)
fitSM4 <- tslm(tsSM3_070809weekly_KIT ~ trend + season) 
summary(fitSM4)

## Create the forecast for sub-meter 1 (Water Heater and AC). 
# Forecast ahead 20 time periods 
forecastfitSM4 <- forecast(fitSM4, h=20)
## Plot the forecast for sub-meter 1. 
plot(forecastfitSM4)

## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM4c <- forecast(fitSM4, h=20, level=c(80,90))

## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfitSM4c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM4c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfitSM4c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

#--------------- PENDING -----------------One comparison chart showing the R2 and RMSE of each model you built


# Decomposing a Seasonal Time Series-----------------------------------------------------------------------------------------
## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

# # Submeter 2 - Laundry Room
## Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM3weekly_LR <- decompose(tsSM3_070809weekly_LR)
## Plot decomposed sub-meter 2 
plot(components070809SM3weekly_LR)
## Check summary statistics for decomposed sub-meter 2 
summary(components070809SM3weekly_LR)

# # Submeter 1 - Kitchen
## Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM3weekly_KIT <- decompose(tsSM3_070809weekly_KIT)
## Plot decomposed sub-meter 1 
plot(components070809SM3weekly_KIT)
## Check summary statistics for decomposed sub-meter 1
summary(components070809SM3weekly_KIT)

# 5. Holt-Winters Forecasting----------------------------------------------------------------------
## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

# Submetering 2 - Laundry Room
## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted_LR <- tsSM3_070809weekly_LR - components070809SM3weekly_LR$random
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted_LR))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809_LR <- HoltWinters(tsSM3_070809Adjusted_LR, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809_LR, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for_LR <- forecast(tsSM3_HW070809_LR, h=25)
plot(tsSM3_HW070809for_LR, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC_LR <- forecast(tsSM3_HW070809_LR, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC_LR, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

# Submetering 1 - Kitchen
## Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM3_070809Adjusted_KIT <- tsSM3_070809weekly_KIT - components070809SM3weekly_KIT$figure
autoplot(tsSM3_070809Adjusted_KIT)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted_KIT))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809_KIT <- HoltWinters(tsSM3_070809Adjusted_KIT, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809_KIT, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for_KIT <- forecast(tsSM3_HW070809_KIT, h=25)
plot(tsSM3_HW070809for_KIT, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC_KIT <- forecast(tsSM3_HW070809_KIT, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC_KIT, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2009))












