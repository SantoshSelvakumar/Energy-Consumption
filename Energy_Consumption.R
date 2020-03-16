# Time series: energy consumption

# Loading libraries -------------------------------------------------------
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(RMySQL)
library(lubridate)
library(plotly)
library(TTR)
library(forecast)
library(tibble)

# Group by month and year -------------------------------------------------

# Subsetting by using mean of the month instead of individual observations (minute)
mydata_my <- mydata_new %>% 
  select(year, month, Global_active_power, Global_reactive_power, Voltage, Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3, GAP, NotincludedAP) %>% 
  filter(year == 2006 | year == 2007 | year == 2008 | year == 2009) %>% 
  group_by(year, month) %>% 
  summarise(Mean_globalactive = mean(Global_active_power, na.rm = TRUE),
            Mean_grp = mean(Global_reactive_power, na.rm = TRUE),
            Mean_volt = mean(Voltage, na.rm = TRUE),
            Mean_gi = mean(Global_intensity, na.rm = TRUE),
            Mean_sub1 = mean(Sub_metering_1, na.rm = TRUE),
            Mean_sub2 = mean(Sub_metering_2, na.rm = TRUE),
            Mean_sub3 = mean(Sub_metering_3, na.rm = TRUE),
            Mean_GAP = mean(GAP, na.rm = TRUE),
            Mean_notincl = mean(NotincludedAP, na.rm = TRUE)
  )

mydata_new %>% 
  filter(year == 2010) %>% 
  group_by(year, month) %>% 
  summarise(Mean_GAP = mean(GAP, na.rm = TRUE))

mydata_my %>%
  plot_ly(x = ~mydata_my$month, y = ~mydata_my$Mean_globalactive, name = 'Mean Global Active Power per month', type = 'scatter', mode = 'lines')

# Convert to timeseries, frequency = 12
# Globalactive_ts <- ts(mydata_my$Mean_globalactive, frequency = 12, start = c(2006,12), end = c(2009,12))
# GRP_ts <- ts(mydata_my$Mean_grp, frequency = 12, start = c(2006,12), end = c(2009,12))
# MV_ts <- ts(mydata_my$Mean_volt, frequency = 12, start = c(2006,12), end = c(2009,12))
# GI_ts <- ts(mydata_my$Mean_gi, frequency = 12, start = c(2006,12), end = c(2009,12))
Sub1_ts <- ts(mydata_my$Mean_sub1, frequency = 12, start = c(2006,12), end = c(2009,12))
Sub2_ts <- ts(mydata_my$Mean_sub2, frequency = 12, start = c(2006,12), end = c(2009,12))
Sub3_ts <- ts(mydata_my$Mean_sub3, frequency = 12, start = c(2006,12), end = c(2009,12))
GAP_ts <- ts(mydata_my$Mean_GAP, frequency = 12, start = c(2006,12), end = c(2009,12))
Notincl_ts <- ts(mydata_my$Mean_notincl, frequency = 12, start = c(2006,12), end = c(2009,12))

# Plot time series
# autoplot(Globalactive_ts)
# autoplot(GRP_ts)
# autoplot(MV_ts)
# autoplot(GI_ts)
autoplot(Sub1_ts)
autoplot(Sub2_ts)
autoplot(Sub3_ts)
autoplot(GAP_ts)
autoplot(Notincl_ts)

# Linear regression tslm --------------------------------------------------
## Apply time series linear regression 
# fitGlobalactive_ts <- tslm(Globalactive_ts ~ trend + season) 
# fitGRP_ts <- tslm(GRP_ts ~ trend + season) 
# fitMV_ts <- tslm(MV_ts ~ trend + season) 
# fitGI_ts <- tslm(GI_ts ~ trend + season) 
fitSub1_ts <- tslm(Sub1_ts ~ trend + season) 
fitSub2_ts <- tslm(Sub2_ts ~ trend + season) 
fitSub3_ts <- tslm(Sub3_ts ~ trend + season) 
fitGAP_ts <- tslm(GAP_ts ~ trend + season) 
fitNotincl_ts <- tslm(Notincl_ts ~ trend + season) 

#summary
# summary(fitGlobalactive_ts)
# summary(fitGRP_ts)
# summary(fitMV_ts)
# summary(fitGI_ts)
summary(fitSub1_ts)
summary(fitSub2_ts)
summary(fitSub3_ts)
summary(fitGAP_ts)
summary(fitNotincl_ts)

## Create the forecast for mean GAP. Forecast ahead 22 time periods 
# forecastGlobalactive_ts <- forecast(fitGlobalactive_ts, h=12, level = c(80,95))
# forecastGRP_ts <- forecast(fitGRP_ts, h=12, level = c(80,95))
# forecastMV_ts <- forecast(fitMV_ts, h=12, level = c(80,95))
# forecastGI_ts <- forecast(fitGI_ts, h=12, level = c(80,95))
forecastSub1_ts <- forecast(fitSub1_ts, h=12, level = c(80,95))
forecastSub2_ts <- forecast(fitSub2_ts, h=12, level = c(80,95))
forecastSub3_ts <- forecast(fitSub3_ts, h=12, level = c(80,95))
forecastGAP_ts <- forecast(fitGAP_ts, h=12, level = c(80,95))
forecastNotincl_ts <- forecast(fitNotincl_ts, h=12, level = c(80,95))

## Plot the forecast for mean GAP. 
# plot(forecastGlobalactive_ts, ylim = c(0,2), ylab="kilowatt", xlab="Years")
# plot(forecastGRP_ts, ylim = c(0,0.3), ylab="kilowatt", xlab="Years")
# plot(forecastMV_ts, ylim = c(235,250), ylab="volt", xlab="Years")
# plot(forecastGI_ts, ylim = c(0,10), ylab="ampere", xlab="Years")
plot(forecastSub1_ts, ylim = c(0,2), ylab="Watt-Hours", xlab="Years")
plot(forecastSub2_ts, ylim = c(-0.5,2.5), ylab="Watt-Hours", xlab="Years")
plot(forecastSub3_ts, ylim = c(1,12), ylab="Watt-Hours", xlab="Years")
plot(forecastGAP_ts, ylim = c(5,35), ylab="Watt-Hours", xlab="Years")
plot(forecastNotincl_ts, ylim = c(-2,25), ylab="Watt-Hours", xlab="Years")

# Holt Winters ------------------------------------------------------------
## Apply time series Holt Winters 
# fitGlobalactive_hs <- HoltWinters(Globalactive_ts, beta=TRUE, gamma=TRUE)
# fitGRP_hs <- HoltWinters(GRP_ts, beta=TRUE, gamma=TRUE)
# fitMV_hs <- HoltWinters(MV_ts, beta=TRUE, gamma=TRUE)
# fitGI_hs <- HoltWinters(GI_ts, beta=TRUE, gamma=TRUE)
fitSub1_hs <- HoltWinters(Sub1_ts, beta=TRUE, gamma=TRUE)
fitSub2_hs <- HoltWinters(Sub2_ts, beta=TRUE, gamma=TRUE)
fitSub3_hs <- HoltWinters(Sub3_ts, beta=TRUE, gamma=TRUE)
fitGAP_hs <- HoltWinters(GAP_ts, beta=TRUE, gamma=TRUE)
fitNotincl_hs <- HoltWinters(Notincl_ts, beta=TRUE, gamma=TRUE)

## Create the forecast for mean GAP. Forecast ahead 22 time periods 
# forecastGlobalactive_hs <- forecast(fitGlobalactive_hs, h=12, level = c(80,95))
# forecastGRP_hs <- forecast(fitGRP_hs, h=12, level = c(80,95))
# forecastMV_hs <- forecast(fitMV_hs, h=12, level = c(80,95))
# forecastGI_hs <- forecast(fitGI_hs, h=12, level = c(80,95))
forecastSub1_hs <- forecast(fitSub1_hs, h=12, level = c(80,95))
forecastSub2_hs <- forecast(fitSub2_hs, h=12, level = c(80,95))
forecastSub3_hs <- forecast(fitSub3_hs, h=12, level = c(80,95))
forecastGAP_hs <- forecast(fitGAP_hs, h=12, level = c(80,95))
forecastNotincl_hs <- forecast(fitNotincl_hs, h=12, level = c(80,95))

## Plot the forecast for mean GAP. 
# plot(forecastGlobalactive_hs, ylim = c(0,2), ylab="kilowatt", xlab="Years")
# plot(forecastGRP_hs, ylim = c(0,0.50), ylab="kilowatt", xlab="Years")
# plot(forecastMV_hs, ylim = c(0,248), ylab="volt", xlab="Years")
# plot(forecastGI_hs, ylim = c(0,10), ylab="ampere", xlab="Years")
plot(forecastSub1_hs, ylim = c(-0.5,2.5), ylab="Watt-Hours", xlab="Years")
plot(forecastSub2_hs, ylim = c(0,3), ylab="Watt-Hours", xlab="Years")
plot(forecastSub3_hs, ylim = c(1,18), ylab="Watt-Hours", xlab="Years")
plot(forecastGAP_hs, ylim = c(5,35), ylab="Watt-Hours", xlab="Years")
plot(forecastNotincl_hs, ylim = c(0,25), ylab="Watt-Hours", xlab="Years")

# Total energy costs per year
mydata_total <- mydata_new %>%
  as_tibble() %>% 
  filter(year == 2007 | year == 2008 | year == 2009) %>% 
  group_by(year,month) %>%
  summarise(totalcost_gap = sum(GAP) * 0.13/1000)  #GAP is in watthour per minute, so divide by 1000 then you have kwh per min

tc_ts <- ts(mydata_total$totalcost_gap, frequency = 12, start = c(2007,1), end = c(2009,12))
autoplot(tc_ts)

#linear regression
fittc_ts <- tslm(tc_ts ~ trend + season) 
summary(fittc_ts)
forecasttc_ts <- forecast(fittc_ts, h=12, level = c(80,95))
plot(forecasttc_ts, ylim = c(20,170), ylab="Energy costs in Euro per month", xlab="Years")

#holt winters
component_fittc_ts <- decompose(tc_ts)
plot(component_fittc_ts)
summary(component_fittc_ts$seasonal)
summary(component_fittc_ts$trend)
summary(component_fittc_ts$random)
## Seasonal adjusting total costs by subtracting the seasonal component & plot
tc_ts_adjusted <- tc_ts - component_fittc_ts$seasonal
autoplot(tc_ts_adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tc_ts_adjusted))

## Holt Winters Exponential Smoothing & Plot
fittc_ts_hw <- HoltWinters(tc_ts_adjusted, beta=FALSE, gamma=FALSE)
tc_ts_hw_forecast <- forecast(fittc_ts_hw, h=12)
plot(tc_ts_hw_forecast, ylim = c(60, 150), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

fittc_ts_hw2 <- HoltWinters(tc_ts_adjusted, beta=TRUE, gamma=FALSE)
tc_ts_hw2_forecast <- forecast(fittc_ts_hw2, h=12)
plot(tc_ts_hw2_forecast, ylim = c(0, 200), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

fittc_ts_hw3 <- HoltWinters(tc_ts_adjusted, beta=TRUE, gamma=TRUE)
tc_ts_hw3_forecast <- forecast(fittc_ts_hw3, h=12)
plot(tc_ts_hw3_forecast, ylim = c(60, 150), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")