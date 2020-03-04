# Libraries
library(RMySQL)
library(dplyr)
library(DBI)
library(dbplyr)
library(ggplot2)
library(modelDB)
library(tidypredict)
library(caret)
library(lattice)
library(ggplot2)

# DataSet: combination of all the year Dataset ---------------------------------------------------------------------------------------------
# yourdataframe <-cbind(yourdataframe,paste(yourdataframe$Date,yourdataframe$Time), stringsAsFactors=FALSE)

combinedDF_All <- cbind(combinedDF, paste(combinedDF$Date,combinedDF$Time), stringsasFactors=FALSE)
combinedDF_All
head(combinedDF_All)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(combinedDF_All)[11] <-"DateTime"

## Move the DateTime attribute within the dataset
combinedDF_All <- combinedDF_All[,c(ncol(combinedDF_All), 1:(ncol(combinedDF_All)-1))]
head(combinedDF_All)

## Convert DateTime from POSIXlt to POSIXct 
combinedDF_All$DateTime <- as.POSIXct(combinedDF_All$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone- CET
attr(combinedDF_All$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(combinedDF_All)

# Remove the "stringsasFactors" column in DS
combinedDF_All[1] <- NULL
head(combinedDF_All)
glimpse(combinedDF_All)

# Exclude rows with NA values in data set: combinedDF_All
combinedDF_All[complete.cases(combinedDF_All),]

# Renaming the heading as Kitchen = Sub_metering_1,Laundry_room = Sub_metering_2, Water_heater_AC = Sub_metering_3
combinedDF_RN <- rename(combinedDF_All, Kitchen = Sub_metering_1,
                  Laundry_room = Sub_metering_2, Water_heater_AC = Sub_metering_3)
head(combinedDF_RN)

library(ggplot2)

# Plotting for Global_active_power
hist(combinedDF_RN$Global_active_power, col = "red",
     xlab = "Global Active Power (kilowatts)", main = "Global Active Power")

GAP_1 <- (ggplot(combinedDF_RN, aes(x = Global_active_power))
       + geom_histogram(fill = "#F8766D")
       + labs(x = "Global Active Power (kilowatts)", title = "Global Active Power"))
GAP_1

GAP_2 <- (ggplot(combinedDF_RN, aes(x = DateTime, y = Global_active_power))
       + geom_line() + xlab("") + ylab("Global Active Power (kilowatts)"))
GAP_2

# Plotting for Global_intensity
GI_1 <- (ggplot(combinedDF_RN, aes(x = Global_intensity))
          + geom_histogram(fill = "#F8766D")
          + labs(x = "Global Intensity (kilowatts)", title = "Global Intensity"))
GAP_1

GI_2 <- (ggplot(combinedDF_RN, aes(x = DateTime, y = Global_intensity))
         + geom_line() + xlab("") + ylab("Global Intensity (kilowatts)"))
GI_2

# Plotting for Voltage
Vol_1 <- (ggplot(combinedDF_RN, aes(x = Voltage))
         + geom_histogram(fill = "#F8766D")
         + labs(x = "Voltage (Volts)", title = "Voltage"))
Vol_1

Vol_2 <- (ggplot(combinedDF_RN, aes(x = DateTime, y = Voltage))
         + geom_line() + xlab("") + ylab("Voltage (Volts)"))
Vol_2

# Plotting for Voltage
plot(combinedDF_RN$DateTime, combinedDF_RN$Kitchen, type = "l", ylab = "Energy sub metering", xlab = "")
points(combinedDF_RN$DateTime, combinedDF_RN$Laundry_room, type = "l", col = "red")
points(combinedDF_RN$DateTime, combinedDF_RN$Water_heater_AC, type = "l", col = "blue")
legend("topright", lty = c(1,1,1), lwd = c(2,2,2), col = c("black", "red", "blue"),
legend = c("Kitchen", "Laundry_room", "Water_heater_AC"))

#  Total consumption by Submeters


ggplot() +
  geom_col(data=combinedDF_RN,
           aes(x=Year, y=Consump, fill=Submeter),
           position="dodge") +
  labs(x="Year", y="Consumption Watt/Hour", title="Total Consumption by Sub-meters") +
  scale_y_continuous(labels=scales::comma) +
  theme_linedraw(base_size = 11, base_family = "") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) 

Consumption <- Consumption %>% 
  mutate(DateTime = ifelse(between(DateTime, as_datetime("2007-03-25 02:00:00"),
                                   as_datetime("2007-10-28 01:59:00")),
                           (DateTime + 3600) , (DateTime))) 
ggplot() +
  geom_col(data=anual.week,
           aes(x=week_days, y=Mean_GC, fill=Month),
           position= "dodge") +
  labs(x="Week Days", y="Average Global Consumption", title="Average Consumption by Weekdays ") +
  theme_linedraw(base_size = 11, base_family = "") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

library(dplyr)

# Total energy consumed by year
combinedDF_RN %>% 
  as_tibble() %>% 
  mutate(year = year(DateTime)) %>% 
  select(year, DateTime, Global_active_power) %>% 
  group_by(year) %>% 
  summarise(total_GAP = sum(Global_active_power)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=total_GAP)) +
  geom_col(fill='dodgerblue4') +
  theme_bw()

# Find the max value of attribute Kitchen (Kwh)
# Find the min value of attribute Kitchen (Kwh)
# Find the max value of attribute Laundry_room (Kwh)
# Find the min value of attribute Laundry_room (Kwh)
# Find the max value of attribute Water_heater_AC (Kwh)
# Find the min value of attribute Water_heater_AC (Kwh)
# Find the Mean, Mode, SD, quartiles and charactization of distribution

combinedDF_RN %>% 
  as_tibble()  %>% 
  lapply(data, class)
  

















