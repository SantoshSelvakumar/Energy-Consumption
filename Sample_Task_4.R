library(dplyr)
library(tidyr)
library(Hmisc)
library(knitr)
library(lubridate)
library(ggplot2)
library(forecast)
library(plotrix)

data <- read.csv("C:/Users/Santosh/Desktop/household_power_consumption.txt", sep=";",stringsAsFactors = F)
#data <- as.data.frame(data)
# changing the data type

cols <- c(3,4,5,6,7,8)
data[,cols] <- lapply(data[,cols],function(x) as.numeric(x))

View(data)

#Only taking complete cases
datac <- data[complete.cases(data),]
str(datac)

#changing column name
colnames(datac)[7] <- "kitchen"
colnames(datac)[8] <- "laundry"
colnames(datac)[9] <- "AC"

names(datac)

datac <-cbind(datac,paste(datac$Date,datac$Time), stringsAsFactors=FALSE)
colnames(datac)[10] <-"DateTime"
datac <- datac[,c(ncol(datac), 1:(ncol(datac)-1))]
head(datac)

datac$DateTime <- strptime(datac$DateTime, "%d/%m/%Y %H:%M:%S")
datac$Date <- as.Date(datac$Date, "%d/%m/%Y")
str(datac)

#observations with missing values
obm <- 2075259-2049280;kable(obm)

#Describing the shape of each varibale
#describe(datac[,-c(1,2)])


# datac <-cbind(datac,paste(datac$Date,datac$Time), stringsAsFactors=FALSE)
# colnames(datac)[10] <-"DateTime"
# datac <- datac[,c(ncol(datac), 1:(ncol(datac)-1))]
# head(datac)
# 
# datac$DateTime <- strptime(datac$DateTime, "%d/%m/%Y %H:%M:%S")
# datac$Date <- as.Date(datac$Date, "%d/%m/%Y")
# str(datac)



# tail(datac)
# 
# 
# datac$Date <- as.Date(datac$Date,format = "%d/%m/%Y")
# datac$Time <- strptime(datac$Time, "%H:%M:%S")
# 
# 
# #datac$Date <- year(ymd(datac$Date))
# 
# datac$Time <- parse_date_time(datac$Time, "%Y%m%d")

# head(datac)
# tail(datac)
str(datac)

View(datac)
#Overall trend of global reactive and active power

par(mfrow = c(2,2))
ggplot(datac, aes(x=Date, y= Global_active_power)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm,   
              se=FALSE) 

ggplot(datac, aes(x=Date, y= Global_reactive_power)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm,   
              se=FALSE) 

newdata1 <- datac[ which(datac$Date  > "2006-12-16" & datac$Date < "2007-11-16"), ]
dim(newdata1)





