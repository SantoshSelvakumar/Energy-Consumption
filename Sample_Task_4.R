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

newdata2 <- datac[ which(datac$Date  > "2007-11-16" & datac$Date < "2008-12-16"), ]
dim(newdata2)

newdata3 <- datac[ which(datac$Date  > "2008-11-16" & datac$Date < "2009-12-16"), ]
dim(newdata2)

newdata4 <- datac[ which(datac$Date  > "2009-11-16" & datac$Date < "2010-12-16"), ]
dim(newdata4)

par(mfrow=c(2,2))
# qplot(newdata4$kitchen, binwidth=3)
# qplot(newdata4$laundry, binwidth=3 )
# qplot(newdata4$AC, binwidth=3)

# NewData4
par(mfrow=c(2,2))
hist(newdata4$kitchen)
hist(newdata4$laundry)
hist(newdata4$AC)

# NewData1
par(mfrow=c(2,2))
hist(newdata1$kitchen)
hist(newdata1$laundry)
hist(newdata1$AC)

# NewData2
par(mfrow=c(2,2))
hist(newdata2$kitchen)
hist(newdata2$laundry)
hist(newdata2$AC)

# NewData3
par(mfrow=c(2,2))
hist(newdata3$kitchen)
hist(newdata3$laundry)
hist(newdata3$AC)

#For dataset one
df <- newdata1%>%select(c(kitchen,laundry,AC))%>%summarise_each(funs(sum))
sums <- c(df$kitchen,df$laundry,df$AC)

lbls <- c("kitchen", "laundry", "AC")
pie3D(sums,labels=lbls,explode=0.1,
      main="Pie Chart of summeterAreas ")

#For daatset two
df1 <- newdata2%>%select(c(kitchen,laundry,AC))%>%summarise_each(funs(sum))
sums <- c(df1$kitchen,df1$laundry,df1$AC)

lbls <- c("kitchen", "laundry", "AC")
pie3D(sums,labels=lbls,explode=0.1,
      main="Pie Chart of summeterAreas ")

#For daatset two
df2 <- newdata3%>%select(c(kitchen,laundry,AC))%>%summarise_each(funs(sum))
sums <- c(df2$kitchen,df2$laundry,df2$AC)

lbls <- c("kitchen", "laundry", "AC")
pie3D(sums,labels=lbls,explode=0.1,
      main="Pie Chart of summeterAreas ")

#For daatset two
df3 <- newdata4%>%select(c(kitchen,laundry,AC))%>%summarise_each(funs(sum))
sums <- c(df3$kitchen,df3$laundry,df3$AC)

lbls <- c("kitchen", "laundry", "AC")
pie3D(sums,labels=lbls,explode=0.1,
      main="Pie Chart of summeterAreas ")

# Time series analysis

dg <- aggregate(datac$kitchen,by=list((substr(datac$Date,1,7))),sum)
dim(dg)

#building time series object
t <- ts(dg$x, frequency=12, start=c(2006,12))
class(t)

kable(t)

plot(t)

# Laundry
dg_l <- aggregate(datac$laundry,by=list((substr(datac$Date,1,7))),sum)
t_l <- ts(dg_l$x, frequency=12, start=c(2006,12))
plot(t_l)

# AC
dg_A <- aggregate(datac$AC,by=list((substr(datac$Date,1,7))),sum)
t_A <- ts(dg_A$x, frequency=12, start=c(2006,12))
plot(t_A)

#comparing usage by zones
par(mfrow=c(2,2))
boxplot(t~cycle(t))
boxplot(t_l~cycle(t_l))
boxplot(t_A~cycle(t_A))

# Seasonal Decomposition
er <- decompose(t)
plot(er)

#Seasonaly adjusted
sesA <- t - er$seasonal
plot(sesA)

# For AC
AC_decomp <- decompose(t_A)
plot(AC_decomp)

sesAC <- t_A - AC_decomp$seasonal
plot(sesAC)

# holtwinter
AC_forecasts <- HoltWinters(t_A, beta=FALSE, gamma=FALSE)
AC_forecasts

#fitted
AC_forecasts$fitted

plot(AC_forecasts)

AC_forecasts$SSE

forcast <- forecast.HoltWinters(AC_forecasts, h=8);kable(forcast)

Box.test(forcast$residuals, lag=20, type="Ljung-Box")

plot.ts(forcast$residuals)

plot(forcast$residuals)

# plotting residual
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}



plotForecastErrors(forcast$residuals)






















