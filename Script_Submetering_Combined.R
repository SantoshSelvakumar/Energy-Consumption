## Combine Date and Time attribute values in a new attribute column
# DataSet: yr_2006 ---------------------------------------------------------------------------------------------
yourdataframe <-cbind(yourdataframe,paste(yourdataframe$Date,yourdataframe$Time), stringsAsFactors=FALSE)

yr_2006_df <- cbind(yr_2006, paste(yr_2006$Date,yr_2006$Time), stringsasFactors=FALSE)
yr_2006_df
head(yr_2006_df)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(yr_2006_df)[11] <-"DateTime"

## Move the DateTime attribute within the dataset
yr_2006_df <- yr_2006_df[,c(ncol(yr_2006_df), 1:(ncol(yr_2006_df)-1))]
head(yr_2006_df)

## Convert DateTime from POSIXlt to POSIXct 
yr_2006_df$DateTime <- as.POSIXct(yr_2006_df$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(yr_2006_df$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(yr_2006_df)

## Combine Date and Time attribute values in a new attribute column
# DataSet: yr_2007 ---------------------------------------------------------------------------------------------
yr_2007_df <- cbind(yr_2007, paste(yr_2007$Date,yr_2007$Time), stringsasFactors=FALSE)
yr_2007_df
head(yr_2007_df)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(yr_2007_df)[11] <-"DateTime"

## Move the DateTime attribute within the dataset
yr_2007_df <- yr_2007_df[,c(ncol(yr_2007_df), 1:(ncol(yr_2007_df)-1))]
head(yr_2007_df)

## Convert DateTime from POSIXlt to POSIXct 
yr_2007_df$DateTime <- as.POSIXct(yr_2007_df$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(yr_2007_df$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(yr_2007_df)

## Combine Date and Time attribute values in a new attribute column
# DataSet: yr_2008 ---------------------------------------------------------------------------------------------
yr_2008_df <- cbind(yr_2008, paste(yr_2008$Date,yr_2008$Time), stringsasFactors=FALSE)
yr_2008_df
head(yr_2008_df)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(yr_2008_df)[11] <-"DateTime"

## Move the DateTime attribute within the dataset
yr_2008_df <- yr_2008_df[,c(ncol(yr_2008_df), 1:(ncol(yr_2008_df)-1))]
head(yr_2008_df)

## Convert DateTime from POSIXlt to POSIXct 
yr_2008_df$DateTime <- as.POSIXct(yr_2008_df$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(yr_2008_df$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(yr_2008_df)

## Combine Date and Time attribute values in a new attribute column
# DataSet: yr_2009 ---------------------------------------------------------------------------------------------
yr_2009_df <- cbind(yr_2009, paste(yr_2009$Date,yr_2009$Time), stringsasFactors=FALSE)
yr_2009_df
head(yr_2009_df)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(yr_2009_df)[11] <-"DateTime"

## Move the DateTime attribute within the dataset
yr_2009_df <- yr_2009_df[,c(ncol(yr_2009_df), 1:(ncol(yr_2009_df)-1))]
head(yr_2009_df)

## Convert DateTime from POSIXlt to POSIXct 
yr_2009_df$DateTime <- as.POSIXct(yr_2009_df$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(yr_2009_df$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(yr_2009_df)

## Combine Date and Time attribute values in a new attribute column
# DataSet: yr_2010 ---------------------------------------------------------------------------------------------
yr_2010_df <- cbind(yr_2010, paste(yr_2010$Date,yr_2010$Time), stringsasFactors=FALSE)
yr_2010_df
head(yr_2010_df)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(yr_2010_df)[11] <-"DateTime"

## Move the DateTime attribute within the dataset
yr_2010_df <- yr_2010_df[,c(ncol(yr_2010_df), 1:(ncol(yr_2010_df)-1))]
head(yr_2010_df)

## Convert DateTime from POSIXlt to POSIXct 
yr_2010_df$DateTime <- as.POSIXct(yr_2010_df$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(yr_2010_df$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(yr_2010_df)

# Lubridate: yr_2006_df -----------------------------------------------------------------------------------------------------
# Calling the Lubridate library
library(lubridate)

## Create "year" attribute with lubridate
yr_2006_df$Year <- year(yr_2006_df$DateTime)





















