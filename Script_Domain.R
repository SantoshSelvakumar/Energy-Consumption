# Install the RMySQL package
#install.packages(c("RMySQL","dplyr","DBI","dbplyr","ggplot2","modelDB","tidypredict"))
                 
library(RMySQL)
library(dplyr)
library(DBI)
library(dbplyr)
library(ggplot2)
library(modelDB)
library(tidypredict)

# DataSet for household power consumption
# calling the IRIS dataset
ReadyData <- read.delim(file.choose("C:/Users/Santosh/Desktop/Ubiqum/RStudio/Domfain Research and Exploratory Data Analysis/household_power_consumption.txt"),
                        sep = ";")
glimpse(ReadyData)

# Connect the Database
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
glimpse(irisALL)

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
glimpse(irisSELECT)

## Use asterisk to specify all attributes for download year 2006
yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
glimpse(yr_2006)

## Use asterisk to specify all attributes for download from yr_2006
attr_yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
glimpse(attr_yr_2006)

## Use asterisk to specify all attributes for download year 2007
yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
glimpse(yr_2007)

## Use asterisk to specify all attributes for download from yr_2007
attr_yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
glimpse(attr_yr_2007)

## Use asterisk to specify all attributes for download year 2008
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
glimpse(yr_2008)

## Use asterisk to specify all attributes for download from yr_2008
attr_yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
glimpse(attr_yr_2008)

## Use asterisk to specify all attributes for download year 2010
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
glimpse(yr_2009)

## Use asterisk to specify all attributes for download from yr_2009
attr_yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
glimpse(attr_yr_2009)

## Use asterisk to specify all attributes for download year 2009
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")
glimpse(yr_2010)

## Use asterisk to specify all attributes for download from yr_2009
attr_yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
glimpse(attr_yr_2009)

# Checking for date attribute, start and stop of yr_2006 data frame
str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)
# Export the file yr_2006
library(xlsx)
write.table(yr_2006, file = "C:/Users/Santosh/Desktop/yr_2006.csv", row.names = FALSE, sep=",")

# Checking for date attribute, start and stop of yr_2007 data frame
str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)
# Export the file yr_2007
write.table(yr_2007, file = "C:/Users/Santosh/Desktop/yr_2007.csv", row.names = FALSE, sep=",")

# Checking for date attribute, start and stop of yr_2008 data frame
str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)
# Export the file yr_2008
write.table(yr_2008, file = "C:/Users/Santosh/Desktop/yr_2008.csv", row.names = FALSE, sep=",")

# Checking for date attribute, start and stop of yr_2009 data frame
str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)
# Export the file yr_2009
write.table(yr_2009, file = "C:/Users/Santosh/Desktop/yr_2009.csv", row.names = FALSE, sep=",")

# Checking for date attribute, start and stop of yr_2010 data frame
str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)
# Export the file yr_2010
write.table(yr_2010, file = "C:/Users/Santosh/Desktop/yr_2010.csv", row.names = FALSE, sep=",")

# Combining all the tables (yr_2006,yr_2007,yr_2008,yr_2009,yr_2010) into one dataframe using dplyr
combinedDF <- bind_rows(yr_2006,yr_2007,yr_2008,yr_2009,yr_2010)
glimpse(combinedDF)

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


















