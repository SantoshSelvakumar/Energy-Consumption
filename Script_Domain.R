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
# newDF <- bind_rows(yr_2006,yr_2007,yr_2008,yr_2009,yr_2010)



















