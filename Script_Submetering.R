# Install the RMySQL package---------------------------------------------------------------------------------------------
install.packages(c("RMySQL","dplyr","DBI","dbplyr","ggplot2","modelDB","tidypredict","config"))

library(RMySQL)
library(dplyr)
library(DBI)
library(dbplyr)
library(ggplot2)
library(modelDB)
library(tidypredict)
library(config)
# Calling the file -------------------------------------------------------------------------------------------------------
# ReadyData <- read.delim(file.choose("C:/Users/Santosh/Desktop/Ubiqum/RStudio/Domfain Research and Exploratory Data Analysis/household_power_consumption.txt"))
con = dbConnect(MySQL(), user='NA', password='NA', dbname='household_power_consumption', host='C:/Users/Santosh/Desktop/Ubiqum/RStudio/Domain_Research_and_Exploratory_Data_Analysis/household_power_consumption.txt')
glimpse(con)

# Connect the Database
con1 = DBI::dbConnect(RSQLite::SQLite(), dbname = "ReadyData")

summary(con)

dbGetInfo(con)

dbListResults(con)

dbListTables(con)

dbDisconnect(con)


