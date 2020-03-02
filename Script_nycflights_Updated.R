# install.packages("nycflights13", repos='http://cran.us.r-project.org')# Data: nycflights13
# Link: https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html

# install packages
# install.packages("nycflights13")
# install.packages("tidyverse")
# Call Library
library("nycflights13", lib.loc = "C:/Program Files/R/R-3.1.1/library")
library(tidyverse)
library(tibble)
library(caret)
library(ggplot2)
library(lattice)

#Assign a variable, "data_raw", to the complete dataframe, "flights".
#Then, display the "head" and "tail" of the dataset, "data_raw".
data_raw<-flights
head(data_raw)
# Tail - Return the First or Last Part of an Object
tail(data_raw)

#Display the summary statistics of "data_raw".
summary(data_raw)

#Display the names found in "data_raw".
names(data_raw)

#Display the structure of "data_raw".
str(data_raw)

# Categorize 'origin' as a factor and display its resulting levels.
data_raw$origin = as.factor(data_raw$origin)
levels(data_raw$origin)

# Categorize 'dest' as a factor and display its resulting levels.
data_raw$dest = as.factor(data_raw$dest)
levels(data_raw$dest)

#Remove any rows that contain "NA" in "data_raw", creating "data_clean".
#Randomly take a sample of 5000 observations from "data_clean", creating "nycflights".
data_clean<-na.omit(data_raw)

flight.index = sample(1:nrow(data_clean),5000,replace=FALSE)

nycflights<-data_clean[flight.index,]

summary(nycflights)

names(nycflights)

str(nycflights)

#Display the head and tail of "nycflights".
head(nycflights)

tail(nycflights)

#Display the levels of 'origin' and 'dest' within "nycflights".
levels(nycflights$origin)

levels(nycflights$dest)

par(mfrow=c(1,1))
#Create a histogram of Arrival Time Delays ('arr_delay') across all 2013 flights.
hist(nycflights$arr_delay, main = "Arrival Time Delays [in minutes]")

par(mfrow=c(1,1))
#Create a boxplot of Arrival Time Delays ('arr_delay') at each destination airport ('dest') [ylim = c(min|'arr_delay',max|'arr_delay')].
boxplot(nycflights$arr_delay~nycflights$dest, main = "Arrival Time Delays [in minutes]", ylim = c(min(nycflights$arr_delay),max(nycflights$arr_delay)), ylab = "Minutes")

par(mfrow=c(1,1))
#Create a boxplot of Arrival Time Delays ('arr_delay') at each destination airport ('dest') [ylim = c(~1st Quarter|'arr_delay',~3rd Quarter|'arr_delay')].(Gives better indication of median differences among destination airport locations.)
boxplot(nycflights$arr_delay~nycflights$dest, main = "Arrival Time Delays [in minutes]", ylim = c(-20,20), ylab = "Minutes")

#Perform an analysis of variance (ANOVA) for the different mean values observed for the delays in arrival time, given the factor 'origin'.
model_origin <- aov(arr_delay~origin,nycflights)
anova(model_origin)

#Perform an analysis of variance (ANOVA) for the different mean values observed for the delays in arrival time, given the factor 'dest'.
model_dest <- aov(arr_delay~dest,nycflights)
anova(model_dest)

# Perform an analysis of variance (ANOVA) for the different mean values observed for the delays in arrival time, 
# given the interaction of 'origin' and 'dest'.
model_interaction <- aov(arr_delay~origin*dest,nycflights)
anova(model_interaction)

par(mfrow=c(1,1))
#Create an interaction plot that plots the mean values of the delays in arrival time('arr_delay') against the interaction of both 'origin' and 'dest'.
interaction.plot(nycflights$origin,nycflights$dest,nycflights$arr_delay)

# Tables of Parameter Values
#Display summary statistics of nycflights$arr_delay.
summary(nycflights$arr_delay)

#Display standard deviation of nycflights$arr_delay.
sd(nycflights$arr_delay, na.rm = FALSE)

# Diagnostics/Model Adequacy Checking
qqnorm(nycflights[,"arr_delay"])
qqline(nycflights[,"arr_delay"])

#Create a Normal Q-Q Plot of the residuals for "model_origin".
qqnorm(residuals(model_origin))
qqline(residuals(model_origin))

#Create a Normal Q-Q Plot of the residuals for "model_dest".
qqnorm(residuals(model_dest))
qqline(residuals(model_dest))

#Create a Normal Q-Q Plot of the residuals for "model_interaction".
qqnorm(residuals(model_interaction))
qqline(residuals(model_interaction))

#Perform Shapiro-Wilk Test of Normality on Delay in Arrival Time Data within "nycflights" dataset (normality is assummed if p > 0.1).
shapiro.test(nycflights[,"arr_delay"])

# Kruskal-Wallis Rank Sum Test Results
#Perform Kruskal-Wallis Rank Sum Test on Delay in Arrival Time Data within "nycflights" dataset for both 'origin' and 'dest' (identical populations is assummed if p > 0.05).
kruskal.test(nycflights[,"arr_delay"],nycflights$origin)








