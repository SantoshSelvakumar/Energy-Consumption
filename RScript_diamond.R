# Libraries 
if (require(tidyverse) == FALSE) {
  install.packages('pacman')
}
pacman::p_load(tidyverse)

# Library pacman
library(pacman)
library(tidyverse)
library(caret)
library(ggplot2)
library(lattice)
library(dplyr)
library(corrplot)
library(e1071)

# Import the data "train.rds" and "validation_NOprice.rds"
training <- readRDS("C:/Users/Santosh/Desktop/Ubiqum/RStudio/Datathon_Diamond/train.rds")
head(training)
glimpse(training)

testing <- readRDS("C:/Users/Santosh/Desktop/Ubiqum/RStudio/Datathon_Diamond/validation_NOprice.rds")
head(testing)
glimpse(testing)

# Dataset information
?diamonds

# The diamonds with a bad cut are in average more expensive
training %>% 
  ggplot(aes(cut, price)) + 
  geom_boxplot()

# The diamonds with a bad color are also more expensive
training %>% 
  ggplot(aes(color, price)) + 
  geom_boxplot()

# And the diamonds with a bad clarity have a higher price
training %>% 
  ggplot(aes(clarity, price)) +
  geom_boxplot()

# checking the dimension of Training and testing data sets
dim(training)
dim(testing)

# Task 1
# Why the diamonds that have a fair cut, bad color and a bad clarity are, in median, more expensive? 
# Finding the mean of the diamonds for fair cut
# ggplot(data=training) + geom_histogram(binwidth=500, aes(x=training$price))

# # Mean is not available for the cut, color and clarity of the testing dataset
# mean.cut <- mean(training$cut, na.rm = TRUE)
# mean.cut

ggplot(training, aes(carat, price)) +
  geom_hex(bins = 50)

ggplot(training, aes(depth, price)) +
  geom_hex(bins = 50)

ggplot(training, aes(table, price)) +
  geom_hex(bins = 50)

ggplot(training, aes(x, price)) +
  geom_hex(bins = 50)

ggplot(training, aes(y, price)) +
  geom_hex(bins = 50)

ggplot(training, aes(z, price)) +
  geom_hex(bins = 50)

# Using ggplot along with geom_point, geom_quantile and geom_smooth for better line
ggplot(training, aes(x = carat, y = price)) + geom_point() + geom_quantile() + geom_smooth()

ggplot(training, aes(x = depth, y = price)) + geom_point() + geom_quantile() + geom_smooth()

ggplot(training, aes(x = table, y = price)) + geom_point() + geom_quantile() + geom_smooth()

ggplot(training, aes(x = x, y = price)) + geom_point() + geom_quantile() + geom_smooth()

ggplot(training, aes(x = y, y = price)) + geom_point() + geom_quantile() + geom_smooth()

ggplot(training, aes(x = z, y = price)) + geom_point() + geom_quantile() + geom_smooth()

# mean.price <- mean(training$price,na.rm = TRUE)
# mean.price

# Preprocess the data
# dummify the data

newDataFrame <- dummyVars(" ~ .", data = training)

readyData <- data.frame(predict(newDataFrame, newdata = training))

corrData <- cor(readyData)

corrData

corrplot(corrData)

# combining the attributes of x*y*z
c(readydata$combined) <- c(readyData$x*readyData$y*readyData$z)

newdata2 <- readyData %>%
  mutate(size = x*y*z)

corrData <- cor(newdata2)

corrData

corrplot(corrData)

view(newdata2)
newdata2[26:29] <- NULL
head(newdata2)

summary(newdata2)

corrData1 <- cor(newdata2)

corrData1

corrplot(corrData1)

# Choosing CARAT as best attribute for modeling 
# Linear model

# Linear Model - Modalisation 1 ------------------------------------------------------------------------------------
mod_lm <- lm(formula = price ~ carat, data = newdata2)
mod_lm
summary(mod_lm)

# Linear Model - Modalisation 2 ------------------------------------------------------------------------------------
set.seed(250)

trainSize<-round(nrow(newdata2)*0.8)
testSize<-nrow(newdata2)-trainSize

training_indices<-sample(seq_len(nrow(newdata2)),size =trainSize)
trainSet<-newdata2[training_indices,]
testSet<-newdata2[-training_indices,] 

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_lm_1 <- train(price ~ carat,
                data = trainSet,
                method="lm",
                trControl = fitControl)
  
mod_lm_1
# RMSE    Rsquared         
# 1568    0.8482   

summary(mod_lm_1)

test_results_lm_1 <- predict(object = mod_lm, 
                             newdata = testSet)
postResample(testSet$price, test_results_lm_1)

# RMSE     Rsquared          MAE 
# 1542.1411051    0.8520021 1009.814096

# Random Forest Model - Modalisation 1 ---------------------------------------------------------------------------------------------------------
set.seed(100)

inTraining1 <- createDataPartition(newdata2$price, p = 0.8, list = FALSE)
trainset3 <-newdata2[inTraining1,]
testset3 <- newdata2[-inTraining1,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_rf <- train(price ~ carat,
                data = trainset3,
                method="rf",
                preProcess = c("center","scale"))

mod_rf 
# RMSE      Rsquared   MAE     
# 1456.627  0.8701383  826.7318

#Results on testset
test_results_rf <- predict(object = mod_rf, 
                           newdata = testset3)
postResample(testset2$price, test_results_rf) 

difference <- test_results_rf - testset3$price

RMSE <- sqrt(mean(difference^2)) 

RMSE
# RMSE     Rsquared          MAE 
# 1449.6304761    0.8681834  834.6042725 

# Random Forest Model - Modalisation 2 ---------------------------------------------------------------------------------------------------------
set.seed(100)

inTraining2 <- createDataPartition(newdata2$price~carat, p = 0.8, list = FALSE)
trainset4 <-newdata2[inTraining,]
testset4 <- newdata2[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_rf2 <- train(price ~ carat+cut+color,
                data = trainset4,
                method="rf",
                preProcess = c("center","scale"))

mod_rf2 
# RMSE      Rsquared   MAE     
# 1456.627  0.8701383  826.7318

#Results on testset
test_results_rf1 <- predict(object = mod_rf, 
                           newdata = testset4)
postResample(testset2$price, test_results_rf1) 

difference <- test_results_rf - testset3$price

RMSE <- sqrt(mean(difference^2)) 

RMSE
# RMSE     Rsquared          MAE 
# 1449.6304761    0.8681834  834.6042725 


# SVM Model - Modalisation 1 ---------------------------------------------------------------------------------------------------------
set.seed(250)

#split to train and test data 80/20
inTraining <- createDataPartition(newdata2$price, p = 0.8, list = FALSE)
trainset2 <- newdata2[inTraining,]
testset2 <- newdata2[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train model: SVM
mod_svm <- train(price ~ carat,
                 data = trainset2,
                 method="svmPoly")

mod_svm
summary(mod_svm)

#Results on testset
test_results_svm <- predict(object = mod_svm, 
                            newdata = testset2)
postResample(testset2$Volume, test_results_svm) 

difference <- test_results_svm - testset2$Volume

RMSE <- sqrt(mean(difference^2)) 

# K-nn Model Modalisation 1----------------------------------------------------------------------------------------------------------------
library(ISLR)

set.seed(222)

inTraining3 <- createDataPartition(newdata2$price, p = 0.8, list = FALSE)
trainset5 <-newdata2[inTraining3,]
testset5 <- newdata2[-inTraining3,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_kNN <- train(price ~.,
                 data = trainset5,
                 method = "knn",
                 trControl = fitControl,
                 preProcess = c("center","scale")) 
mod_kNN 
# k  RMSE      Rsquared   MAE     
# 5  1120.026  0.9240848  540.7460
# 7  1191.127  0.9154571  579.3953
# 9  1258.959  0.9068931  619.6566


#Results on testset
test_results_kNN <- predict(object = mod_kNN, 
                            newdata = testset5)
postResample(testset5$price, test_results_kNN)
# RMSE     Rsquared          MAE 
# 1117.3076676    0.9264158  533.1252527 
---------------------------------------------------------------------------------
#10 fold cross validationwith caret 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_kNN1 <- train(price ~ carat,
                 data = trainset5,
                 method = "knn",
                 trControl = fitControl,
                 preProcess = c("center","scale")) 
mod_kNN1 


#Results on testset
test_results_kNN <- predict(object = mod_kNN, 
                            newdata = testset5)
postResample(testset5$price, test_results_kNN)







