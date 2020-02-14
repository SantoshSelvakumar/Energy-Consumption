# Calling all the Packages include: Caret, ggplot2, lattice, dplyr, corrplot, e1071, ISLR
library(caret)

library(ggplot2)

library(lattice)

library(dplyr)

library(corrplot)

library(e1071)

#Loading the Exisiting Product ---------------------------------------------------------------------------------------------------------------------------------
# Loading the .csv file in the environment
existing_products <- read.csv("existingproductattributes2017.csv")

# dummify the data - To convert all the factor or character data types to binary features.

dummy_formula <- dummyVars(" ~ .", data = existing_products)
                          
readyData <- data.frame(predict(dummy_formula, newdata = existing_products))

#To check all the Dummy variables created
readyData

glimpse(readyData)


# To check all the data types in DataFrame 
str (existing_products)

# To check all the missing data in DF. 
Summary(readyData)

# All the attributes with NA are mentioned as 'False'
is.na(readyData)

# To delete all the attribute with missing information
# yourdataframe$attributeWithMissingData <- NULL
readyData$BestSellersRank <- NULL

# Correlation Function
# Installed the Correlation package
# Code: install.packages("corrplot")

# Loading the Library function - caret, ggplot and lattice
Library("corrplot")
# Correlation Matrix
corrData <- cor(readyData)

# Removing the column "Best Sellers Rank" from the "readyData" DS
readyData$BestSellersRank <- NULL

corrData

# Plotting the correlation matrix
corrplot(corrData)

# Checking the colinearity of the individual attribute in the existing products---------------------------------------------------

#Collinearity coefficients of Product Type
plot(existing_products$ProductType, existing_products$Volume)

# cor(fat$age, fat$pctfat.brozek, method="pearson")

existing_products$ProductType <- as.character(existing_products$ProductType)
class(ProductType)

str(existing_products)

# Checking for the Collinearity coefficients of all the attributes under "Product Type"
cor(readyData$Volume,readyData$ProductType.Accessories, method = "pearson") # ProductType.Accessories - Coefficient is 0.1278038

cor(readyData$Volume,readyData$ProductType.Display, method = "pearson") # ProductType.Display - Coefficient is -0.03758386

cor(readyData$Volume,readyData$ProductType.ExtendedWarranty, method = "pearson")  # ProductType.ExtendedWarranty - Coefficient is 0.07086528

cor(readyData$Volume,readyData$ProductType.GameConsole, method = "pearson") # ProductType.GameConsole - Coefficient is 0.3882982

cor(readyData$Volume,readyData$ProductType.Laptop, method = "pearson") # ProductType.Laptop - Coefficient is -0.06979958

cor(readyData$Volume, readyData$ProductType.Netbook, method = "pearson") # ProductType.Netbook - Coefficient is -0.07001054

cor(readyData$Volume, readyData$ProductType.PC, method = "pearson") # ProductType.Netbook - Coefficient is -0.1028917

cor(readyData$Volume, readyData$ProductType.Printer, method = "pearson") # ProductType.Netbook - Coefficient is -0.1492007

cor(readyData$Volume, readyData$ProductType.PrinterSupplies, method = "pearson") # ProductType.Netbook - Coefficient is -0.09040334

cor(readyData$Volume, readyData$ProductType.Smartphone, method = "pearson") # ProductType.Netbook - Coefficient is -0.03850828

cor(readyData$Volume, readyData$ProductType.Software, method = "pearson") # ProductType.Netbook - Coefficient is 0.001196472

cor(readyData$Volume, readyData$ProductType.Tablet, method = "pearson") # ProductType.Netbook - Coefficient is -0.05094191

# Checking for the Collinearity coefficients of all the attributes other than "Product Type"
cor(readyData$Volume, readyData$ProductNum, method = "pearson") # ProductNum - Coefficient is 0.1661208

cor(readyData$Volume, readyData$Price, method = "pearson") # Price - Coefficient is  -0.142344

cor(readyData$Volume, readyData$x5StarReviews, method = "pearson") # x5StarReviews - Coefficient is  1

cor(readyData$Volume, readyData$x4StarReviews, method = "pearson") # x4StarReviews - Coefficientis  0.8790064

cor(readyData$Volume, readyData$x3StarReviews, method = "pearson") # x3StarReviews - Coefficientis  0.7633732

cor(readyData$Volume, readyData$x2StarReviews, method = "pearson") # x2StarReviews - Coefficient is  0.4872793

cor(readyData$Volume, readyData$x1StarReviews, method = "pearson") # x1StarReviews - Coefficient is  0.7633732

cor(readyData$Volume, readyData$PositiveServiceReview, method = "pearson") # PositiveServiceReview - Coefficient is 0.2550239

cor(readyData$Volume, readyData$NegativeServiceReview, method = "pearson") # NegativeServiceReview - Coefficient is 0.309419

cor(readyData$Volume, readyData$Recommendproduct, method = "pearson") # Recommendproduct - Coefficient is 0.1695413

cor(readyData$Volume, readyData$ShippingWeight, method = "pearson") # ShippingWeight - Coefficient is -0.188024

cor(readyData$Volume, readyData$ProductDepth, method = "pearson") # ProductDepth - Coefficient is 0.06610525

cor(readyData$Volume, readyData$ProductWidth, method = "pearson") # ProductWidth - Coefficient is -0.1434366

cor(readyData$Volume, readyData$ProductHeight, method = "pearson") # ProductHeight - Coefficient is -0.160004

cor(readyData$Volume, readyData$ProfitMargin, method = "pearson") # ProfitMargin - Coefficient is -0.160004

cor(readyData$Volume, readyData$Volume, method = "pearson") # ProfitMargin - Coefficient is 1

# Preprocessing the data ---------------------------------------------------------------------------------------------------------
#  Removing the attributes - ShippingWeight, ProductWidth, ProductHeight, ProfitMargin
#Back to feature selection, removing attributes ShippingWeight, ProductDepth, ProductWidth, ProductHeight, ProfitMargin
readyData[23:27] <- NULL
head(readyData)

#deleting Productnumber and Price
readyData[13:14] <- NULL
head(readyData)

#deleting NegativeServiceReview and recommendproduct
readyData[19:20] <- NULL

#remove x5StarReviews due to perfect correlation with Volume and colinear =1
readyData[13] <- NULL

#Checking for colinearity
cor(readyData$x3StarReviews, readyData$x4StarReviews) #correlation coefficient is 0.9372142, indicating collinearity
cor(readyData$x2StarReviews, readyData$x4StarReviews) #correlation coefficient is 0.6790056
cor(readyData$x2StarReviews, readyData$x3StarReviews) #correlation coefficient is 0.86148
cor(readyData$x1StarReviews, readyData$x2StarReviews) #correlation coefficient is 0.951913


#remove the attributes - ProductType.Accessories	ProductType.Display	ProductType.ExtendedWarranty
readyData[1:3] <- NULL
head(readyData)

# Remove the ProductTypes excluding the game console
readyData[2:9] <- NULL
head(readyData)

#remove the attributes x3StarReviews
readyData[3] <- NULL
head(readyData)

# Removing outliers (row#50) in readyData 
readyData <- readyData[-c(50),]

summary(readyData)

# Again to check the Correlation matrix after cleaning the data set
corrData3 <- cor(readyData)
corrData3
corrplot(corrData3, method="square")
corrplot(corrData3,order="AOE", method="color", addCoef.col = "gray")




#Linear Regression model with x4StarReview (independent variable) vs. Volume (dependent variable)
#LM: x4StarReview vs. Volume
# existing_products_4star <- lm(existing_products$Volume ~ existing_products$x4StarReviews, trainSet) 
# summary(existing_products_4star) #R-squared=0.6144

# Linear Regression Model Modalisation 1-------------------------------------------------------------------------------------------------------------------------
#creating train and testdata (80/20%)

set.seed(999)

trainSize<-round(nrow(readyData)*0.8)
testSize<-nrow(readyData)-trainSize

training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,] 

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

----------------------------------------------------------------------------
#train model: LM
mod_lm <- train(Volume~PositiveServiceReview+x4StarReviews+x2StarReviews+x1StarReviews + ProductType.GameConsole,
                data = trainSet,
                method="lm",
                trControl = fitControl)
mod_lm  
# RMSE     Rsquared   MAE     
# 361.061  0.8697945  198.1672

summary(mod_lm)

test_results_lm_1 <- predict(object = mod_lm, 
                           newdata = testSet)
postResample(testSet$Volume, test_results_lm_1)
# mod_lm
# RMSE    Rsquared         MAE 
# 511.8931783   0.5761537 302.2498969

----------------------------------------------------------------------------
mod_lm2 <- train(Volume~PositiveServiceReview,
                 data = trainSet,
                 method="lm",
                 trControl = fitControl)
mod_lm2
# RMSE      Rsquared   MAE    
# 411.0243  0.8553308  310.266

summary(mod_lm2)

test_results_lm_2 <- predict(object = mod_lm3, 
                             newdata = testSet)
postResample(testSet$Volume, test_results_lm_2)
# mod_lm
# RMSE    Rsquared         MAE 
# 502.0417112   0.5945145 298.7420320
------------------------------------------------------------------------------

mod_lm3 <- train(Volume~PositiveServiceReview+x4StarReviews,
                 data = trainSet,
                 method="lm",
                 trControl = fitControl)
mod_lm3
summary(mod_lm3)
# RMSE      Rsquared   MAE     
# 204.3187  0.8996569  148.1854

summary(mod_lm3)

test_results_lm_3 <- predict(object = mod_lm3, 
                             newdata = testSet)
postResample(testSet$Volume, test_results_lm_3)

# RMSE    Rsquared         MAE 
# 502.0417112   0.5945145 298.7420320 

------------------------------------------------------------------------------

mod_lm4 <- train(Volume~PositiveServiceReview+x4StarReviews+ProductType.GameConsole,
                 data = trainSet,
                 method="lm",
                 trControl = fitControl)
mod_lm4
# RMSE      Rsquared   MAE     
# 224.8089  0.8534121  155.2589

summary(mod_lm4)

test_results_lm <- predict(object = mod_lm4, 
                           newdata = testSet)
postResample(testSet$Volume, test_results_lm)
# mod_lm
# RMSE    Rsquared         MAE 
# 508.9121490   0.5989194 293.0940137 

---------------------------------------------------------------------------------
Data_Predict <- predict(mod_lm3, newdata=testSet)

Data_Predict
testSet$Volume
diff <- Data_Predict - testSet$Volume
df <- data.frame(Predicted=Data_Predict,Observed=testSet$Volume, Difference=diff)

plot(df)

# SVM model using e1071 Modalisation 1-------------------------------------------------------------------------------------------------------------------------

library(e1071)
set.seed(999)

#split to train and test data 80/20
inTraining <- createDataPartition(readyData$Volume, p = 0.8, list = FALSE)
trainset2 <- readyData[inTraining,]
testset2 <- readyData[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train model: SVM
mod_svm <- train(Volume~PositiveServiceReview+x4StarReviews+x2StarReviews + ProductType.GameConsole,
                 data = trainset2,
                 method="svmPoly")

mod_svm
summary(mod_svm)

#Results on testset
test_results_svm <- predict(object = mod_svm, 
                            newdata = testset2)
postResample(testset2$Volume, test_results_svm) # 363.3830  0.6935439    182.1203

difference <- test_results_svm - testset2$Volume

RMSE <- sqrt(mean(difference^2)) 
# RMSE    Rsquared         MAE 
# 326.2268968   0.7179017 158.3238355

# Random Forest Model Modalisation 1-------------------------------------------------------------------------------------------------------------------------

set.seed(100)
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_rf <- train(Volume~PositiveServiceReview+x4StarReviews+x2StarReviews + ProductType.GameConsole,
                data = trainset2,
                method="rf",
                preProcess = c("center","scale"))

mod_rf #RMSE 212.6188, R squared 0.8738800

#Results on testset
test_results_rf <- predict(object = mod_rf, 
                           newdata = testset2)
postResample(testset2$Volume, test_results_rf) #RMSE 190.0181828, R squared 0.9601338

# K-nn Model Modalisation 1----------------------------------------------------------------------------------------------------------------
# install.packages("ISLR")
library(ISLR)

set.seed(222)

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_kNN <- train(Volume~PositiveServiceReview+x4StarReviews+x2StarReviews+x1StarReviews + ProductType.GameConsole,
                 data = trainset2,
                 method = "knn",
                 trControl = fitControl,
                 preProcess = c("center","scale")) 
mod_kNN 
#k  RMSE      Rsquared   MAE     
# 5  269.7167  0.8311423  150.1115

#Results on testset
test_results_kNN <- predict(object = mod_kNN, 
                            newdata = testset2)
postResample(testset2$Volume, test_results_kNN)
# RMSE    Rsquared         MAE 
# 198.2319939   0.8971056 112.7794872 

# SVM - Second Modalisation--------------------------------------------------------------------------------------------------------------

library(e1071)
library(caret)
set.seed(342)

#split to train and test data 80/20
inTraining <- createDataPartition(readyData$Volume, p = 0.8, list = FALSE)
trainset3 <- readyData[inTraining,]
testset3 <- readyData[-inTraining,]

#SVM

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train model: SVM
mod_svm2 <- train(Volume~PositiveServiceReview+x4StarReviews+x2StarReviews + ProductType.GameConsole,
                  data = trainset3,
                  method="svmPoly")

mod_svm2
# degree  scale  C     RMSE        Rsquared   MAE  
# 0.100  1.00    364.6844  0.7260813    178.8642

#Results on testset
test_results_svm2 <- predict(object = mod_svm2, 
                             newdata = testset3)
postResample(testset3$Volume, test_results_svm2)

difference <- test_results_svm2 - testset3$Volume
RMSE <- sqrt(mean(difference^2)) 

# RMSE    Rsquared         MAE 
# 328.9571056   0.6452464 149.0654628 


#Random Forest - Second Modalisation--------------------------------------------------------------------------------------------------------------
#10 fold cross validation
set.seed(130)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_rf2 <- train(Volume~PositiveServiceReview+x4StarReviews+x2StarReviews + ProductType.GameConsole,
                 data = trainset3,
                 method="rf",
                 preProcess = c("center","scale"))

mod_rf2 
# mtry  RMSE      Rsquared   MAE     
# 4     223.4219  0.8796565  118.6103

#Results on testset
test_results_rf2 <- predict(object = mod_rf2, 
                            newdata = trainset3)
postResample(trainset3$Volume, test_results_rf2) 
# RMSE   Rsquared        MAE 
# 95.3048674  0.9772835 45.6980549

# KNN Model - Second Modalisation--------------------------------------------------------------------------------------------------------------
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_kNN2 <- train(Volume~PositiveServiceReview+x4StarReviews+x2StarReviews + ProductType.GameConsole,
                  data = trainset3,
                  method = "knn",
                  trControl = fitControl,
                  preProcess = c("center","scale")) 
mod_kNN2 
# k  RMSE      Rsquared   MAE     
# 5  248.7389  0.9127179  153.5486

#Results on testset
test_results_kNN2 <- predict(object = mod_kNN2, 
                             newdata = testset3)
postResample(testset3$Volume, test_results_kNN2) 
# RMSE    Rsquared         MAE 
# 315.1234581   0.6646472 122.8307692 

# Third Modalisation--------------------------------------------------------------------------------------------------------------

library(e1071)
library(caret)
set.seed(342)

#split to train and test data 80/20
inTraining <- createDataPartition(readyData$Volume, p = 0.8, list = FALSE)
trainset4 <- readyData[inTraining,]
testset4 <- readyData[-inTraining,]

#SVM

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train model: SVM
mod_svm3 <- train(Volume~PositiveServiceReview+x4StarReviews + ProductType.GameConsole,
                  data = trainset4,
                  method="svmPoly")

mod_svm3

# degree  scale  C     RMSE        Rsquared   MAE   
# 1       0.100  0.50    328.6435  0.7436749    167.3372

#Results on testset
test_results_svm3 <- predict(object = mod_svm3, 
                             newdata = testset4)
postResample(testset4$Volume, test_results_svm3)

difference <- test_results_svm2 - testset3$Volume
RMSE <- sqrt(mean(difference^2)) 

# RMSE    Rsquared         MAE 
# 318.5548119   0.6725876 125.3872628 

#Random Forest - Third Modalisation--------------------------------------------------------------------------------------------------------------
#10 fold cross validation
set.seed(130)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_rf3 <- train(Volume~PositiveServiceReview+x4StarReviews+x2StarReviews + ProductType.GameConsole,
                 data = trainset4,
                 method="rf",
                 preProcess = c("center","scale"))

mod_rf3 
# mtry  RMSE      Rsquared   MAE  
# 4     223.4219  0.8796565  118.6103

#Results on testset
test_results_rf3 <- predict(object = mod_rf3, 
                            newdata = trainset4)
postResample(trainset4$Volume, test_results_rf3) 

#       RMSE   Rsquared        MAE 
# 95.3048674  0.9772835 45.6980549 

# KNN Model - Third Modalisation--------------------------------------------------------------------------------------------------------------
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

mod_kNN3 <- train(Volume~PositiveServiceReview+x4StarReviews + ProductType.GameConsole,
                  data = trainset4,
                  method = "knn",
                  trControl = fitControl,
                  preProcess = c("center","scale")) 
mod_kNN3 

# k  RMSE      Rsquared   MAE     
# 5  229.7652  0.9186485  143.3744
#Results on testset
test_results_kNN3 <- predict(object = mod_kNN3, 
                             newdata = testset4)
postResample(testset4$Volume, test_results_kNN3) #RMSE 277.7277324, R squared 0.9160001
# RMSE   Rsquared        MAE 
# 307.295838   0.697645 116.424908 

# New Product list - Predicting the model------------------------------------------------------------------------------------

readNewData <- read.csv("newproductattributes2017 (1).csv")

dummy_formulaNew <- dummyVars(" ~ .", data = readNewData)

new_Data <- data.frame(predict(dummy_formulaNew, newdata = readNewData))

new_Data[1:3] <- NULL
head(new_Data)

new_Data[2:12] <- NULL
head(new_Data)

new_Data[3] <- NULL
head(new_Data)

new_Data[6:13] <- NULL
head(new_Data)

# set.seed(143)
# 
# train_new <- round(nrow(new_Data)*0.8)
# test_new <- nrow(new_Data)-train_new
# 
# training_new <- sample(seq_len(nrow(new_Data)),size =train_new)
# new_trainSet <- new_Data[training_new,]
# new_testSet <- new_Data[-training_new,] 
# 
# #10 fold cross validation
# fitControlNew <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

---------------------------------------------------------------------------------------------------
#train model: SVM
#model selected: mod_svm2

finalPred <- predict(object = mod_svm2,newdata = new_Data)

# mod_lm
# RMSE    Rsquared         MAE 
# 511.8931783   0.5761537 302.2498969

output_DS <- read.csv("newproductattributes2017 (1).csv")

output_DS$predictions <- finalPred


write.csv(output_DS, file="C2.T3output.csv", row.names = TRUE)







