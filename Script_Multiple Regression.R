# Loading the .csv file in the environment
existing_products <- read.csv("existingproductattributes2017.csv")

# dummify the data - To convert all the factor or character data types to binary features.

dummy_formula <- dummyVars(" ~ .", data = existing_products)
                          
readyData <- data.frame(predict(dummy_formula, newdata = existing_products))

#To check all the Dummy variables created
readyData


# To check all the data types in DataFrame 
str (existing_products)

# To check all the missing data in DF. 
Summary (readyData)

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

# -------------------------------------------------------------------------------------------------------------------------
#Predicting a Linear Regression Model with "Volume", as the dependent variable

#Calling the caret library
library(caret)

# Existing_Training <- newDataFrame_1
y_volume <- existing_products$Volume

# x_Product <- c(existing_products$NegativeServiceReview, existing_products$PositiveServiceReview +
#                   existing_products$PositiveServiceReview, existing_products$product +
#                   existing_products$ProductHeight, existing_products$ProductNum + 
#                   existing_products$ProductWidth, existing_products$ProfitMargin + 
#                   existing_products$ShippingWeight, existing_products$x2StarReviews +
#                   existing_products$x4StarReviews)

rel_var <- c("NegativeServiceReview", "ProfitMargin")

linear_Model <- lm(Volume ~ ., data = readyData)
# linear_Model <- lm(volume,existing_products~.)
# 
# summary(linear_Model)
# 
# # Predicted Output
# predicted= predict(linear_Model,x_test)
# ---------------------------------------------------
# set.seed(2019)
# 
# # train and test
# Existing_Products_Model <- createDataPartition(y = completed_survey$brand,
#                                  p = 0.75,
#                                  list = F)
# train <- completed_survey[train_ids,]
# test <- completed_survey[-train_ids,]
# 
# # cross validation
# ctrl <- trainControl(method = "repeatedcv",
#                      repeats = 5,
#                      number = 3)
# 
# # Creating the model with caret and using the model rpart
# mod_caret_dt <- caret::train(brand_name ~ .,
#                              data = train %>% dplyr::select(-brand),
#                              method = "rpart",
#                              trControl = ctrl)







