# #Install the caret package
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# 
# #Install the inum package for C.5 model to work.
# install.packages("inum")

#load library and set seed
library(caret)

#loading the ggplot2 library
library(ggplot2)

#loading the lattice library
library(lattice)

# Read the complete response csv file
Complete_Responses_DS <- read.csv("CompleteResponses.csv", header = TRUE,sep = ',')

#Summary of the Complete Responses (.csv file) Dataset
summary (Complete_Responses_DS)

#Pre-Processing
#convert the data type of attributes “elevel” to Factor, “car” & “zipcode” to CHARACTER data type and attributes “brand” to LOGICAL

#Rename the attributes names with the new ones 
names(Complete_Responses_DS) <- c("salary","age","level","car_character","zipcode_character", "credit", "brand_logical")

#"elevel" attribute -> factor datatype
Complete_Responses_DS$level <- as.factor(Complete_Responses_DS$level)
class(level)

#"car" attribute -> Character datatype
Complete_Responses_DS$car_character <- as.character(Complete_Responses_DS$car)
class(car_character)

#"zipcode" attribute -> Character datatype
Complete_Responses_DS$zipcode_character <- as.character(Complete_Responses_DS$zipcode)
class(car_character)

# “brand” -> Logical datatype
Complete_Responses_DS$brand_logical <- as.logical(Complete_Responses_DS$brand)
class(brand_logical)

#Summary of the all the data types available in Complete Responses
summary(Complete_Responses_DS)

# Checking if the all the data types has been converted to Factor, Character and Logical operator
str(Complete_Responses_DS)

#To check if there is any MISSING DATA in the data set.
summary(is.na(Complete_Responses_DS))

View(Complete_Responses_DS)
#To check if no special character is available in the data set.
gsub("[^[:alnum:]///' ]", "", Complete_Responses_DS)

# Checking for outliers using the boxplot graphs for individual attributes vs observation
# Salary
boxplot((Complete_Responses_DS$salary), outline = TRUE)
#Age
boxplot((Complete_Responses_DS$age), outline = TRUE)
#Credit
boxplot((Complete_Responses_DS$credit), outline = TRUE)
#brand_logical
boxplot((Complete_Responses_DS$brand_logical), outline = TRUE)

# Renaming the value of TRUE -> SONY and FALSE -> ACER 

Complete_Responses_DS$brand_logical[Complete_Responses_DS$brand_logical=="FALSE"] <- "ACER"

Complete_Responses_DS$brand_logical[Complete_Responses_DS$brand_logical=="TRUE"] <- "SONY"

#Comparing the values of all the 6 attributes vs the dependent attributes i.e. Brand

# Histogram and plot for the non-numeric values of Dataset
hist(Complete_Responses_DS$salary, breaks = 5)

# Plot for checking the level attribute
plot(Complete_Responses_DS$level)

# Plot for checking the CAR attribute
plot(Complete_Responses_DS$car_character)

# Plot for checking the Zip code attribute
plot(Complete_Responses_DS$zipcode_character)

# Plot for checking the Credit attribute
hist(Complete_Responses_DS$credit)

# GGPlot
# GGplot comparing the age 'age' attribute with 'brand_logical' 
ggplot(data = Complete_Responses_DS, aes(x= brand_logical)) +
  geom_bar(stat = "count", aes(fill=brand_logical)) +
  facet_wrap(~age)

# GGplot comparing the age 'car' attribute with 'brand_logical' 
ggplot(data = Complete_Responses_DS, aes( x=car_character)) +
  geom_bar( stat = "count", aes(fill= brand_logical)) +
  facet_wrap(~brand_logical)

# GGplot comparing the age 'Zip Code' attribute with 'brand_logical'
ggplot(data = Complete_Responses_DS, aes( x=zipcode_character)) +
  geom_bar( stat = "count", aes(fill= brand_logical)) +
  facet_wrap(~brand_logical)

# GGplot comparing the age 'Salary' attribute with 'brand_logical'
ggplot(data = Complete_Responses_DS, aes( x=salary)) +
  geom_bar( stat = "count", aes(fill= brand_logical)) +
  facet_wrap(~brand_logical)

# GGplot comparing the age 'Credit' attribute with 'brand_logical'
ggplot(data = Complete_Responses_DS, aes( x=credit)) +
  geom_bar( stat = "count", aes(fill= brand_logical)) +
  facet_wrap(~brand_logical)

#Checking the individual attributes with the dependent variable using GG Plot(Geom jitter)
ggplot(data = Complete_Responses_DS ,mapping = aes(x = brand_logical, y = age) ) +
  geom_jitter( color = "blue")

ggplot(data = Complete_Responses_DS ,mapping = aes(x = level , y = brand_logical) ) +
  geom_jitter( color = "green")

ggplot(data = Complete_Responses_DS ,mapping = aes(x = car_character, y = brand_logical) ) +
  geom_jitter( color = "brown")

ggplot(data = Complete_Responses_DS ,mapping = aes(x = zipcode_character, y = brand_logical) ) +
  geom_jitter( color = "brown")

ggplot(data = Complete_Responses_DS ,mapping = aes(x = credit, y = brand_logical) ) +
  geom_jitter( color = "red")

ggplot(data = Complete_Responses_DS ,mapping = aes(x = salary, y = brand_logical) ) +
  geom_jitter( color = "red")

ggplot(data = Complete_Responses_DS ,mapping = aes(x = salary, y = age, color = brand_logical) ) +
  geom_jitter()

ggplot(data = Complete_Responses_DS ,mapping = aes(x = salary, y = age, color = brand_logical) ) +
  geom_jitter()

# Using facets by wrap
ggplot (Complete_Responses_DS, aes(x= salary, y = age)) +
  geom_jitter() +
  facet_wrap(facets = vars(brand_logical))

# Using facets by vars 
ggplot(data = Complete_Responses_DS ,mapping = aes(x = salary, y = age, color = brand_logical)) +
  geom_line() +
  facet_wrap(facets =  vars(brand_logical))

# Using ggplots, facet_wrap and theme
ggplot(data = Complete_Responses_DS ,mapping = aes(x = salary, y = age, color = brand_logical)) +
  geom_line() +
  facet_wrap(vars(brand_logical)) +
  theme_bw()

# Using ggplots, facet_wrap, labs and theme
ggplot(data = Complete_Responses_DS ,mapping = aes(x = salary, y = age, color = brand_logical)) +
  geom_line() +
  facet_wrap(vars(brand_logical)) +
  labs(title = "Brand Choosen by Customer",
       x = "Salary of the Customer",
       y = "Age of the Customer") +
  theme_bw()

#Using theme with better labels 
ggplot(data = Complete_Responses_DS ,mapping = aes(x = salary, y = age, color = brand_logical)) +
  geom_line() +
  facet_wrap(vars(brand_logical)) +
  labs(title = "Brand Choosen by Customer",
       x = "Salary of the Customer",
       y = "Age of the Customer") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))


# ggplot (Complete_Responses_DS, aes(x = salary, y =  age)) +
#   stat_density_2d(geom = "point", aes(size = brand_logical), n = NA, contour = FALSE ) +
#   scale_size(range (0, 10))

# -----------------------------------------------------------------------------------------------------------------------------
#Random Forrest Automatic Grid and TuneLength = 1 and 2
#create 10000 observation to  be sampled in dataset. Training Set = 75 % and Testing Set = 25 %

# Setting for an random number
set.seed(998)

#Automation Gird
# define an 75%/25% train/test split of the dataset

Complete_Responses_Partition <- createDataPartition(Complete_Responses_DS$brand_logical, p=.75, list = FALSE)

str(Complete_Responses_Partition)

CR_Training <- Complete_Responses_DS[Complete_Responses_Partition,]

CR_Testing <- Complete_Responses_DS[- Complete_Responses_Partition,]

nrow(CR_Training)

nrow(CR_testing)

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest Regression model with a tuneLenght = 2 (trains with 2 mtry value for RandomForest)
rfFit1 <- train(brand_logical~., data = CR_Training, method = "rf", trControl=fitControl, tuneLength = 2)

#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rfFit1 <- train(brand_logical~., data = CR_Training, method = "rf", trControl=fitControl, tuneLength = 1)

#training results
rfFit1

plot(rfFit1)

# -----------------------------------------------------------------------------------------------------------------------------

# Random Forrest - Manual Grid
#create 10000 observation to  be sampled in dataset. Training Set = 75 % and Testing Set = 25 %

# Setting for an random number
set.seed(998)

#Automation Gird
# define an 75%/25% train/test split of the dataset

Complete_Responses_Partition_Manual <- createDataPartition(Complete_Responses_DS$brand_logical, p=.75, list = FALSE)

str(Complete_Responses_Partition_Manual)

CR_Training_Manual <- Complete_Responses_DS[Complete_Responses_Partition_Manual,]

CR_Testing_Manual <- Complete_Responses_DS[- Complete_Responses_Partition_Manual,]

nrow(CR_Training_Manual)

nrow(CR_Testing_Manual)

#10 fold cross validation
fitControl_M <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3))


#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- train(brand_logical~., data = CR_Training_Manual, method = "rf", trControl=fitControl_M, tuneGrid=rfGrid))

#train Random Forest Regression model with a tuneLenght = 2 (trains with 2 mtry value for RandomForest)
rfGrid <- train(brand_logical~., data = CR_Training_Manual, method = "rf", trControl=fitControl_M, tuneLength = 1)

#training results
rfFitm1



# -----------------------------------------------------------------------------------------------------------------------------

# C5.0 Model - Automotive Grid
#create 10000 observation to  be sampled in dataset. Training Set = 75 % and Testing Set = 25 %
#Calling the CARET library
library(C50)
# Setting for an random number
set.seed(998)

#Automation Gird
# define an 75%/25% train/test split of the dataset

Complete_Responses_Partition_C5 <- createDataPartition(Complete_Responses_DS$brand_logical, p=.75, list = FALSE)

str(Complete_Responses_Partition_C5)

CR_Training_C5 <- Complete_Responses_DS[Complete_Responses_Partition_C5,]

CR_Testing_C5 <- Complete_Responses_DS[- Complete_Responses_Partition_C5,]

nrow(CR_Training_C5)

nrow(CR_Testing_C5)

#10 fold cross validation
C5fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train C5 model with a tuneLenght = 2 (trains with 2 mtry value for RandomForest)
C5Fit1 <- train(brand_logical~., data = CR_Training_C5, method = "C5.0", trControl=C5fitControl, tuneLength = 2)

#train C5 model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
# rfFit1 <- train(brand_logical~., data = CR_Training_C5, method = "rf", trControl=C5fitControl, tuneLength = 1)

#training results
C5Fit1

plot(C5Fit1)

# -----------------------------------------------------------------------------------------------------------------------------
# C5.0 Model - Manual Grid
#create 10000 observation to  be sampled in dataset. Training Set = 75 % and Testing Set = 25 %
#Calling the CARET library
library(C50)
# Setting for an random number
set.seed(998)

#Automation Gird
# define an 75%/25% train/test split of the dataset

CR_Partition_C5_Manual <- createDataPartition(Complete_Responses_DS$brand_logical, p=.75, list = FALSE)

str(CR_Partition_C5_Manual)

CR_Training_C5_Manual <- Complete_Responses_DS[CR_Partition_C5_Manual,]

CR_Testing_C5_Manual <- Complete_Responses_DS[- CR_Partition_C5_Manual,]

nrow(CR_Training_C5_Manual)

nrow(CR_Testing_C5_Manual)

#10 fold cross validation
C5fitControl_Manual <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


#dataframe for manual tuning of mtry
C5_Grid_M <- expand.grid(mtry=c(30:40))

#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- train(brand_logical~., data = CR_Training_C5_Manual, method = "rf", trControl=fitControl_M, tuneGrid=C5_Grid_M))

#train Random Forest Regression model with a tuneLenght = 2 (trains with 2 mtry value for RandomForest)
rfGrid <- train(brand_logical~., data = CR_Training_Manual, method = "rf", trControl=fitControl_M, tuneLength = 1)

#training results
rfFitm1
