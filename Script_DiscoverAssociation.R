#install the arules and arules packages (to be used in )
# install.packages('arules')
# install.packages('arulesViz')
# install.packages("dplyr")
# Uploading the .csv file using read.transactions() as variables would be created if we use the Read.csv() function
library(arules)
library(arulesViz)
library(dplyr)

elec.Transactions <- read.transactions("ElectronidexTransactions2017.csv", sep=",", rm.duplicates=TRUE)
elec.Transactions
summary(elec.Transactions)

#Total number of cells in sparse matrix
# total number of rows * total number of columns
9835 * 125 # 1229375 cells

#Cells that are not empty (items that are purchased)
# total number of rows * total number of columns * total number of empty cells (density)
9835 * 125 * 0.03506172  # 43104 cells 

# You can view the transactions. Is there a way to see a certain # of transactions?
inspect (elec.Transactions) 
# Number of transactions.
length (elec.Transactions) # 
# Number of items per transaction
size (elec.Transactions) # 
# Lists the transactions by conversion (LIST must be capitalized)
LIST(elec.Transactions) # 
# To see the item labels
itemLabels(elec.Transactions)

# visualize the items within your dataset
# Items with a highest frequency(using support)
itemFrequencyPlot(elec.Transactions)

itemFrequencyPlot(elec.Transactions, support = 0.10)

.# Top 50 items customer purchased
itemFrequencyPlot(elec.Transactions, topN=50)

# top 100
itemFrequencyPlot(elec.Transactions, type = c("relative"),topN=100, popCol = "black", horiz = TRUE)

# visualize the transactions within your dataset 
# Using image function only
image(elec.Transactions) # too much details are provided in the plot as data set is large
# image(elec.Transactions', axes = FALSE, xlim[2])

image(sample(elec.Transactions, 125))

#Apriori Algorithm 
elec.Transaction.Apriori.1 <- apriori (elec.Transactions, 
                                     parameter = list(supp = 0.1, conf = 0.8))

elec.Transaction.Apriori.2 <- apriori (elec.Transactions, 
                                     parameter = list(supp = 0.01, conf = 0.3))

elec.Transaction.Apriori.3 <- apriori (elec.Transactions, 
                                     parameter = list(supp = 0.1, conf = 1, maxlength = 1))           


# Evaluation of the model
summary(elec.Transaction.Apriori)

# Inspect the rule name
inspect(sort(elec.Transaction.Apriori), by="lift")

# Improve your model
# Checking the confidence value with a decreasing values
inspect.confidence <- inspect(sort(elec.Transaction.Apriori, by = "confidence", decreasing=TRUE))
# inspect using support
inspect.support <- inspect(sort(elec.Transaction.Apriori, by = "support"))
# inspect using the lift
inspect.sort <- inspect(sort(elec.Transaction.Apriori, by = "lift"))

# Checking the summary for all the inspect value
summary(inspect.confidence)

summary(inspect.support)

summary(inspect.sort)

# Using item's rules you can use the subset () function

elec.Transaction.Apriori.4 <- apriori (elec.Transactions, 
                                       parameter = list(supp = 0.01, conf = 0.2))

is.redundant(elec.Transaction.Apriori.4)

itemRules.subset <- subset(elec.Transaction.Apriori.2, items %in% "Acer Aspire","HP Laptop")

summary(itemRules.subset)

# Visualize the results using plot
plot(itemRules.subset[1:123], method="graph", control=list(type="items")) 
# Iteration 2 -----------------------------------------------------------------------------------------------------------------------------

library(arulesViz)

# elec.Trans.Apriori.4 <- apriori (elec.Transactions, 
                                       # parameter = list(supp = 0.01, conf = 0.3), appearance = list(default="rhs", default ="iPad"))

elec.Trans.Apriori.4 <- apriori (elec.Transactions, parameter = list(supp = 0.01, conf = 0.03))
                                 
                                 
inspect(elec.Trans.Apriori.4[1:153])

summary()

ruleExplorer(elec.Trans.Apriori.4)

# Iteration 3 -----------------------------------------------------------------------------------------------------------------------------
# Visualize the support and confidence (Lower than the Iteration 2)
library(arulesViz)

# elec.Trans.Apriori.4 <- apriori (elec.Transactions, 
# parameter = list(supp = 0.01, conf = 0.3), appearance = list(default="rhs", default ="iPad"))

elec.Trans.Apriori.5 <- apriori (elec.Transactions, parameter = list(supp = 0.001, conf = 0.1))

inspect(elec.Trans.Apriori.5[1:100])

inspect(head(sort(elec.Trans.Apriori.5,by="lift"),10))

plot(elec.Trans.Apriori.5)

plot(elec.Trans.Apriori.5, method = "grouped")

summary()

ruleExplorer(elec.Trans.Apriori.5)

# Iteration 4 -Keeping the values of the Support, Confidence and the life values as highest--------------------------------------
# Highest values are based on the previous analysis (Iteration 3)

library(arulesViz)

elec.Trans.Apriori.6 <- apriori (elec.Transactions, parameter = list(supp = 0.076, conf = 0.602, lift = 3.360))

inspect(elec.Trans.Apriori.5[1:100])

inspect(head(sort(elec.Trans.Apriori.6 ,by="lift"),10))

plot(elec.Trans.Apriori.6 )

plot(elec.Trans.Apriori.6, method = "grouped")

summary()

ruleExplorer(elec.Trans.Apriori.5)

# Iteration 5 -- Keeping the values of the Support, Confidence and the life values as lowest--------------------------------------
# Lowest values are based on the previous analysis (Iteration 3)

library(arulesViz)

elec.Trans.Apriori.6 <- apriori (elec.Transactions, parameter = list(supp = 0.010, conf = 0.039, lift = 0.693))

inspect(elec.Trans.Apriori.5[1:100])

inspect(head(sort(elec.Trans.Apriori.5,by="lift"),10))

plot(elec.Trans.Apriori.5)

plot(elec.Trans.Apriori.5, method = "grouped")

summary()

ruleExplorer(elec.Trans.Apriori.5)





