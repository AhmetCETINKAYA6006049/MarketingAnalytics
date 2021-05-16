install.packages("arules")

library(arules)
library(psych)


# In our dataset each row is a transaction

# If a matrix consists of many zeros then this is called a sparse matrix

# read.transactions() function creates a sparse matrix for transactional data 
# It has 169 unique items 
groceries <- read.transactions("groceries.csv", sep = ",")

# density is the number of cells that is not zero in the matrix
# 9835*169*0.02609146 = 43367 items were purchased
summary(groceries)

# to look at the contents of the sparse matrix we use inspect()
inspect(groceries[1:5])

# itemfrequency() allows us to see the proportion of transactions that contains the item.

# to view the support level for the first 3 alphabetical items in the grocery data:
# abrasive cleaner and artificial sweeteners are found in about 0.3% of the transactions 
# while baby cosmetics are found in about 0.06% of the transactions.
itemFrequency(groceries[,1:3])


# itemFrequencyPlot() function allows us to produce a bar chart depicting the proportion of transactions.
itemFrequencyPlot(groceries, support = 0.1)

# to show the top N products purchased
itemFrequencyPlot(groceries, topN = 20)

# use the apriori with default support = 0.1 and confidence = 0.8
rules <- apriori(groceries)
summary(rules)

# We didnt get any results we must define a lowe suport level and lower confidence
# minlen shows the mininum length of together bought items (frequent itemset)

groceryRules  <- apriori(groceries, parameter = list( support = 0.006, confidence = 0.25, minlen =2))
summary(groceryRules)


#lift is important, higher is more common purchased
inspect(groceryRules[1:10])

# lift is important, coverage is  the proportion of :  lefthand side itemset / total transactions
inspect(sort(groceryRules, by = "lift")[1:5])

# rules of a product is gathered by subset() function
berryRules <- subset(groceryRules, items %in% "berries")

inspect(berryRules)


