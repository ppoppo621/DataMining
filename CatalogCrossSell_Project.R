####################################################################################################
# Catalog Sales Association Analysis (Market Busket Analysis)
# Author: Xuan(Zoe) Wu                Date: 12/04/2019
####################################################################################################

install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

ccs.df <- read.csv("CatalogCrossSell.csv")

# remove the columns and convert to matrix
ccs.mat <- as.matrix(ccs.df[, -c(1,11,12,13,14,15,16,17,18,19,20)])
print(ccs.mat)

# convert the binary incidence matrix into a transactions database
ccs.trans <- as(ccs.mat, "transactions")
inspect(ccs.trans)

## get rules
# when running aprior(), include the minimum support, minimum confidence, and target 
#as arguments
rules <- apriori(ccs.trans, parameter = list(supp = 0.1, conf = 0.5, target = "rules"))
summary(rules)
#inspect the first 10 rules, sorted by their lift
inspect(head(sort(rules, by = "lift"), n = 10))

# Visualize result
plot(rules, control=list(jitter=2), shading = "lift")
plot(sort(rules, by="lift")[1:10], method="grouped") 

housewares_n <- subset(rules, items %in% "Housewares.Division")
plot(sort(housewares_n, by="lift")[1:10], measure="confidence", method="graph", control=list(type="items"), shading = "lift")
