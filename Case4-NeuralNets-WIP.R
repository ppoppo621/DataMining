############################################################################
# Author: Xuan Wu                   Date: 2/2/2020
############################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Case 4 - Car Sales; Neoral Network Model
# a. Fit a neural network model to the data. Use a single hidden layer with 2 nodes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

install.packages("caret")
install.packages("neuralnet")
install.packages("dummies")

library(caret)
library(dummies)

# load file and data clean
car.df <- read.csv("ToyotaCorolla.csv")

# Change the categorical variable to dummy variable
car.df <- car.df[1:1000, ]
car.df <- cbind(car.df, dummy(car.df$Fuel_Type, sep = "_"))

# Pick the needed variables in dataset
select.var <- c(3, 4, 7, 9, 12, 14, 17, 19, 21, 25, 26, 28, 30, 34, 39, 42, 43)

# Sampling the data and partition into training data and validation data
set.seed(1)   #set seed for testing
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, select.var]
valid.df <- car.df[-train.index, select.var]

# Normalized data with preprocess()
train.norm.df <- train.df
valid.norm.df <- valid.df

norm.values <- preProcess(train.df[, c(1,2,3,4,6,7,9)], method=c("center", "scale"))

train.norm.df[, c(1,2,3,4,6,7,9)] <- predict(norm.values, train.df[, c(1,2,3,4,6,7,9)])
valid.norm.df[, c(1,2,3,4,6,7,9)] <- predict(norm.values, valid.df[, c(1,2,3,4,6,7,9)])

# Apply with neural network model with training data with 2 hidden nodes

# Compute the predicted price with training data

# Compute the original price range in original training data -- [a, b]

# Covert the predicted price to its original unit by multiplied the output by (b-a) and add a.

# Validate price with validation data

# Compute the original price range in original training data -- [a, b]

# Covert the validated price to its original unit by multiplied the output by (b-a) and add a.

# changing the number of hidden layers and nodes to {single layer with 5 nodes}, {two layers, 5 nodes in each layer}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# i. What happens to the RMS error for the training data as the number of layers and nodes increases?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Answer: RMS error will decrease as the number of layers and nodes increases.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ii. What happens to the RMS error for the training data as the number of layers and nodes increases?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Answer: RMS will decrease first then increase fot the validation data.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# iii. Comment on the appropriate number of layers and nodes for this application.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Answer: The approprite number of layers nodes should be the number when validation error is at minimal point.
