############################################################################
# Author: Xuan Wu                   Date: 11/13/2019
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
car.df <- car.df[1:1000, ]
car.df <- cbind(car.df, dummy(car.df$Fuel_Type, sep = "_"))

select.var <- c(3, 4, 7, 9, 12, 14, 17, 19, 21, 25, 26, 28, 30, 34, 39, 42, 43)

set.seed(1)   #set seed for testing
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, select.var]
valid.df <- car.df[-train.index, select.var]

train.norm.df <- train.df
valid.norm.df <- valid.df

norm.values <- preProcess(train.df[, c(1,2,3,4,6,7,9)], method=c("center", "scale"))

train.norm.df[, c(1,2,3,4,6,7,9)] <- predict(norm.values, train.df[, c(1,2,3,4,6,7,9)])
valid.norm.df[, c(1,2,3,4,6,7,9)] <- predict(norm.values, valid.df[, c(1,2,3,4,6,7,9)])

