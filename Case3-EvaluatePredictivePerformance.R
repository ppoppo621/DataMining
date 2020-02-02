############################################################################
# Author: Xuan Wu                    Date: 10/16/2019
############################################################################
# Case 3 - Evaluating Predictive Performance
# Set the working directory to appropriate folder on your machine, so as to access the data files.
# Load the libraries/packages.
install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)
install.packages("e1071")
library(e1071)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. A data mining routine has been applied to a transaction dataset and has classifed 88 
# records as fraudulent (30 correctly so) and 952 as non-fraudulent (920 correctly so). 
# Construct the confusion matrix and calculate the overall error rate.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# assign values to the data sets
lvs <- c('Fraud','Non-Fraud')
Actual <- factor(rep(lvs, times = c(62, 978)),
                levels = rev(lvs))
Pred <- factor(
  c(
    rep(lvs, times = c(30, 32)),
    rep(lvs, times = c(58, 920))),
  levels = rev(lvs))

#draw confusion matrix
confusionMatrix(Pred, Actual)
Overall_error_rate <- (32+58)/(88+952)
cat('Overall_error_rate:',Overall_error_rate)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1.a Produce the confusion matrix for the sample as it stands. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# assign values to the data sets
lvs <- c('Fraud','Non-Fraud')
insur_actual <- factor(rep(lvs, times = c(400, 400)),
                 levels = rev(lvs))
insur_pred <- factor(
  c(
    rep(lvs, times = c(310, 90)),
    rep(lvs, times = c(130, 270))),
  levels = rev(lvs))

#draw confusion matrix
confusionMatrix(insur_pred, insur_actual)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1.b Find the adjusted misclassifcation rate (adjusting for the oversampling).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Adjusting the Confusion Matrix for Oversampling with 1% response rate.
# Fruad: 1% of the whole data; 50% of the sample 
# Non-fraud: 99% of the whole data, 50% of the sample
# The actual number of fruad must be divided by 50, and the 
# actual number of non-fruad divided by 0.5050.
# Adjusted predict vs acutal is {310/50=6, 90/50=2, 130/0.505=257, 270/0.505=535}

lvs <- c('Fraud','Non-Fraud')
insur_actual <- factor(rep(lvs, times = c(8, 792)),
                       levels = rev(lvs))

insur_pred <- factor(
  c(
    rep(lvs, times = c(6, 2)),
    rep(lvs, times = c(257, 535))),
  levels = rev(lvs))

#draw confusion matrix
confusionMatrix(insur_pred, insur_actual)

#misclassifcation rate
misclassification_rate <- (2+257)/(800)
cat('misclassification_rate:',misclassification_rate)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1.c What percentage of new records would you expect to be classifed as fraudulent? 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pred_fraud_rate <- (257+6)/800
cat('The precentage of new records would be classified as fraudulent is:',pred_fraud_rate)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2.a Calculate error rates, sensitivity, and specifcity using cutofs of 0.25, 0.5, and 0.75.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# assign data
propensity <- c(0.03,0.52,0.38,0.82,0.33,0.42,0.55,0.59,0.09,0.21,0.43,0.04,0.08,0.13,0.01,0.79,0.42,0.29,0.08,0.02)
act <- c(0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0)
act_f <- factor(act)

# cutoff = 0.25
p1 <- factor(ifelse(propensity>0.25,1,0))
confusionMatrix(p1, act_f)

# Answer: For cutoff=0.25:
#   Error rate  : 0.4   
#   Sensitivity : 0.5294          
#   Specificity : 1.0000

# cutoff = 0.5
p2 <- factor(ifelse(propensity>0.5,1,0))
confusionMatrix(p2, act_f)

# Answer: For cutoff=0.5:
#   Error rate  : 0.1   
#   Sensitivity : 0.8824          
#   Specificity : 1.0000

# cutoff = 0.75
p3 <- factor(ifelse(propensity>0.75,1,0))
confusionMatrix(p3, act_f)

# Answer: For cutoff=0.75:
#   Error rate  : 0.05   
#   Sensitivity : 1.0000          
#   Specificity : 0.6667

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2.b Create a decile-wise lift chart in R. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
install.packages("gains")
library(gains) 

# use gains() to compute deciles.
gain <- gains(act, propensity,groups=10) 

barplot(gain$mean.resp / mean(act), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart") 
