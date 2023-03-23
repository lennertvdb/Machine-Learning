# Deployment_and_metrics.R
#
#

# import libraries
library(caret) #confusion matrix and basic measures
library(ROCR)  # ROC curve 
library(ggplot2)

### Read the best models
# logistic regression
data <- read.csv("./../data/results/lr_prop_validation.csv")

# random forrest
#

# lgb
data <- read.csv("./../data/results/lgb2_prop_validation.csv")
label_true <- data$val.default
prob_estimated <- data$preds

## ROC curve
prediction <- prediction(predictions = prob_estimated,labels = label_true)
perf <- performance(prediction,"tpr","fpr")

# Plot
plot(perf,lwd=2,col='steelblue1',main="ROC curve - lgb" )
#abline(a=0, b= 1,col = "lightgrey")

lines(c(0,1),c(0,1), col = "lightgrey")


test <- performance(prediction,"auc")


########### When we set threshold manually #############
# get the estimated label
treshold = 0.05
label_estimated = ifelse(prob_estimated > treshold,1,0)

# convert labels to factors
label_true <- as.factor(label_true)
label_estimated <- as.factor(label_estimated)

# Confusion matrix and basic statistics
confusionMatrix(data = label_estimated,reference = label_true)

