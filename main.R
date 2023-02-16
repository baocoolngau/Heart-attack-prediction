
library(tidyverse)
library(ISLR)
library(RColorBrewer)
library('caTools')
library('kernlab')
library(ggplot2)
library(caTools)
library(e1071)
library(caret)

dataset = read.csv("training_dataset/heart.csv")

dataset$output = factor(dataset$output, levels = c(0, 1))

set.seed(123)
split = sample.split(dataset$output, SplitRatio = 0.8)

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

train_model = svm(formula = output ~ .,
                 data = training_set,
                 kernel = "linear",
                 type = "C-classification")
summary(train_model)

output_pred = predict(train_model, newdata = test_set)

# print(test_result)
test_result = data.frame(predict=output_pred, actual=test_set$output)
print(test_result)

# Making the Confusion Matrix
cm = table(output_pred, test_set$output)
confusionMatrix(cm, mode="everything")

#create ROC plot
ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))

plot.roc(rocobj, print.auc=TRUE, auc.polygon=TRUE, auc.polygon.col=rgb(.3,0,.8,0.2), print.thres=TRUE)
plot.roc(smooth(rocobj), add=TRUE, col=2)
