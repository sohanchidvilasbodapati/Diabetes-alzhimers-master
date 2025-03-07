require(party)
require(ROCR)
require(caret)
set.seed(123)

# Building the tree
tree <- ctree(Outcome ~., data = train)
summary(tree)

# Predicting
predict_reg <- predict(tree, 
                       test, type = "response")
predict_reg  
predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy

# using confusion matrix
confusion_matrix_decision_tree <- confusionMatrix(as.factor(predict_reg),as.factor(test$Outcome),mode = "everything",positive = "1")

missing_classerr <- mean(predict_reg != test$Outcome)
print(paste('Accuracy =', 1 - missing_classerr))

# Using ROC curve

# ROC-AUC Curve
ROCPred <- prediction(predict_reg, test$Outcome) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

# Plotting
plot(tree)

