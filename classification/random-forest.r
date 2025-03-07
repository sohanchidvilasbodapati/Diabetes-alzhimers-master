require(randomForest)
require(caret)
require(ROCR)

set.seed(123)
Forest = randomForest(Outcome ~., data = train,ntree=100)


# Predict test data based on model
predict_reg <- predict(Forest, 
                       test, type = "response")
predict_reg  

# Changing probabilities
predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy

# using confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predict_reg),as.factor(test$Outcome),mode = "everything",positive = "1")

missing_classerr <- mean(predict_reg != test$Outcome)
print(paste('Accuracy =', 1 - missing_classerr))


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

auc <- round(0.7352, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)


# Plotting
plot(Forest)

