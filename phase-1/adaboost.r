require(adabag)
require(ROCR)
require(tree)

set.seed(123)
Train = train
Train$Outcome = as.factor(Train$Outcome)
cntrl <- rpart.control(maxdepth = 17)
ada = boosting(Outcome ~.,data = Train,mfinal=1000,control = cntrl)

summary(ada)
ada$trees
ada$weights
ada$importance


# Predicting
Predic <- predict(ada,test,type='response')
predict_reg  = as.double(Predic$class)

# Evaluating model accuracy

# using confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predict_reg),as.factor(test$Outcome),mode = "everything",positive = "1")

missing_classerr <- mean(predict_reg != test$Outcome)
print(paste('Accuracy =', 1 - missing_classerr))

# Using ROC curve

# ROC-AUC Curve
ROCPred <- prediction((predict_reg), test$Outcome) 
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

# Plots
# importance plots
importanceplot(ada)
Trees <- ada$trees[[500]]
plot(Trees)
text(Trees,pretty=0)