library(h2o)

# initialize the h2o
h2o.init()

# Identify predictors and response
y <- "Outcome"
x <- setdiff(names(train), y)

# Converting outcome to factor to ensure models do classification
train$Outcome <- as.factor(train$Outcome)
Train <- as.h2o(train)
test$Outcome <- as.factor(test$Outcome)
Test <- as.h2o(test)


# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5


# 1. Generate a 3-model ensemble (my_gbm + RF + Logistic)
# Train & Cross-validate a my_gbm
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = Train,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 5)


# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = Train,
                          nfolds = nfolds,
         
                        keep_cross_validation_predictions = TRUE,
                          seed = 5)


# Train & Cross-validate a LR
my_lr <- h2o.glm(x = x,
                 y = y,
                 training_frame = Train,
                 family = c("binomial"),
                 nfolds = nfolds,
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)


# Train a stacked random forest ensemble using the my_gbm, RF and LR above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = Train,
                                base_models = list(my_my_gbm, my_rf, my_lr),
                   )



# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = Test)


# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = Test)

ensemble_auc_test <- h2o.auc(perf)

# Getting the confusion matrix of the ensemble model
h2o.confusionMatrix(perf)

# We will have to calculate rest of measures using the confusion matrix and formulas
print(sprintf("Ensemble Test AUC:  %s", h2o.auc(perf)))

# Graphs
plot(my_gbm)
plot(my_lr)
plot(my_rf)
h2o.learning_curve_plot(ensemble)
h2o.partialPlot(ensemble,data = Test,
                cols =setdiff(colnames(test),'Outcome'))
test$Insulin
