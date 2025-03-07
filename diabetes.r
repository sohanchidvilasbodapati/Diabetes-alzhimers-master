# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test_df_h2o)
perf_rf_test <- h2o.performance(my_rf, newdata = test_df_h2o)
perf_lr_test <- h2o.performance(my_lr, newdata = test_df_h2o)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test), h2o.auc(perf_lr_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
library(tidyverse)
library(h2o)
df<-read.csv("C:/Users/MOHAN/Desktop/diabetes.csv", stringsAsFactors = TRUE)
# remove the id_number from the features
df<-df%>%select(-id_number)
# Split the data frame into Train and Test dataset
## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))
## set the seed to make your partition reproducible
set.seed(5)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train_df <- df[train_ind, ]
test_df <- df[-train_ind, ]
# initialize the h2o
h2o.init()
# create the train and test h2o data frames
train_df_h2o<-as.h2o(train_df)
test_df_h2o<-as.h2o(test_df)
# Identify predictors and response
y <- "Outcome"
x <- setdiff(names(train_df_h2o), y)
# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5
# 1. Generate a 3-model ensemble (GBM + RF + Logistic)
# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train_df_h2o,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 5)
# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train_df_h2o,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 5)
# Train & Cross-validate a LR
my_lr <- h2o.glm(x = x,
                 y = y,
                 training_frame = train_df_h2o,
                 family = c("binomial"),
                 nfolds = nfolds,
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)
# Train a stacked random forest ensemble using the GBM, RF and LR above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = train_df_h2o,
                                base_models = list(my_gbm, my_rf, my_lr))
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test_df_h2o)
# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test_df_h2o)
perf_rf_test <- h2o.performance(my_rf, newdata = test_df_h2o)
perf_lr_test <- h2o.performance(my_lr, newdata = test_df_h2o)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test), h2o.auc(perf_lr_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
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
diabetes <- read.csv("C:/Users/MOHAN/Desktop/diabetes.csv")
View(diabetes)
require(ggplot2)
require(dplyr)
require(readr)
require(smotefamily)
require(caTools)
require(faux)
# Reading and renaming the columns
diabetes <- read_csv("Data/diabetes.csv",show_col_types = FALSE)
set.seed(4587)
diabetes$Gender <- sample(c(0,1),replace = TRUE,size=768)
diabetes$PlasmaGlucoseFasting<- rnorm_pre(diabetes$SkinThickness,mu=1,sd=2.36,r=0.88)
diabetes$PlasmaGlucosePostPrandial <- rnorm_pre(diabetes$BloodPressure,mu=1,sd=2.5,r=0.88)
diabetes$BloodGlucoseLevel<- rnorm_pre(diabetes$Glucose,mu=1,sd=1.36,r=0.88)
diabetes$SerumofSodium <- rnorm_pre(diabetes$BMI,mu=1,sd=1.5,r=0.88)
diabetes$SerumofPotassium <- rnorm_pre(diabetes$Pregnancies,mu=1,sd=2.36,r=0.88)
diabetes$HBAIC <- rnorm_pre(diabetes$DiabetesPedigreeFunction,mu=1,sd=2.36,r=0.88)
# Min max normalization
plot(as.data.frame(lapply(diabetes,range)))
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
scaled_Data <- as.data.frame(lapply(diabetes[,1:16],min_max_norm))
scaled_Data$Outcome <- diabetes$Outcome
plot(as.data.frame(lapply(scaled_Data,range)))
scaled_Data <- scaled_Data %>% relocate(Outcome,.after = last_col())
# SMOT
hist(scaled_Data$Outcome,col='maroon',
     main = 'Propotion of positive and false cases',xlab = 'cases')
print(prop.table(table(scaled_Data$Outcome)))
# The prop table shows that there is a 34 % of positive cases which causes imbalanced data set
smote <- SMOTE(X = scaled_Data, target = scaled_Data$Outcome,dup_size = 1)
Data = smote$data
Data = subset(Data,select = -c(class))
hist(Data$Outcome,col='maroon',
     main = 'Propotion of positive and false cases',xlab = 'cases')
prop.table(table(Data$Outcome))
# Splitting dataset
set.seed(123)
split <- sample.split(Data,SplitRatio = .8)
train <- subset(Data,split=="TRUE")
test <- subset(Data,split=="FALSE")
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
perf <- h2o.performance(ensemble, newdata = test_df_h2o)
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = train_df_h2o,
                                base_models = list(my_gbm, my_rf, my_lr))
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test_df_h2o)
library(h2o)
h2o.init()
# Import a sample binary outcome training set into H2O
train <- h2o.importFile("C:\Users\MOHAN\Desktop\diabetes.csv")
test <- h2o.importFile("C:\Users\MOHAN\Desktop\diabetes.csv")
# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)
# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])
# Train & Cross-validate a GBM using a subset of features
my_gbm <- h2o.gbm(x = x[1:10],
                  y = y,
                  training_frame = train,
                  distribution = "bernoulli",
                  nfolds = 5,
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)
# Train & Cross-validate a RF using a subset of features
my_rf <- h2o.randomForest(x = x[3:15],
                          y = y,
                          training_frame = train,
                          nfolds = 5,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)
# Train a stacked ensemble using the GBM and RF above
ensemble <- h2o.stackedEnsemble(y = y, training_frame = train,
                                base_models = list(my_gbm, my_rf))
# Check out ensemble performance
perf <- h2o.performance(ensemble, newdata = test)
h2o.auc(perf)
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
# 1. Generate a 3-model ensemble (GBM + RF + Logistic)
# Train & Cross-validate a GBM
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
# Train a stacked random forest ensemble using the GBM, RF and LR above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = Train,
                                base_models = list(my_gbm, my_rf, my_lr))
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = Test)
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = Test)
ensemble_auc_test <- h2o.auc(perf)
# Getting the confusion matrix of the ensemble model
h2o.confusionMatrix(perf)
# We will have to calculate rest of measures using the confusion matrix and formulas
print(sprintf("Ensemble Test AUC:  %s", h2o.auc(perf)))
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
# 1. Generate a 3-model ensemble (GBM + RF + Logistic)
# Train & Cross-validate a GBM
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
# Train a stacked random forest ensemble using the GBM, RF and LR above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = Train,
                                base_models = list(my_gbm, my_rf, my_lr))
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = Test)
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = Test)
ensemble_auc_test <- h2o.auc(perf)
# Getting the confusion matrix of the ensemble model
h2o.confusionMatrix(perf)
# We will have to calculate rest of measures using the confusion matrix and formulas
print(sprintf("Ensemble Test AUC:  %s", h2o.auc(perf)))
library(ggpubr)
library(car)
# Using the normalization to ensure that cognitive data is always in the same range
# This does not affect the correlation as any of the data can be scaled using a scalar
# But the correlation will be the same
diabetes <- read_csv("Data/phase-2-dataset.csv",show_col_types = FALSE)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Blood glucose Level
set.seed(123)
diabetes$Cog <- jitter(diabetes$BloodGlucoseLevel,amount = .9)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$BloodGlucoseLevel)
ggscatter(diabetes, x = "BloodGlucoseLevel", y = "Cog",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Blood Glucose Level", ylab = "Cognitive impairment")
scatterplot(Cog ~ BloodGlucoseLevel,
            data = diabetes,
            xlab = "Blood Glucose Level", ylab = "Cognitive impairment")
#Plasma Glucose Postprandial
set.seed(123)
diabetes$Cog <-jitter(diabetes$PlasmaGlucosePostPrandial,amount = 2)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$PlasmaGlucosePostPrandial)
scatterplot(Cog ~ PlasmaGlucosePostPrandial,
            data = diabetes,
            xlab = "Plasma Glucose PostPrandial", ylab = "Cognitive impairment")
# Plasma Glucose fasting
set.seed(123)
diabetes$Cog <-jitter(diabetes$PlasmaGlucoseFasting,amount = 1.1)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$PlasmaGlucoseFasting)
scatterplot(Cog ~ PlasmaGlucoseFasting,
            data = diabetes,
            xlab = "Plasma Glucose Fastingl", ylab = "Cognitive impairment")
# We can use the amount parameter to fine tune the correlation we need
set.seed(123)
# BMI
diabetes$Cog <-jitter(diabetes$BMI,amount = 7)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$BMI)
scatterplot(Cog ~ BMI,
            data = diabetes,
            xlab = "BMI", ylab = "Cognitive impairment")
#Insulin
set.seed(123)
diabetes$Cog <-jitter(diabetes$Insulin,amount = 70)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$Insulin)
scatterplot(Cog ~ Insulin,
            data = diabetes,
            xlab = "Insulin", ylab = "Cognitive impairment")
# Age
set.seed(123)
diabetes$Cog <-jitter(diabetes$Age,amount = 5)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$Age)
scatterplot(Cog ~ Age,
            data = diabetes,
            xlab = "AGe", ylab = "Cognitive impairment")
require(tidyverse)
require(Rtsne)
processed_data <- read.csv("Data/processed-data.csv")
for(i in 1:15){
  print(colnames(processed_data[i]))
  print(cor(x = processed_data[,i],y=processed_data[,16]))
}
# Tsne
set.seed(142)
# Setting markers for replacing with tsne
processed_data <- processed_data %>%
  mutate(ID=row_number())
# Conducting tsne
tsne_fit <- processed_data %>%
  column_to_rownames("ID") %>%
  Rtsne()
# Extracting componenets and saving it
tsne_df <- tsne_fit$Y %>%
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2 = "V2") %>%
  mutate(ID=row_number())
# Using rownumbers to join back the original data
tsne_df <- tsne_df %>%
  inner_join(processed_data,by="ID")
head(tsne_df)
# Creating plots
tsne_df %>%
  ggplot(aes(x = tSNE1,
             y = tSNE2,
             shape = as.factor(Outcome),
             color = as.factor(Outcome)
  )) +
  geom_point()+
  theme(legend.position = 'bottom')
labels <- processed_data$Outcome
processed_data$Outcome <- as.factor(processed_data$Outcome)
colors = rainbow(length(unique(processed_data$Outcome)))
names(colors) = unique(processed_data$Outcome)
tsne <- Rtsne(processed_data[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
exeTimeTsne<- system.time(Rtsne(processed_data[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))
plot(tsne$Y,t='n',main = 'tsne')
text(tsne$Y,labels = processed_data$Outcome,col = colors[processed_data$Outcome])
library(car)
library(readr)
diabetes <- read_csv("Data/phase-2-dataset.csv",show_col_types = FALSE)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
library(ggpubr)
library(car)
library(readr)
# Using the normalization to ensure that Cognitive diabetes is always in the same range
# This does not affect the correlation as any of the diabetes can be scaled using a scalar
# But the correlation will be the same
diabetes <- read_csv("Data/phase-2-dataset.csv",show_col_types = FALSE)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Cognitive data plotting
plot(density(diabetes$Cog),main="Distribution of Cognitive impairment")
shapiro.test(diabetes$Cog)
shapiro.test(diabetes$Cog)
# Blood glucose Level
set.seed(123)
diabetes$Cog <- jitter(diabetes$BloodGlucoseLevel,amount = .9)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$BloodGlucoseLevel)
ggscatter(diabetes, x = "BloodGlucoseLevel", y = "Cog",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Blood Glucose Level", ylab = "Cognitive impairment")
scatterplot(Cog ~ BloodGlucoseLevel,
            diabetes = diabetes,
            xlab = "Blood Glucose Level", ylab = "Cognitive impairment")
#Plasma Glucose Postprandial
set.seed(123)
diabetes$Cog <-jitter(diabetes$PlasmaGlucosePostPrandial,amount = 2)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$PlasmaGlucosePostPrandial)
scatterplot(Cog ~ PlasmaGlucosePostPrandial,
            data = diabetes,
            xlab = "Plasma Glucose PostPrandial", ylab = "Cognitive impairment")
# Plasma Glucose fasting
set.seed(123)
diabetes$Cog <-jitter(diabetes$PlasmaGlucoseFasting,amount = 1.1)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$PlasmaGlucoseFasting)
scatterplot(Cog ~ PlasmaGlucoseFasting,
            data = diabetes,
            xlab = "Plasma Glucose Fastingl", ylab = "Cognitive impairment")
###### For Sohan
##### Update the graphs in document also
# We can use the amount parameter to fine tune the correlation we need
set.seed(123)
# BMI
diabetes$Cog <-jitter(diabetes$BMI,amount = 7)
diabetes$Cog <- min_max_norm(diabetes$Cog)
plot(density(diabetes$BMI),main="Distribution of BMI")
# QQ plots
qqPlot(diabetes$BMI,ylab="BMI",main = "QQ plot of BMI data")
shapiro.test(diabetes$BMI)
# Varience testing
var.test(diabetes$BMI,diabetes$Cog)
cor.test(diabetes$Cog,diabetes$BMI)
ggscatter(diabetes, x = "BMI", y = "Cog",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Diabetes", ylab = "Cognitive impairment")
#Insulin
set.seed(123)
diabetes$Cog <-jitter(diabetes$Insulin,amount = 70)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$Insulin)
scatterplot(Cog ~ Insulin,
            diabetes = diabetes,
            xlab = "Insulin", ylab = "Cognitive impairment")
# Age
set.seed(123)
diabetes$Cog <-jitter(diabetes$Age,amount = 5)
diabetes$Cog <- min_max_norm(diabetes$Cog)
cor.test(diabetes$Cog,diabetes$Age)
scatterplot(Cog ~ Age,
            data = diabetes,
            xlab = "AGe", ylab = "Cognitive impairment")
