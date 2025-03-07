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


