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



###### For Vishal 
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




