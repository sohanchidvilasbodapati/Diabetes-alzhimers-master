# This module is to investigate the connection between diabetes and cognitive impariment
require(car)
require(ggpubr)

# Density plot
d <- density(data$Dia)
plot(d,main="Distribution of diabetes")
plot(density(data$cog),main="Distribution of cognitive impairment  ")

# QQ plots
qqPlot(data$Dia,ylab="Diabetes")
qqPlot(data$cog,ylab = "Cognitive impairment ")

shapiro.test(data$Dia)
shapiro.test(data$cog)

# Varience testing
boxplot(data$Dia)
boxplot(data$cog)
var.test(data$Dia,data$cog)

# Correlation
cor.test(x=data$Dia,y=data$cog,method="pearson")

# Linear regression
lm(data$Dia~data$cog)
ggscatter(data, x = "Dia", y = "cog", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Diabetes", ylab = "Cognitive impairment")
scatterplot(cog ~ Dia, 
            data = data, 
            xlab = "Diabetes", ylab = "Cognitive impairment")