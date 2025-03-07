# This module is used to generate synthetic data for cognitive impairence

require(faux)
require(ggplot2)

# Generating a correlated data
summary(data$Dia)
data$cog <-rnorm_pre(data$Dia,mu=1,sd=2.36,r=0.88)

# Testing the correlation
cor(data$cog,data$Dia)
ggplot(data,aes(x= cog,y=Dia)) + 
  geom_point() +
  stat_smooth(method = 'lm',col='red')

# Alternative way of generating correalted data
data$cog <- jitter(data$Dia)
cor(data$cog,data$Dia)
plot(data$category,data$cog)
