# Class Lecture 2 - 09/06/2023

# Packages

library(readr)
library(sandwich)
library(visreg)


# Importing the dataset

#real_estate = read.csv("c:/real_estate.csv")

# Attaching the dataset

attach(real_estate)

names(real_estate)

# Summary statistics
summary(price)
summary(bedrooms)

# Boxplot
boxplot(price)
boxplot(bedrooms)

# Histogram
hist(price)
hist(bedrooms)

#Scatterplot
plot(bedrooms, price)

# OLS model

lm.fit = lm(price~bedrooms)

abline(lm.fit, col = "red")

b0 = coef(lm.fit)[1]
b1 = coef(lm.fit)[2]

b0 + 2*b1
b0 + 3*b1 #Prediction for price for a 3 bedroom apartment

summary(lm.fit)

#Confidence interval (upper and lower 5%)

confint(lm.fit, 'bedrooms', level = 0.90)

confint(lm.fit, 'bedrooms', level = 0.95)

confint(lm.fit, 'bedrooms', level = 0.99)

par(mfrow = c(2,3))

visreg(lm.fit, alpha = 0.05)  ## alpha is 1 - CI
visreg(lm.fit, alpha = 0.04)
visreg(lm.fit, alpha = 0.03)   ## CI is 97%, 

# The more confident we are, the wider the range is going to be

