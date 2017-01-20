install.packages("ISLR")

library(MASS)
library(ISLR)

## Simple Linear Regression

names(Boston)
?Boston

# Scatter plot
plot(medv~lstat,Boston)

# Fitting a model
fit1 = lm(medv~lstat, data=Boston)
fit1

summary(fit1)

# Showing a line which was used to fit
abline(fit1, col='red')
names(fit1)

# Confidence coefficient
confint(fit1)

# Predicting the value
predict(fit1, data.frame(lstat=c(5, 10, 15)), interval = "confidence")

## Multiple linear regression

fit2 = lm(medv~lstat+age, data=Boston)
summary(fit2)

fit3 = lm(medv~., Boston)
summary(fit3)

par(mfrow=c(2, 2))
plot(fit3)

# Removing age and indus
fit4 = update(fit3, ~.-age-indus)
summary(fit4)

# Nonlinear terms and Interactions
fit5 = lm(medv~lstat*age, Boston)
summary(fit5)

fit6 = lm(medv~lstat + I(lstat^2), Boston)
summary(fit6)

# Global access to the variables
attach(Boston)

par(mfrow=c(1, 1))
plot(medv~lstat)
points(lstat, fitted(fit6), col="red", pch=20)

# Fourth degree polynomial
fit7 = lm(medv~poly(lstat, 4))
points(lstat, fitted(fit7), col="blue", pch=20)

# Plotting characteristics
plot(1:20, 1:20, pch=1:20, cex=2)

#### Qualitative Predictors
## Another dataset

# Data Editor
fix(Carseats)
names(Carseats)
summary(Carseats)

fit1 = lm(Sales ~. + Income:Advertising+Age:Price, Carseats)

summary(fit1)

contrasts(Carseats$ShelveLoc)

## Writing R Functions
regplot = function(x, y) {
	fit = lm(y~x)
	plot(x,y)
	abline(fit, col="red")
}

attach(Carseats)

regplot(Price, Sales)

regplot = function(x, y, ...){
	fit = lm(y~x)
	plot(x, y, ...)
	abline(fit, col="red")
}

regplot(Price, Sales, xlab="Price", ylab="Sales", col="blue", pch=20)
