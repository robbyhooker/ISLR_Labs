library(MASS)
library(ISLR2)

# 3.6.2 Simple Linear Regression

head(Boston)
boston <- Boston
attach(boston)
View(boston)

lm.fit <- lm(medv ~ lstat)

lm.fit

summary(lm.fit)

names(lm.fit)

confint(lm.fit)

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), 
        interval = "confidence")

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), 
        interval = "prediction")

plot(lstat, medv, pch=10, col=3)
abline(reg = lm.fit, col=4, lwd=3)

par(mfrow = c(1, 1))
plot(lm.fit)
print(predict(lm.fit))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


# 3.63 Multiple Linear Regression

lm.fit <- lm(medv ~ lstat + age)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data=boston)
summary(lm.fit)

library(car)
vif(lm.fit)

lm.fit1 <- update(lm.fit, ~ . - age)
summary(lm.fit1)

# 3.6.4 Interaction Terms

summary(lm(medv ~ lstat * age, data=boston))

# 3.6.5 Non-linear Transformation of the Predictors

lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

lm.log <- lm(medv ~ log(rm), data=boston)
summary(lm.log)

# 3.6.6 Qualitative Predictors

carseats <- Carseats
head(carseats)
attach(carseats)
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data=carseats)
summary(lm.fit)
contrasts(US)

# 3.7.7 Writing Functions

addInts <- function(a, b){
  return (a+b)
}

result <- addInts(3, 5)
print(result)

LoadLibraries <- function (){
  library(ISLR2)
  library(MASS)
  print("Libraries Loaded")
}
LoadLibraries()

















