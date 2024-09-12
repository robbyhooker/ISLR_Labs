#4.7.1 The Stock Market Data
library(ISLR2)
names(Smarket)
smarket <- Smarket
dim(smarket)
summary(smarket)

attach(smarket)
cor(smarket[, -9])

plot(Volume)

#4.7.2 Logistic Regression

names(smarket)

glm.fits <- glm(
  Direction ~ . - Year - Today, data=smarket, family=binomial
)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction)
mean(glm.pred == Direction)

train <- (Year < 2005)
smarket.2005 <- smarket[!train, ]
dim(smarket.2005)
direction.2005 <- Direction[!train]
print(direction.2005)

glm.fits <- glm(
  Direction ~ . - Year - Today, data = smarket, family=binomial, subset=train
)

glm.probs <- predict(glm.fits, smarket.2005, type="response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"

table(glm.pred, direction.2005)
mean(glm.pred == direction.2005)

glm.fits <- glm(Direction ~ Lag1 + Lag2, data=smarket, family=binomial, 
                subset=train)
glm.probs <- predict(glm.fits, smarket.2005, type="response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"

table(glm.pred, direction.2005)
mean(glm.pred == direction.2005)

# 4.7.3 Linear Discriminant Analysis

library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data=smarket, subset=train)
lda.fit

plot(lda.fit)

lda.pred <- predict(lda.fit, smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, direction.2005)
mean(lda.class == direction.2005)

# 4.7.4 Quadratic Discriminant Analysis

qda.fit <- qda(Direction ~ Lag1 + Lag2, data=smarket, subset=train)
qda.fit

qda.class <- predict(qda.fit, smarket.2005)$class
table(qda.class, direction.2005)
mean(qda.class == direction.2005)

# 4.7.5 Naive Bayes

library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = smarket, subset=train)

nb.fit

nb.class <- predict(nb.fit, smarket.2005)
table(nb.class, direction.2005)
mean(nb.class == direction.2005)

# 4.7.6 K-Nearest Neighbors

library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction = Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, direction.2005)

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, direction.2005)
mean(knn.pred == direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X <- scale(Caravan[, -86])

test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")

table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)

glm.fits <- glm(Purchase ~ ., data=Caravan, family=binomial, subset=-test)

glm.probs <- predict(glm.fits, Caravan[test, ], type="response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred, test.Y)

glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
table(glm.pred, test.Y)

# 4.4.7 Poisson Regression

attach(Bikeshare)
dim(Bikeshare)
names(Bikeshare)

mod.lm <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit,
  data = Bikeshare
)

summary(mod.lm)

contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)

mod.lm2 <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit, 
  data = Bikeshare
)
summary(mod.lm2)

all.equal(predict(mod.lm), predict(mod.lm2))
coef.months <- c(coef(mod.lm2)[2:12], -sum(coef(mod.lm2)[2:12]))

plot(coef.months, xlab = "Month", ylab = "Coefficient", xaxt = "n", 
     col=4, pch=19, type="o")
axis(side = 1, tick=T, at = 1:12, labels = c("J", "F", "M", "A", 
                                     "M", "J", "J", "A", "S", "O", "N", "D"))

coef.hours <- c(coef(mod.lm2)[13:35], -sum(coef(mod.lm2)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient", 
     col=4, pch=19, lwd=2, type="o")

mod.pois <- glm(
  bikers ~ mnth + hr + workingday + temp + weathersit,
  data = Bikeshare, family = poisson
)
summary(mod.pois)

coef.months <- c(coef(mod.pois)[2:12], -sum(coef(mod.pois)[2:12]))
plot(coef.months, xlab = "Month", ylab = "Coefficient",
     xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J"
                                     , "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.pois)[13:35],-sum(coef(mod.pois)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
     col = "blue", pch = 19, type = "o")

plot(predict(mod.lm2), predict(mod.pois, type = "response"))
abline(0, 1, col = 2, lwd = 3)