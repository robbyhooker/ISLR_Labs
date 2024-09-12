library(ISLR2)

auto <- Auto
attach(auto)

lm.fit <- lm(mpg ~ horsepower)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower=98), interval="confidence")
predict(lm.fit, data.frame(horsepower=98), interval="prediction")

plot(horsepower, mpg, pch=10, col=2)
abline(reg = lm.fit, col=4, lwd=2)

par(mfrow = c(1, 1))
plot(lm.fit)

pairs(auto)
cor_matrix <- cor(Auto[, names(auto) !="name"])

melted_cor_matrix <- melt(cor_matrix)

ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))


lm.fit <- lm(mpg ~ . - name, data = auto)


plot(lm.fit)
lm.fit <- lm(mpg ~ poly(weight, 2), data=auto)
summary(lm.fit)
plot(weight, mpg)

carseats <- Carseats
attach(carseats)
View(carseats)
contrasts(US)
lm.fit <- lm(Sales ~ Price + Urban + US)
summary(lm.fit)
lm2.fit <- lm(Sales ~ Price + US)
summary(lm2.fit)


confint(lm2.fit)
plot(lm2.fit)

set.seed(1)
x <- rnorm(100)
y <- x + rnorm(100, 0, 0.1)

lm.noint <- lm(y ~ x)
lm.noint2 <- lm(x ~ y)
summary(lm.noint)
summary(lm.noint2)
?lm()

x <- rnorm(100, 0, 1)
eps <- rnorm(100, 0, .25)
y <- -1 + 0.5 * x + eps

lm.fit3 <- lm(y ~ x)
summary(lm.fit3)
plot(x, y)
abline(lm.fit3, lwd=2, col=3)
abline(-1, 0.5, col = "red", lty = 2, lwd=2)
legend("topleft",
       c("model fit", "population regression"),
       col = c("black", "red"),
       lty = c(1, 2)
)

lm.fit4 <- lm(y ~ poly(x, 2))
summary(lm.fit4)

confint(lm.fit3)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

plot(x1, x2)
cor(x1, x2)

fit <- lm(y ~ x1 + x2)
summary(fit)

fit1 <- lm(y ~ x1)
summary(fit1)

fit2 <- lm(y ~ x2)
summary(fit2)

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

fitn <- lm(y ~ x1 + x2)
summary(fitn)
plot(fitn)


boston <- Boston
attach(boston)
na.omit(boston)
View(boston)

names(boston)

pred <- subset(boston, select = -crim)

fits <- lapply(pred, function(x) lm(crim ~ x))
printCoefmat(do.call(rbind, lapply(fits, function(x) coef(summary(x))[2, ])))

lm.all <- lm(crim ~ ., data = boston)
summary(lm.all)

plot(sapply(fits, function(x) coef(x)[2]), coef(lm.all)[-1], 
     xlab = "Univariate regression", 
     ylab = "multiple regression")

pred <- subset(pred, select = -chas)
fits <- lapply(names(pred), function(p) {
  f <- paste0("crim ~ poly(", p, ", 3)")
  lm(as.formula(f), data = boston)
})
for (fit in fits) printCoefmat(coef(summary(fit)))









