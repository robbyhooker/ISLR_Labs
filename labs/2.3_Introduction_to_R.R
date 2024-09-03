x <- c(1, 3, 2, 5)
x

x = c(1, 6, 2)
x

y = c(1, 4, 3)

length(x)
length(y)
x + y

ls()
rm(x, y)
ls()

rm(list = ls())

?matrix
rnames <- list("Row 1", "Row 2")
cnames <- list("Col 1", "Col2")
tnames <- list(rnames, cnames)
length(rnames)
data = c(1, 2, 3, 4)
x <- matrix(data=data, nrow=2, ncol=2, byrow=TRUE, dimnames=tnames)

x
sqrt(x)
x^2

x <- rnorm(50)
y <- x + rnorm(50, mean=50, sd=.1)
cor(x, y)

set.seed(1303)
x <- rnorm(100)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
plot(x, y, xlab = "x-axis", ylab="y-axis", main="Plot of X vs Y")

plot(x, y, col="blue")

x <- seq(-pi, pi, length=50)

?contour
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))
contour(x, y, f, nlevels = 45, add = T)
fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels=15)

persp(x, y, fa, theta=45, phi=30)

A <- matrix(1:16, 4, 4)
A
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
A[1,]

A[-c(1, 3), ]
A[-c(1, 3), -c(1, 3, 4)]
dim(A)

rm(list = ls())
Auto <- read.table("Auto.data", header = T, na.strings = "?", stringsAsFactors = T)
head(Auto)
View(Auto)
dim(Auto)

Auto <- na.omit(Auto)
dim(Auto)

names(Auto)
attach(Auto)
plot(cylinders, mpg)

cylinders <- as.factor(cylinders)

plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth=T)
plot(cylinders, mpg, col = "red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col = "red", varwidth=T, xlab="cylinders", ylab="MPG")

hist(mpg, col=5, breaks=15)
pairs(
  ~ mpg + displacement + horsepower + weight + acceleration, data = Auto
)

plot(horsepower, mpg)
identify(horsepower, mpg, name)

summary(Auto)

summary(mpg)
rm(list = ls())
dev.off()
q()




