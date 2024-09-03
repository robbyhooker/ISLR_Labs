college <- read.csv("College.csv")
View(college)

rownames(college) <- college[, 1]
View(college)
college <- college[, -1]
View(college)
names(college)

summary(college)
attach(college)
pairs(college[,2:5])
boxplot(Outstate~Private, data = college)

Elite <- rep("No", nrow(college))
Elite[Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(Elite)
boxplot(Outstate~Elite, data=college)

par(mfrow = c(1,1))
hist(Outstate, breaks=15, col=2)

Auto <- read.table("Auto.data", header = T, na.strings = "?", stringsAsFactors = T)
Auto <- na.omit(Auto)
dim(Auto)
attach(Auto)
mean(mpg)
sd(mpg)
mean(acceleration)
sd(acceleration)

Auto.reduced = Auto[-c(10:84),]
sapply(Auto.reduced[,1:7], range)
sapply(Auto.reduced[,1:7], mean)
sapply(Auto.reduced[,1:7], sd)
year <- as.factor(year)
plot(year, mpg, col=6, varwidth=T, xlab="year", ylab="mpg")

library(ISLR2)

boston <- Boston
?Boston
View(boston)
plot(lstat, crim)
pairs(boston[, 1:13])
range(crim)

High.Crime = Boston[which(Boston$crim > mean(Boston$crim) + 2*sd(Boston$crim)),]
range(Boston$crim) ; mean(Boston$crim) ; sd(Boston$crim)

sum(chas==1)
sum(chas==0)

median(ptratio)
which(medv == min(medv))
boston[399,]
sum(rm>7)
sum(rm>8)

summary(subset(boston, rm > 8))
summary(boston)
