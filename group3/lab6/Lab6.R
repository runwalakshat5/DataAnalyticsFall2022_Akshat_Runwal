vec = c(1,7,30,60,72,98,145,212,240,270,399,400,450,3000,3333,5000)
fivenum(vec)
boxplot(vec)

lowerQ = fivenum(vec)[2]
upperQ = fivenum(vec)[4]
iqr = upperQ - lowerQ
iqr

lowerIF = lowerQ-iqr*1.5
upperIF = upperQ+iqr*1.5

lowerIF
upperIF



mtcars
head(mtcars)
str(mtcars)
model1 <- lm(mpg ~ cyl + wt, data=mtcars)
model1

plot(model1, pch=18, col='red', which = c(4))

cooks.distance(model1)
CooksDistance = cooks.distance(model1)

sort(round(CooksDistance,5))



library(ISLR)
head(Hitters)
dim(Hitters)
is.na(Hitters)

HittersData = na.omit(Hitters)
dim(HittersData)
glimpse(HittersData)


SalaryPredictModel1 = lm(Salary ~ ., data=HittersData)
summary(SalaryPredictModel1)

cooksD = cooks.distance(SalaryPredictModel1)
influential = cooksD[(cooksD > (3 * mean(cooksD, na.rm=TRUE)))]
influential


names_of_influence = names(influential)
names_of_influence

library(dplyr)

outliers <- HittersData[names_of_influence,]
Hitters_Without_Outliers = HittersData %>% anti_join(outliers)

SalaryPredictModel2 = lm(Salary ~ ., data=Hitters_Without_Outliers)
summary(SalaryPredictModel2)
