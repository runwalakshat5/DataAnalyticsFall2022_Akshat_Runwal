multivariate <- read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
# help(lm)
library(dplyr)
library(ggplot2)
View(multivariate)

homeowners <- multivariate$Homeowners
immigrant <- multivariate$Immigrant

mm <- lm(homeowners~immigrant)
mm
summary(mm)$coef

plot(homeowners~immigrant)
help(abline)
abline(mm)
abline(mm, col=2, lwd=3)

newImmigrantData <- data.frame(Immigrant = c(0, 20))
mm %>% predict(newImmigrantData)

abline(mm)
abline(mm, col=3, lwd=3)
attributes(mm)
mm$coefficients

# ggplot stuff
plot(multivariate$Homeowners, multivariate$Immigrant)
qplot(multivariate$Homeowners, multivariate$Immigrant)
qplot(Homeowners, Immigrant, data=multivariate)
ggplot(multivariate, aes(x=Homeowners, y=Immigrant)) + geom_point()
ggplot(multivariate, aes(x=Homeowners, y=Immigrant)) + geom_line() + geom_point()

# bar graphs
barplot(multivariate$Homeowners, names.arg = multivariate$City)

# histograms
hist(multivariate$Immigrant)
hist(multivariate$Immigrant, breaks = 10)
qplot(Homeowners, data = multivariate)
ggplot(multivariate, aes(x=Homeowners)) + geom_histogram(binwidth = 30)

# box plots
boxplot(Homeowners ~ Immigrant, data=multivariate)
qplot(multivariate$Homeowners, multivariate$Immigrant, geom = "boxplot")