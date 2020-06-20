data()

data(mtcars)
View(mtcars)
attach(mtcars)

install.packages("Hmisc")    # Describes categorical data
library(Hmisc)
summary(mtcars)
describe(mtcars$cyl)

install.packages("psych")   # Describes numerical data
library(psych)
describe(mtcars$mpg)   

install.packages("grid")
library(grid)


install.packages("lattice")
library(lattice)



install.packages("ggplot2")
library(ggplot2)

describe(mtcars)

?describe

newcut <- summarize(mtcars$mpg, mtcars$cyl, mean) # (input, bygroup, function)
newcut

newcut <- summarize(mtcars$mpg, mtcars$cyl, quantile)




newcut

newcut <- summarize (mtcars$mpg, mtcars$cyl, summary)
newcut

attach(mtcars)

mean(wt, na.rm=T)
var(wt, na.rm = T)
sd (wt, na.rm=T)
unique(hp)

aggregate(mpg~cyl, data =mtcars, mean)
aggregate(wt~gear, data=mtcars, sd)
cor(mtcars)
?cor
install.packages("corrgram")
library(corrgram)


corrgram(mtcars$mpg)
plot(mtcars$cyl, mtcars$mpg)  # plot (x, y)
plot(mtcars$cyl, type = "o", col="blue", ylim=c(0,30))
lines(mtcars$mpg, type = "o", col="red")
boxplot(mtcars$mpg, col="blue", horizontal = T)
boxplot(mtcars$mpg~mtcars$cyl, data=mtcars, main="Car Mileage Data", xlab = "No. of cylinders", ylab = "Miles per gallon")
stars(mtcars, draw.segments=TRUE, key.loc = c(13,1.5))


install.packages("dummy")
install.packages("dummies")
library(dummies)
library(dummy)

data("iris")
attach(iris)
x<- as.data.frame(dummy(iris))
iriss <- cbind(iris[,-5],x)
