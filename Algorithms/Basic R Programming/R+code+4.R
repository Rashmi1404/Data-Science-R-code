data()

data(mtcars)
View(mtcars)
attach(mtcars)

install.packages("Hmisc")
library(Hmisc)
summary(mtcars)
install.packages("grid")
library(grid)
install.packages("lattice")
library(lattice)
install.packages("survival")
library(survival)
install.packages("Formula")
library(Formula)
install.packages("ggplot2")
library(ggplot2)

describe(mtcars)

?describe

newcut <- summarize(mtcars$mpg, mtcars$cyl, mean) # (input, bygroup, function)
class(newcut)
newcut
newcut2 <- summarize(mtcars$mpg, mtcars$cyl, quantile)
newcut2

newcut3 <- summarize (mtcars$mpg, mtcars$cyl, summary)
newcut3

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
install.packages("iterators")
library(iterators)

corrgram(mtcars,text.panel = panel.txt,upper.panel = panel.cor,lower.panel = panel.pie,diag.panel = panel.minmax)
plot(mtcars$cyl, mtcars$mpg)  # plot (x, y)
plot(mtcars$cyl, type = "o", col="blue", ylim=c(0,30))
lines(mtcars$mpg, type = "o", col="red")
boxplot(mtcars$mpg, col="blue", horizontal = T)
boxplot(mtcars$mpg~mtcars$cyl, data=mtcars, main="Car Mileage Data", xlab = "No. of cylinders", ylab = "Miles per gallon")
#stars(mtcars, draw.segments=TRUE, key.loc = c(13,1.5))
boxplot(mtcars$cyl~mtcars$mpg)

#stars(mtcars[, 1:7], locations = c(0, 0), radius = FALSE,
      key.loc = c(0, 0), main = "Motor Trend Cars", lty = 2)
