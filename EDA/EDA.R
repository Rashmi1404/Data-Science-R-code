a <- c(1,2,3,100)
mean(a)
median(a)

mba <- read.csv(choose.files())
#Measures of Central Tendency
mean(mba$gmat)
median(mba$gmat)

#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(mba$gmat)

#Measures of Dispersion
var(mba$gmat)
sd(mba$gmat)
range(mba$gmat)
rangevalue <- function(x){max(x)-min(x)}
rangevalue(mba$gmat)

#Measures of skewness
install.packages("moments")
library(moments)

#Measures of skewness
skewness(mba$gmat)


#Measures of Kurtosis 
kurtosis(mba$gmat)

#Graphical Representation
#Boxplot
#Histogram
#Barplot

boxplot(mba$gmat,horizontal = TRUE)
hist(mba$gmat)
barplot(mba$gmat)


str(mba)
mba$datasrno <- as.factor(mba$datasrno)

#install.packages(psych)
library(psych)
describe(mba)


# to calculate Z score
qnorm(0.950)#90%
qnorm(0.975)#95%
qnorm(0.995)


#to calculate t score
qt(0.950,772)#0.90%

#qqplot
qqnorm(mba$gmat)
qqline(mba$gmat)


















