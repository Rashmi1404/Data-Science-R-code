data()
?iris
data(iris)
View(iris)
plot(iris)
head(iris)
tail(iris)
plot(iris$Sepal.Length, iris$Species) # x axis (variable), y axis

plot(iris$Species, iris$Sepal.Length) # x axis is categorical

plot(iris$Sepal.Length)

write.csv(iris, file = "iris.csv")
getwd()

setwd()
data(faithful)
?faithful
View(faithful)

write.csv(faithful, file="faithful.csv")

?iris

hist(iris$Sepal.Length)

plot(iris$Sepal.Length, type = 'l')
?plot
barplot(iris$Sepal.Length)
pie(table(iris$Species))
hist(iris$Sepal.Length)
boxplot(iris$Sepal.Length, horizontal = T)
rug(iris$Sepal.Length, side=1)

?rug
par(mfrow=c(3,2))  # rows, columns
sunflowerplot(iris$Sepal.Length)
plot(iris$Sepal.Length)
boxplot(iris$Sepal.Length)
plot(iris$Sepal.Length, type='l')
plot(density(iris$Sepal.Length))
hist(iris$Sepal.Length)

par(mfrow=c(1,2))
plot(mtcars$mpg, mtcars$cyl, main = "Example Title", col = "blue", xlab = "Miles per Gallon", ylab = "Number of Cylinders")
plot(mtcars$mpg, mtcars$cyl)
colors()

par(bg="grey") # changed background color to grey
boxplot(mtcars$mpg~mtcars$cyl)  # Y = f(X) (E.g. Weight gained ~ Calories Consumed)

data("VADeaths")
View(VADeaths)
?VADeaths
head(VADeaths)
par(mfrow=c(3,2)) # rows, columns
hist(VADeaths, col=heat.colors(7), main="col=heat.colors(7)")
hist(VADeaths, col=terrain.colors(7), main="col=terrain.colors(7)")
hist(VADeaths, col=topo.colors(8), main="col=topo.colors(8)")
hist(VADeaths, col=cm.colors(8), main="col=cm.colors(8)")
hist(VADeaths, col=cm.colors(10), main="col=cm.colors(10)")
hist(VADeaths, col=rainbow(8), main="col=rainbow(8)")
?hist

?par
install.packages("RColorBrewer")
library(RColorBrewer)

par(mfrow=c(2,3)) # 2 rows & 3 columns
hist(VADeaths, col=brewer.pal(3, "Set3"), main="Set3 3 colors")
hist(VADeaths, col=brewer.pal(3, "Set2"), main="Set2 3 colors")
hist(VADeaths, col=brewer.pal(3, "Set1"), main="Set1 3 colors")
hist(VADeaths, col=brewer.pal(8, "Set3"), main="Set3 8 colors")
hist(VADeaths, col=brewer.pal(8, "Greys"), main="Grey 8 colors")
hist(VADeaths, col=brewer.pal(8, "Greens"), main="Green 8 colors")

install.packages("hexbin")
library(hexbin)
plot(hexbin(iris$Species, iris$Sepal.Length))

plot(hexbin(mtcars$mpg, mtcars$cyl))

install.packages("tabplot")
library(tabplot)

tableplot(iris)
View(iris)
install.packages("vcd")
library(vcd)
mosaic(Titanic)

heatmap(as.matrix(mtcars))

install.packages("sp")
library(sp) 
install.packages("maptools")
library(maptools)
gpclibPermit()
nc <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
                    proj4string=CRS("+proj=longlat +datum=NAD27")) 
names(nc)
# create two dummy factor variables, with equal labels: 
set.seed(31)
nc$f = factor(sample(1:5,100,replace=T),labels=letters[1:5]) 
nc$g = factor(sample(1:5,100,replace=T),labels=letters[1:5])
library(RColorBrewer)
## Two (dummy) factor variables shown with qualitative colour ramp; degrees in axes
spplot(nc, c("f","g"), col.regions=brewer.pal(5, "Set3"), scales=list(draw = TRUE))

# axesspplot(nc, c("f","g"), col.regions=brewer.pal(5, "Set3"), scales=list(draw = TRUE))

install.packages("raster")
library(raster)
alt <- getData('alt', country = "IND")
plot(alt)

gadm<- getData('GADM', country = "IND", level=3) 
head(gadm)
table(gadm$NAME_1)
gadm_GUJ=subset(gadm,gadm$NAME_1=="Gujarat")
plot(gadm_GUJ)

