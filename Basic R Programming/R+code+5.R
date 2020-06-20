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
stem(iris$Sepal.Length) # stem & leaf

write.csv(iris, file = "iris.csv")
getwd()

setwd()
data(faithful)
?faithful
View(faithful)
duration = faithful$eruptions
max(table(duration))
stem(faithful$eruptions)

write.csv(faithful, file="faithful.csv")

?iris
stem(iris$Sepal.Length)
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

plot(hexbin( mtcars$cyl,mtcars$mpg))
table(mtcars$mpg,mtcars$cyl)
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

gadm<- getData('GADM', country = "IND", level=2) 
plot(gadm)
table(gadm$NAME_1)
gadm_maha=subset(gadm,gadm$NAME_1=="Maharashtra")
plot(gadm_GUJ,main="maharashtra",col=brewer.pal(8,"Dark2"))

subset(c_ind_1,c_ind_1$NAME_1=="Assam")
######
plot(subset(c_ind_1,c_ind_1$NAME_1=="Assam"))
# GADM

library(raster)
View(ccodes()) # Country codes
gadm_brazil_0<-getData("GADM",country="BRA",level=0)
gadm_brazil_1<-getData("GADM",country="BRA",level=1)
gadm_brazil_2<-getData("GADM",country="BRA",level=2)

# Plot 
library(RColorBrewer)
par(mfrow=c(3,1))
plot(gadm_brazil_0,col=brewer.pal(8,"Dark2"))
plot(gadm_brazil_1,col=brewer.pal(8,"Dark2"))
plot(gadm_brazil_2,col=brewer.pal(8,"Dark2"))

c_ind<-getData("GADM",country="IND",level=0)
c_ind_1<-getData("GADM",country="IND",level=1)


plot(c_ind_1,col=brewer.pal(12,"Dark2"),main="INDIA")
plot(c_ind,col="lightgreen")
# worldclim
library(raster)
climate<-getData("worldclim",var="bio",res=2.5)
climate

summary(climate)
View(climate)
library(RColorBrewer)
plot(climate$bio1,col=brewer.pal(8,"Accent"),main="Annual Mean Temperature")
plot(climate$bio10,col=brewer.pal(8,"Accent"),main="Annual Precipitation")

# Elevation plot
install.packages("rgdal")
library(raster)
library(rgdal)
library(sp)
srtm<-getData("SRTM",lon=16,lat=48)
srtm2 <- getData('SRTM', lon=13, lat=48)
srtm3 <- getData('SRTM', lon=9, lat=48)
srtmmosaid<-mosaic(srtm,srtm2,srtm3,fun=mean)
plot(srtmmosaid);plot(gadm_maha)
View(srtm)
gadm<- getData('GADM', country = "IND", level=2)


plot(srtmmosaid)
plot(gadm_maha,add=T)

 
# displaying maps
install.packages("maps")
install.packages("mapdata")
library(maps)
library(mapdata)
help("maps")
??maps
??mapdata
library(RColorBrewer)
map('world2',fill=TRUE)

# Display
