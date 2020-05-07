airquality<-datasets::airquality
airquality   #uploading datasets


head(airquality)   #displays first 6 rows
tail(airquality)   ##displays last 6 rows


airquality[,c(1,2)] #displays first two colms
airquality[,c(5,6)]  #last two colms
airquality$Temp   #can also access the colms using dollar symbol


summary(airquality$Temp)  #we get summary of coloms using this commands that is mean and median we get using it
summary(airquality$Ozone)
summary(airquality$Wind)


plot(airquality$Ozone)  #for ploting graphs
plot(airquality$Ozone,airquality$Temp)  #how ozone affects temperature
plot(airquality)   #gives scatter plots


summary(airquality)  #gives summary of all coloms of airquality


#points and line
plot(airquality$Ozone, type="l") #p:points, l:lines,b:both,h:histogram 

plot(airquality$Ozone, xlab = 'ozone conscentration',ylab = 'no. of instances',main ='Ozone levels in NY city',col= 'blue')

#horizontal barplot
plot(airquality$Ozone, main ='Ozone concentration of air',xlab = 'ozone levels', col= 'Red',horiz = FALSE)

#histogram
hist(airquality$Solar.R)
hist(airquality$Solar.R,main = 'Solar Radiation Vakue in Air', xlab = 'Solar Rad', col='yellow')


#Single boxplot
boxplot(airquality$Solar.R)

#multiple Boxplot
boxplot(airquality[,1:4],main='Multiple') #shows outiliers of first four colms, show multiple box plots

#to plot all the graphs together
par(mfrow=c(3,3),mar=c(2,5,2,1), las=0, bty="n")
plot(airquality$Ozone)
plot(airquality$Ozone, main ='Ozone concentration of air',xlab = 'ozone levels', col= 'Red',horiz = FALSE)
hist(airquality$Solar.R)
hist(airquality$Solar.R,main = 'Solar Radiation Vakue in Air', xlab = 'Solar Rad', col='yellow')
boxplot(airquality$Solar.R)
boxplot(airquality[,1:4],main='Multiple') #shows outiliers of first four colms, show multiple box plots
plot(airquality$Ozone, type="l") #p:points, l:lines,b:both,h:histogram 

plot(airquality$Ozone, xlab = 'ozone conscentration',ylab = 'no. of instances',main ='Ozone levels in NY city',col= 'blue')