2 + 2 # control + enter  or control + R
# works as calculator

1:50 # print numbers 1 to 50 to the console

50:1 # print numbers 50 to 1 in reverse order to console

print ("Hello World!")

# ctrl + L is going to clear the console

x <- 1:5 # assigning numbers 1 to 5 to the object x
x

y <- c(3, 5, 8, 1, 2) # alt + '-' is the shortcut for assignment operator
y

a <- x + y
a

z <- 4:9
z

x * 2

ls()  # list objects

install.packages("XML")

library(XML)

version

xyz <- read.csv(choose.files()) # load csv file into R

# C:\Users\ExcelR\Desktop\mba.csv - this is windows default file path with a '\'
# C:\\Users\\ExcelR\\Desktop\\mba.csv - change it to '\\' to make it work in R

?read.csv

View(mba) # show the dataset uploaded to R

getwd() # shows the current working directory

setwd("E:/R Codes/Basics") # set a working directory of your choice

str(mba) # quick overview of the variables & dataset

update.packages()

install.packages("foreign")

library(foreign)

spss <- read.spss(choose.files())

spss

View(spss)
spss1 <- read.spss(choose.files(), to.data.frame = T, use.value.labels = T)

View(spss1)

??AirPassengers

?read.spss

install.packages("sas7bdat")  # or install Hmisc
library(sas7bdat)
data(sas7bdat.sources)

View(sas7bdat.sources)

str(sas7bdat.sources)

write.csv(sas7bdat.sources, file = "aaa.csv") 



install.packages("RCurl")
library(RCurl)
data2 <- getURL("https://www.excelr.com")


browseURL("http://ftp.iitm.ac.in/cran/")


rm(list=ls())


  
  # vector
  

x <- c(1, 2, 3, 4, 5)

mean(x)

# List

rain <- list('Y', 'N', 'N', 'Y', 'Y')
temp <- list(38, 32, 34, 38, 40)
rain

raintemp <- list(rain, temp)
raintemp

# matrix

temp <- c(38, 32, 34, 38, 40)
perce <- c(110, 102, 103, 117, 90)

matrix(c(temp, perce), nrow=2, byrow=T)

temptrain <- data.frame(temp = c(38, 32, 34, 38, 40), 
                           perce=c(110, 102, 103, 117, 90), 
                           rain=c('Y', 'N', 'N', 'Y', 'Y'))

temptrain
View(temperptrain)


#user defined function
cube <- function(x){x*x*x}
a <- cube(2)
a
cube(1:4) 

#in-built functions
a <- seq(1, 10, 2)
a

b <- array(1:30, c(5, 3, 4))
b

data()
data("iris")
View(iris)
?iris
