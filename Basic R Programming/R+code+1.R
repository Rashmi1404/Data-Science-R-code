2 + 2 # control + enter  or control + R
# works as calculator

1:50 # print numbers 1 to 50 to the console

50:1 # print numbers 50 to 1 in reverse order to console

print ("Hello World!")

# ctrl + L is going to clear the console

x <- 1:5 # assigning numbers 1 to 5 to the object x
x

y <- c(3, 5, 8, 1, 2) # alt + '-' is the shortcut for assignment operator
y[1:5]
y[-4]
y[c(1,4,5)]
length(y)
class(y)
x<-c(1,3,4,5,6) # 5
y<-c(10,12,13) # 3 
x+y
a <- x + y
a

z <- 4:9
z

x + z

x * 2

x<-1:100
for(i in 1:100){
  if(i%%2!=0){
    print(x[i])
  }
  
}
y<-function(k){
  s<-sum(k)
  MEAN<-s/length(k)
  print(MEAN)
}
g<-c(1,3,3,2,5,5,7,9,10)
mean(g)
y(g)
ls()  # list objects

install.packages("XML")

library(XML)

version

mba <- read.csv(file.choose()) # load csv file into R

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

spss <- read.spss(file.choose())
class(spss)
spss
class(c("AB","BC",6))

L<-list(A=c(1,3,4,6),B=c("AB","CD",6))
L$A[c(1,3)]
L$B

??spss
class(spss)
help("spss")
??Foreign

View(spss)
spss1 <- read.spss(file.choose(), to.data.frame = T, use.value.labels = F)
class(spss1)
str(spss1)
View(spss1)

??AirPassengers

?read.spss

install.packages("sas7bdat")  # or install Hmisc
library(sas7bdat)
data(sas7bdat.sources)

View(sas7bdat.sources)
??sas7bdat.sources
str(sas7bdat.sources)


write.csv(sas7bdat.sources, file = "aaa.csv") 

 # Installing package to access data from Oracle data base 
install.packages("D:\\R\\ROracle_1.2-1.zip")
# Establishing connection to Oracl data base
library(ROracle) #Package to load data from Oracle database
drv <- dbDriver("Oracle")  # Connecting 
??dbDriver
con <- dbConnect(drv, "system", "pavan") # Establishes connection with the database 

# My data base name: "system" and password : "pavan"

student<-dbGetQuery(con,"select * from student") # Getting a table from Oracle data base 
View(student)
class(dbGetQuery(con,"select * from sports"))
data("mtcars")
View(mtcars)

dbWriteTable(con,"spec",mtcars)
dbGetQuery(con,"select * from SPEC")
dbDisconnect(con)

install.packages("base")
library(base)
web_page_data <- readLines("https://en.wikipedia.org/wiki/Wikipedia")
class(web_page_data)
length(web_page_data)
install.packages("RCurl")
library(RCurl)
data2 <- getURL("https://www.excelr.com")

class(data2)

browseURL("http://google.co.in")
View(mtcars)

library()
install.packages("xlsx")
library(xlsx)

bahman<-read.xlsx(file.choose(),1)
class(bahman)

c<-data.frame(A=c(1,5),B=c("AB","CD"))
View(c)

library(help = "xlsx")

vignette(package="xlsx")

browseVignettes(package="xlsx")

vignette()

browseVignettes()


rm(list=ls())

  
  # vector
  
temp <- c(38, 32, 34, 38, 40)  

mean(c(1,2,3,4,5))

x <- c(1, 2, 3, 4, 5)

mean(x)

# List

rain <- list(A='Y', B='N', C='N', D='Y', E='Y')
class(rain$A)
class(rain["A"])

temp <- list(a1=38, a2=32, a3=34, a4=38, a5=40)

class(mba["gmat"])
class(mba$gmat)
mba$gmat
raintemp <- list(rain, temp)
raintemp

# matrix

temp <- c(38,43,53,54,50,66) # 6
percp <- c(110, 102, 103) # 3 

#, 117, 90,121,2232,232,32423)

m<-matrix(c(temp, percp), nrow=2, ncol=3,byrow=T)
b<-data.frame(m)
colnames(b)<-c("X","Y","Z")
temperptrain <- data.frame(temp = c(38, 32, 34, 38, 40), 
                           percp=c(110, 102, 103, 117, 90), 
                           rain=c('Y', 'N', 'N', 'Y', 'Y'))

temperptrain

#user defined function
cube <- function(x){x*x*x}
cube(2)
cube(1:4) 

#in-built functions
a <- seq(100,0, -9)
a
fraud<-read.csv(file.choose())

View(mba)
ordered <- order(mba$workex) # arrange dataset in ascending order based on a var
ordered
View(mba[ordered,])
fraudData[ordered, ]
p<-mba[ordered,]$workex
k<-sort(mba$workex)
p==k
fraudData[rev(order(fraudData$creditLine)), ] # arrange dataset in descending order based on a var

Dia<-read.csv(file.choose())
attach(Dia)
df<-data.frame(A=c(1,2,3,4,5),B=c("A","B","C","D","E"))
View(df)
df2<-data.frame(C=9:11,D=c("FF","GG","DD"))
kk<-cbind(df$A,df2$C)
View(kk)
colnames(df2)<-c("A","B")

colnames(df2)<-c("B","A")
pp<-rbind(df,df2)
df<-data.frame(x=1:2,y=3:4)
rbind(df,df[,2:1])
View(pp)
str(pp)
cc <- cbind(Plasma, Diabetes) # used to combine 2 datasets with unequal columns

cr <- rbind(hour_transaction, transaction_data) # used to combine 2 datasets with unequal rows

credit<-read.csv(file.choose())
View(credit)

# Merge Functions in Data Frames 

authors <- data.frame(
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))

View(authors)
View(books)

m1<-merge(authors,books,by.x="surname",by.y = "name")
View(m1)
m2 <- merge(books, authors, by.x = "name", by.y = "surname")
m3<-merge(authors, books, by.x = "surname", by.y = "name", all = TRUE)
View(m3)
class(m1)

View(m1)
View(m2)
View(m3)
sum(ctwod$fakeID_Cardholder)

sum(ctwod$fakeID_Cardholder, na.rm=TRUE)

b <- array(1:30, c(5, 3, 4))
a
b
a[ ,2:3, ] # rows, columns, tables

data()
data("EuStockMarkets")
View(EuStockMarkets)
?EuStockMarkets
