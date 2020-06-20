read.csv(file="", header = TRUE, sep=",")

install.packages("foreign")
library(foreign)   # used to load statistical software packages

indata <- read.spss(choose.files())
indataframe <- as.data.frame(indata)
str(indataframe)
summary(indataframe)


install.packages("sas7bdat")  # or install Hmisc
library(sas7bdat)
data(sas7bdat.sources)





library(base)
web_page_data <- readLines("http://www.edureka.co")

install.packages("RCurl")
library(RCurl)
data2 <- getURL("http://www.edureka.co")
