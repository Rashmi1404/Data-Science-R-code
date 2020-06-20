victims <- readLines(file.choose())
class(victims) # Vector type 
victims
df  <-  as.data.frame (victims)

df

class(df)

length(df)

nrow(df)

ncol(df)

dim(df)

str(df)
class(df$victims) # Vector format 
class(df["victims"]) # Data Frame 

comments  <- grepl("^%", victims)
comments
#victims<-c(victims,"aseqw%322")
text  <- victims[!comments]
text

comments_grepl  <-  grepl("^%", victims)
?grepl

comments_grepl

comments_grep  <- grep ("^%", victims)
comments_grep

text_grep  <- victims[-comments_grep]
text_grep

text[1]

x  <- text[1]
x

y  <- sub ("[[:digit:]]", "", x) # (pattern, replacement, x)
y


text[1]
x  <- text[1]
y  <- gsub ("[[:digit:]]", "", x)
y

text[9]

r  <- regexpr("9", text[9])
r

r  <- gregexpr("9", text[9])
r
mon<-unlist(strsplit(boby_lines,split = "-C"))
mon
k<-length(unlist(strsplit(boby_lines,split = "-C")))
pk<-NULL
for (i in 1:k){
  if(i%%2==0){
    ki<-unlist(strsplit(mon[i],"-P"))
    print(ki)
    pk<-c(pk,ki[1])
  }
}


pk
mon
ki<-unlist(strsplit(mon[c(2,4,6)],"-P"))
ki[c(1,3,5)]
text
splitlines  <- strsplit(text, split = ",")
class(splitlines)
splitlines


boby_lines<-readLines(file.choose())


Lines  <- matrix (unlist(splitlines), nrow=length(splitlines), byrow = TRUE)
Lines

colnames(Lines)  <- c("Name", "BirthYear", "DeathYear")
titanic_victims  <-  as.data.frame(Lines, stringsAsFactors = FALSE)
titanic_victims

class(titanic_victims$BirthYear)

titanic_victims$BirthYear  <- as.numeric(titanic_victims$BirthYear)
titanic_victims$BirthYear

titanic_victims  <- transform (titanic_victims, BirthYear = as.numeric(BirthYear), 
                               DeathYear = as.numeric(DeathYear))

class(titanic_victims$BirthYear)

class(titanic_victims$DeathYear)

titanic_victims

str(titanic_victims)

mean(titanic_victims$BirthYear)
round(mean(titanic_victims$BirthYear))

telecomsCalls<-read.csv(file.choose())

clean_callsdata  <- apply(telecomsCalls, MARGIN=2, 
                          function(x)
                          {ifelse (x==99 | x==-99, NA,x)})

clean_callsdata

apply(clean_callsdata, MARGIN=2, function(x) {mean(x,na.rm=TRUE)})
?apply

apply(clean_callsdata, MARGIN=2, function(x) {sum(x, na.rm=TRUE)})

install.packages("lattice")

library(lattice)
data(barley)
View(barley)

dim(barley)
str(barley)
class(lapply(barley, function(x) length(unique(x))))

class(sapply(barley, function(x) length(unique(x))))
apply(barley, 2,function(x) length(unique(x)))
?apply

tapply (barley$yield, barley$site, mean)
table(barley$site)

tapply(barley$yield, list(barley$year, barley$site), mean)
table(barley$year)
