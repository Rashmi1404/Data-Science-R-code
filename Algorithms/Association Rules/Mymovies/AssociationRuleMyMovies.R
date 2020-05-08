library(rmarkdown)
library(arules)
library(arulesViz)

mymovies <- read.csv("C:/Users/Rashmi/Desktop/ExcelR/ASSIGNMENTS/Association Rule/my_movies.csv")

rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
rules

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift))
head(quality(rules))


plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
# It looks ike most of them has wateched Lord of the rings movies along with Gladiator and Greenville
# Also most of them has watched Gladiator, Sixth sense along with Patrioit
# Patriot ,Braveheart and other three items along with Gladiator. 
plot(rules,method = "graph")
plot(rules,method="paracoord",control=list(reorder=TRUE))  #this takes longer time
plot(rules,method="two-key plot", jitter=0)

toprules <- head(rules, n = 20, by = "confidence")
plot(toprules, method = "graph",  engine = "htmlwidget")


#changing values of support and confidence and minlen
rules3 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.5, confidence = 0.8,minlen=5)))
rules3 # 77 rules 
plot(rules3,method = "scatterplot")
plot(rules3,method = "grouped")
plot(rules3,method = "graph")


rules4 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.8, confidence = 1,minlen=6)))
rules4

plot(rules4,method = "scatterplot")
plot(rules4,method = "grouped")
plot(rules4,method = "graph")

