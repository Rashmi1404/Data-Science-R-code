library(arules)
library(arulesViz)


book <- read.csv("C:/Users/Rashmi/Desktop/ExcelR/ASSIGNMENTS/Association Rule/book.csv")
View(book)
str(book) #internal structure of object
class(book)
colnames(book)

book$ChildBks <- factor(book$ChildBks,levels = c("1","0"),labels = c("ChildBks",""))
book$YouthBks <- factor(book$YouthBks,levels = c("1","0"),labels = c("YouthBks",""))
book$CookBks <- factor(book$CookBks,levels = c("1","0"),labels = c("CookBks",""))
book$DoItYBks <- factor(book$DoItYBks,levels = c("1","0"),labels = c("DoItYBks",""))
book$RefBks <- factor(book$RefBks,levels = c("1","0"),labels = c("RefBks",""))
book$ArtBks <- factor(book$ArtBks,levels = c("1","0"),labels = c("ArtBks",""))
book$GeogBks <- factor(book$GeogBks,levels = c("1","0"),labels = c("GeogBks",""))
book$ItalCook <- factor(book$ItalCook,levels = c("1","0"),labels = c("ItalCook",""))
book$ItalAtlas <- factor(book$ItalAtlas,levels = c("1","0"),labels = c("ItalAtlas",""))
book$ItalArt <- factor(book$ItalArt,levels = c("1","0"),labels = c("ItalArt",""))
book$Florence <- factor(book$Florence,levels = c("1","0"),labels = c("Florence",""))


book1 <- as(book,"transactions")

itemFrequencyPlot(book1,topN=25)

rules <- apriori(book1, parameter = list(supp = 0.02, confidence = 0.5,minlen = 10, maxlen = 20))  # Provided the rules with 2 % Support, 50 % Confidence and Minimum to purchase # 5 books
inspect(head(sort(rules), n = 10))
rules #no. of rules #714 rules

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift))

head(quality(rules))


plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method="paracoord",control=list(reorder=TRUE))  #this takes longer time
plot(rules,method="two-key plot", jitter=0)
top4rules <- head(rules, n = 10, by = "confidence")
plot(top4rules, method = "graph",  engine = "htmlwidget")


#changing values of support and confidence 
rules1 <- apriori(book1, parameter = list(supp = 0.05, confidence = 0.8,minlen = 5, maxlen = 20))  # Provided the rules with 2 % Support, 50 % Confidence and Minimum to purchase # 5 books
inspect(head(sort(rules1), n = 10))
rules1 #no. of rules set of 21344 rules

plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")


rules2 <- apriori(book1, parameter = list(supp = 0.04, confidence = 0.6,minlen = 6, maxlen = 10))  # Provided the rules with 2 % Support, 50 % Confidence and Minimum to purchase # 5 books
inspect(head(sort(rules2), n = 10))
rules2 #no. of rules set of 26296  rules

plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")
