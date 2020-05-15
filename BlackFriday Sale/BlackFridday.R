library(UsingR)
library(sampling)
library(stringr)  # for STRING operations
library(tidyverse)  # to work with TIBBLE
library(stats)
library(prob)
library(dtplyr)
library(dbplyr)
library(data.table)
library(DataExplorer) # For initial exploratory data analysis
library(dplyr) # For data manipulation
library(githubinstall)
githubinstall("xda")
library(xda) # For Exploratory data analysis
library(ggplot2)
library(vcd)
library(rpart)
library(corrplot)
library(Amelia)
library(caTools)
library(rpart.plot)


# Load The Dataset

BlackFridayDataset <- read.csv("C:/Users/Rashmi/Desktop/Black Friday/BlackFridayDataset.csv")

set.seed(123) # set seed to ensure you always have same random numbers generated

#Removing Na value rows
BlackFriday <- na.omit(BlackFridayDataset)
View(BlackFriday)

#Checking missing data.
missmap(BlackFriday, col = c('yellow','black'), main = 'check') #We got Plot as complete Black,which indicates that there is no missing value

str(BlackFriday) #structure of data

summary(BlackFriday)


###########The top 5 purchased product.###########

Most_sold <- BlackFriday %>% group_by(Product_ID) %>% summarise(count=n()) %>% arrange(desc(count))
top_5 <- Most_sold[1:5, ]
names(top_5) <- c('Product_ID', 'Quantity')
ggplot(top_5,aes(factor(
  Product_ID,
  level = c('P00110742','P00184942','P00025442','P00112142','P00057642')), Quantity)) + 
  geom_bar(stat = 'identity',fill = c('#c4d8ba', '#d8ceba', '#bac4d8', '#e1daae', '#fa5845'))
+ xlab('Product_ID') + theme_bw()

#########The top 5 products that brought in the most revenue.############

Revenue <- BlackFriday %>% group_by(Product_ID) %>% summarise(revenue = sum(Purchase)) %>% arrange(desc(revenue))
top_5_revenue <- Revenue[1:5, ]
names(top_5_revenue) <- c('Product_ID', 'Revenue')
ggplot(top_5_revenue,aes(factor(
  Product_ID,
  level = c('P00110742','P00184942','P00025442','P00112142','P00059442')), Revenue)) + geom_bar(stat = 'identity',fill = c('#c4d8ba', '#d8ceba', '#bac4d8', '#e1daae', '#fa5845')
  ) + xlab('Product_ID') + theme_bw()

            ####The target variable being the amount of Purchase, the analysis begins with its distribution.#####

ggplot(BlackFriday, aes(Purchase)) + 
  geom_histogram(aes(y=..density..), fill="Pink") +
  geom_density(alpha=1, color = '#49a4aa', size = 1.2) + theme_bw() + xlab('Amount Spent by each buyer') + ylab('Number of buyers') + ggtitle('Purchase Distribution') +  theme(plot.title = element_text(hjust = 0.5))

#Correlation between numerical predictors and Purchase
data <- BlackFriday[, c(1, 5, 9, 10, 11)]
res <- cor(data, use = "complete.obs")
res <- round(res, 2)
corrplot(res, tl.col = 'black', tl.cex = .7, tl.srt = 45) #category 1,2,3 are strong. Hence, product_category 2 and 3 are dropped to avoid multicolinearity.

#########occupation#########
ggplot(BlackFriday, aes(Occupation)) + geom_bar(fill= 'light blue') + theme_classic()  #Occupation 0,4,7 are most noticeable among customers.

by_occ <- BlackFriday %>% group_by(Occupation) %>% summarise(Spending = mean(Purchase))
ggplot(by_occ, aes(Occupation, Spending)) + geom_bar(stat='identity', fill= 'light blue') #The difference in average purchase accross occupations is small.

#########Marital status.#########
ggplot(BlackFriday ,aes(Marital_Status)) + geom_bar(fill= c('red','blue')) # 0=single, 1=Married

table(BlackFriday$Marital_Status)
#there are 32126 more single customers than couples.

#########Product Category##########
ggplot(BlackFriday, aes(Product_Category_1)) + geom_bar(fill = 'light blue') + scale_x_continuous(breaks = 1:20) # Product categories 1,5,2 are in much higher demand relative to other product categories.

by_prod <- BlackFriday %>% group_by(Product_Category_1) %>% summarise(Purchase = mean(Purchase))
ggplot(by_prod, aes(Product_Category_1, Purchase)) + geom_bar(stat = 'identity', fill = 'light blue') + scale_x_continuous(breaks = 1:20) 
#When comparing average spending on each category, surprisingly, the highest is 10,6,15,1,2,3. We can assume that product 1,5,2 are products that consumers take 
#advantage of during Black friday to purchase on lower price.

##########Gender##############
ggplot(BlackFriday,aes(Gender)) + geom_bar(fill= c('red','blue')) + theme_bw() #Males spends more than females

by_gend <- BlackFriday %>% group_by(Gender) %>% summarise(Purchase = mean(Purchase))
ggplot(by_gend, aes(Gender, Purchase)) + geom_bar(stat='identity', fill = c('red', 'blue'))
#There are more considerably more male shoppers and they spend on average.

##########Age########
ggplot(BlackFriday,aes(Age)) + geom_bar(fill = c('#c5fafa', 
                                        '#002ba1', 
                                        '#6287ec', 
                                        '#dd0244', 
                                        '#c90000', 
                                        '#fb0202', 
                                        '#ff9ecb') ) + theme_bw() 
#Most purchase are made by 18-45 year old

by_age <- BlackFriday %>% group_by(Age) %>% summarise(Purchase = mean(Purchase))
ggplot(by_age, aes(Age, Purchase)) + geom_bar(
  stat = 'identity',
  fill = c('#c5fafa', 
           '#002ba1', 
           '#6287ec', 
           '#dd0244', 
           '#c90000', 
           '#fb0202', 
           '#ff9ecb')) 
# On average, each age group spends about the same. Younger age groups contribute more to the sales as a whole,
#however, the age group that spends the most on average are 51-55.


#######City Category######
ggplot(BlackFriday,aes(City_Category)) + geom_bar(fill = c('red','blue','green'))  

by_city <- BlackFriday %>% group_by(City_Category) %>% summarise(Purchase = mean(Purchase))
ggplot(by_city, aes(City_Category, Purchase)) + geom_bar(
  stat = 'identity',
  fill = c('red','blue','green')
)
#Although city B had a lot more customers, the highest average spending occurred at city C on average.
#City B was able to attract the most customers but failed to attract them to spend the most.


#######Number of years customer lived in the city that they are buying.##########
ggplot(BlackFriday,aes(Stay_In_Current_City_Years)) + geom_bar(fill = c(
  '#c4d8ba',
  '#d8ceba',
  '#bac4d8',
  '#6f2859',
  '#e6e6fa'
)) #Buyers that moved most recently to the city purchased the most.

by_stay <- BlackFriday %>% group_by(Stay_In_Current_City_Years) %>% summarise(Purchase = mean(Purchase))
ggplot(by_stay, aes(Stay_In_Current_City_Years, Purchase)) + geom_bar(
  stat = 'identity',
  fill = c(
    '#c4d8ba',
    '#d8ceba',
    '#bac4d8',
    '#6f2859',
    '#e6e6fa'
  )
) 
#Same goes for the duration of stay. Regardless of the duration
#the average spend was more or less the same eventhough new settlers were the group that was most likely to take advantage of Black Friday.


                      #####Transform the data types#######

BlackFriday$User_ID <- as.factor(BlackFriday$User_ID)
BlackFriday$Product_ID <- as.factor(BlackFriday$Product_ID)
BlackFriday$Gender <- as.factor(BlackFriday$Gender)
BlackFriday$Product_Category_1 <- as.factor(BlackFriday$Product_Category_1)
BlackFriday$Product_Category_2 <- as.factor(BlackFriday$Product_Category_2)
BlackFriday$Product_Category_3 <- as.factor(BlackFriday$Product_Category_3)
BlackFriday$Marital_Status <- as.factor(BlackFriday$Marital_Status)
BlackFriday$City_Category <- as.factor(BlackFriday$City_Category)
BlackFriday$Occupation <- as.factor(BlackFriday$Occupation)
BlackFriday$Stay_In_Current_City_Years <- as.factor(BlackFriday$Stay_In_Current_City_Years)
BlackFriday$Age <- as.integer(BlackFriday$Age)

BlackFriday$Product_ID <- NULL #ProductID and UserID are dropped as unique identifiers are not of our interest. 
BlackFriday$User_ID <- NULL #Instead, customer's general attribute are more of interest in estimating the influences on the target variable, purchase.


          ####Spliting data into train(70%) and test(30%)####
set.seed(123) # set seed to ensure you always have same random numbers generated
ind <- sample(2,nrow(BlackFriday),replace = TRUE,prob = c(0.7,0.3))
traindata <- BlackFriday [ind==1,]
testdata <- BlackFriday [ind==2,]

