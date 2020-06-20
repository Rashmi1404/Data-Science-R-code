setwd("D:\\ML\\R\\Naive Bayes")
library(mlbench)
data("HouseVotes84")
View(HouseVotes84)
help("HouseVotes84")
summary(HouseVotes84)

barplot(table(as.factor(HouseVotes84[,1]),as.factor(HouseVotes84[,2])),legend=c("democrat","republic"))
plot(as.factor(HouseVotes84[HouseVotes84$Class=="republican",2]))
plot(as.factor(HouseVotes84[HouseVotes84$Class=="democrat",2]))

str(HouseVotes84)
train_x<-HouseVotes84[,c()]

# na values by column and class

na_by_col_class<-function(col,cls){
  return(sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class==cls))
}

na_by_col_class("V1","democrat")
na_by_col_class("V1","republican")

p_y_col_class<-function(col,class){
  sum_y<-sum(HouseVotes84[,col]=="y" & HouseVotes84$Class==class,na.rm = T)
  sum_n<-sum(HouseVotes84[,col]=="n" & HouseVotes84$Class==class,na.rm = T)
  return (sum_y/(sum_y+sum_n))
}
p_y_col_class(2,'democrat')  
p_y_col_class(2,'republican')
na_by_col_class(2,'democrat')
na_by_col_class(2,'republican')


# imputing missing values 

set.seed(3)
train<-order(runif(290))
test<--train
training<-HouseVotes84[train,]
testing<-HouseVotes84[test,]
#train_X<-training[,-1]
#train_Y<-training[,1]
#test_X<-testing[,-1]
#test_Y<-testing[,1]

library(e1071)
model<-naiveBayes(training$Class~.,data=training)
pred<-predict(model,newdata = testing[,-1])
mean(pred==testing[,1])

acc<-NULL


for (i in 1:20){train<-order(runif(350))
set.seed(350)
test<--train
training<-HouseVotes84[train,]
testing<-HouseVotes84[test,]
model<-naiveBayes(training$Class~.,data=training[,-1])
pred<-predict(model,testing[,-1])
acc<-c(acc,mean(pred==testing[,1]))

}
acc



# Naive Bayes for the continuous data

data()
data("iris")
table(iris[,5])
View(iris3)
str(iris)
set.seed(135)
train<-order(runif(135))
t1<-train
t2<-train
test<--train
training<-iris[train,]
testing<-iris[test,]
table(training[,5])
library(e1071)
model_iris<-naiveBayes(training$Species~.,data = training[,-5])
pred_species<-predict(model_iris,testing[,-5])
mean(pred_species==testing[,5])
table(pred_species)
table(testing[,5])

boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
summary(iris$Petal.Length)
