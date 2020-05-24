#####Simple Linear Equation For Weight Gain$#############

##Calories_consumed-> predict weight gained using calories consumed
#Do the necessary transformations for input variables for getting better R^2 value for the model prepared.
install.packages("DataExplorer")
library(DataExplorer)


#Loading the data
Calo<-read.csv("C:/Users/Rashmi/Desktop/Data Science/ASSIGNMENTS/Simple Linear Regression/calories_consumed.csv")
summary(Calo) #Gives the all over summary of dataset.e.g(mean,median,min,max,quartiles)
head(Calo) #Gives the first 6 values of data set
tail(Calo)# Gives the last 6 values of dataset


#Exploratory Data Analysis(EDA)

#standard Deviation

sd(Calo$Weight.gained..grams.) #Standard Deviation of weight column
sd(Calo$Calories.Consumed) #Standard Deviation of Calories column

#Variance

var(Calo$Weight.gained..grams.) #Variance of weight column
var(Calo$Calories.Consumed) #Variance Of Calories

#Correlation matrix

cor(Calo)  #Shows the correlation matrix of dataset

#PLOTS

boxplot(Calo)  #Boxplot shows if the datasets colums has any outliers or not,so here their are none
hist(Calo$Weight.gained..grams.) #Histogram
hist(Calo$Calories.Consumed) 
plot(Calo) #A simple dot plot of the dataset


        ####Simple LInear Model#####

model <- lm(Weight.gained..grams.~.,data = Calo)  #A simple linear model for dataset=Calo
summary(model) #Gives the overview of your model 
plot(model) #Gives the QQplot,residualplot etc
#Here we can see the R-Squared value is 0.896  which means 89% it will predict right value and p value is <0.05
#But here the residual error value is 111.6 which is not good 

##log transformation

#X= log(x) and Y=y ----lm(y~log(x))

calo_log_model <-lm(Weight.gained..grams.~log(Calories.Consumed),data=Calo) # A logarithmic model 
summary(calo_log_model)
plot(calo_log_model)
##Here we can see the R-Squared value is 0.807  which means 80% it will predict right value and p value is <0.05
#But here the residual error value is 152.6 which is not good at all

#Exponential Model

#X= x and Y=log(y) ----lm(log(y)~x)

calo_expo_model<-lm(log(Weight.gained..grams.)~.,data =Calo ) #A exponential model 
summary(calo_expo_model)
plot(calo_expo_model)

#Here we can see the R-Squared value is 0.877  which means 87% it will predict right value and p value is <0.05
#But here the residual error value is 0.3314 which is good 

#so we can say that the Exponential model is preferable over all as it has higest R squared value = 0.877 and lowest Residual Error=0.334
#so we will use it for prediction

predict <- predict(calo_expo_model) #Predicts for the given model
predict

calo_expo_model$residuals #gives the residual values


sqrt(mean(calo_expo_model$residuals^2))


ab <- exp(predict)
ab

confint(calo_expo_model,level=0.95)
predict(calo_expo_model, interval="confidence")

final<-data.frame(Calo,predict,"Error"= Calo$Weight.gained..grams.-ab)
final
