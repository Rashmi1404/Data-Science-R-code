# Load wc_at.csv dataset
library(readr)
wc_at <- read_csv("C:/Datasets_BA/Linear Regression/wc-at.csv")
View(wc_at)

# Exploratory data analysis
summary(wc_at)

#Scatter plot
plot(wc_at$Waist, wc_at$AT)  # plot(X,Y)

?plot

attach(wc_at)


#Correlation Coefficient (r)
cor(Waist, AT)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(AT ~ Waist) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(wc_at))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = wc_at, aes(x = Waist, y = AT)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = wc_at, aes(x=Waist, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(wc_at,aes(Waist,AT))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(Waist); y = AT

plot(log(Waist), AT)
cor(log(Waist), AT)

reg_log <- lm(AT ~ log(Waist))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(wc_at))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = Waist and y = log(AT)

plot(Waist, log(AT))

cor(Waist, log(AT))

reg_exp <- lm(log(AT) ~ Waist)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = wc_at$AT - at
error

sqrt(sum(error^2)/nrow(wc_at))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(Waist, AT)
plot(Waist*Waist, AT)

cor(Waist*Waist, AT)

plot(Waist*Waist, log(AT))

cor(Waist, log(AT))
cor(Waist*Waist, log(AT))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(AT) ~ Waist + I(Waist*Waist))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = wc_at$AT - expy

sqrt(sum(err^2)/nrow(wc_at))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = wc_at, aes(x = Waist + I(Waist^2), y = log(AT))) + 
     geom_point(color='blue') +
     geom_line(color='red',data = wc_at, aes(x=Waist+I(Waist^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(AT)~Waist + I(Waist*Waist) + I(Waist*Waist*Waist))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = wc_at, aes(x = Waist + I(Waist^2) + I(Waist^3), y = AT)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = wc_at, aes(x=Waist+I(Waist^2)+I(Waist^3), y=expy3))

################################
