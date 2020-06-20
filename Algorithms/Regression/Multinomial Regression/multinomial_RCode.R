# Multinomail logit model
# Packages Required 
require("mlogit")
require("nnet")

data(Mode)
head(Mode)
View(Mode)

# Tabular representation of r variables
table(Mode$choice)

?Mode # Learn more about Mode data set

attach(Mode)
Mode.choice<-multinom(choice~cost.car+cost.carpool+cost.bus+cost.rail+time.car+time.carpool+time.bus+time.rail)
summary(Mode.choice)

Mode$choice<-relevel(Mode$choice,ref="carpool") # Change the baseline

# Significance of Regression Coefficients
z<-summary(Mode.choice)$coefficients/summary(Mode.choice)$standard.errors
p_value<-(1-pnorm(abs(z),0,1))*2

summary(Mode.choice)$coefficients
p_value

#Odds Ratio
exp(coef(Mode.choice))

# Predicted Probabilities
prob<-fitted(Mode.choice)
prob

# Intercation term - should be known through the EDA
summary(multinom(choice~cost.car+cost.carpool+cost.bus+cost.rail+time.car+time.carpool+time.bus+time.rail+cost.car*cost.carpool,data=Mode))

# The better is the model the least is it's AIC value
# So we choose the model the one we have prepared first i.e without interaction term

