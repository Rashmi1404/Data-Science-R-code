
############## 1 sample z test ##########
fabric=read.csv(file.choose())
fabric
##### Normality Test##################
library(nortest)
ad.test(fabric$Fabric_length)      ###Anderson-Darling test

########### 1 sample z-test ##########
z.test(fabric$Fabric_length, alternative = "two.sided", mu = 0, sigma.x = 4,
       sigma.y = NULL, conf.level = 0.95)

############## 1 sample t test ##########
bolt_d=read.csv(file.choose())
##### Normality Test##################
library(nortest)
ad.test(bolt_d$Diameter)      ###Anderson-Darling test
########### 1 sample t-test ##########
t.test(bolt_d$Diameter, mu = 0, alternative = "two.sided")

############2 sample T Test ##################
Promotion<-read_excel(file.choose())    # Promotion.xlsx
attach(Promotion)
colnames(Promotion)<-c("Credit","Promotion.Type","InterestRateWaiver","StandardPromotion")
# Changing column names
View(Promotion)
attach(Promotion)

#############Normality test###############

shapiro.test(InterestRateWaiver)
# p-value = 0.2246 >0.05 so p high null fly => It follows normal distribution

shapiro.test(StandardPromotion)
# p-value = 0.1916 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############

var.test(InterestRateWaiver,StandardPromotion)#variance test
# p-value = 0.653 > 0.05 so p high null fly => Equal variances

############2 sample T Test ##################

t.test(InterestRateWaiver,StandardPromotion,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.02523 < 0.05 accept alternate Hypothesis 
# unequal means

?t.test
t.test(InterestRateWaiver,StandardPromotion,alternative = "greater",var.equal = T)

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (InterestRateWaiver-StandardPromotion) < 0
# Alternative Hypothesis -> (StandardPromotion - InterestRateWaiver) > 0
# p-value = 0.01211 < 0.05 => p low null go => accept alternate hypothesis
# InterestRateWaiver better promotion than StandardPromotion


###################Proportional T Test(JohnyTalkers data)##########

Johnytalkers<-read_excel(file.choose())   # JohnyTalkers.xlsx
View(Johnytalkers) 
attach(Johnytalkers)
table1 <- table(Icecream,Person)
table1
?prop.test
prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions of Adults and children under purchased
# p-value = 6.261e-05 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 

prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "less")
# Ha -> Proportions of Adults > Proportions of Children
# Ho -> Proportions of Children > Proportions of Adults
# p-value = 0.999 >0.05 accept null hypothesis 
# so proportion of Children > proportion of children 
# Do not launch the ice cream shop


#########Chi Square(Bahaman Research)#################

Bahaman<-read_excel(file.choose()) # Bahaman.xlsx
View(Bahaman)
attach(Bahaman)
table(Country,Defective)
chisq.test(table(Country,Defective))
# p-value = 0.6315 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 

############# Anova (Contract_Renewal Data )##########
CRD<-read.csv(file.choose())   # ContractRenewal_Data(unstacked).xlsx
View(CRD)
Stacked_Data <- stack(CRD)
View(Stacked_Data)
attach(Stacked_Data)

#############Normality test###############
library(nortest)
ad.test(Stacked_Data$values) 

############# Variance test ###############
library(car)
leveneTest(Stacked_Data$values~Stacked_Data$ind, data = Stacked_Data)   #Test for equal Variance

################ One-way Anova ########
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 0.104 > 0.05 accept null hypothesis 
# All Proportions all equal 

# Customer order form 
# Unstacked data 

cof<-read_excel(file.choose()) # customer_order(unstacked).xlsx
View(cof) # countries are in their own columns; so we need to stack the data 
stacked_cof<-stack(cof)
attach(stacked_cof)
View(stacked_cof)
table(stacked_cof$ind,stacked_cof$values)
chisq.test(table(stacked_cof$ind,stacked_cof$values))

########## Non-parametric test ########
############ 1 sample sign-test ######
student_s=read.csv(file.choose())
dat
######## Normality Test ############
library(nortest)
ad.test(student_s$Scores)    ###Anderson-Darling test

############ 1 sample sign-test ##########
library(BSDA)
SIGN.test(student_s$Scores, y = NULL, md = 0, alternative = "two.sided",conf.level = 0.95)

############ Mann-Whitney Test ########
additive=read.csv(file.choose())
additive
######## Normality Test ############
library(nortest)
ad.test(additive$Without.additive)      ###Anderson-Darling test
ad.test(additive$With.Additive) 

########## Mann-Whitney Test ########
wilcox.test(additive$Without.additive,additive$With.Additive,mu=0,alt="two.sided",conf.int=T,conf.level=0.95,paired=F,exact=T,correct=T)

################# Mood's Median Test #################
install.packages("RVAideMemoire")
library(RVAideMemoire)
height <- read.csv(file.choose())
height
attach(height)
mood.medtest(Growth ~ Treatment,data = Data,exact = FALSE)

############### Kruskal Wallis #################
kruskal.test(Growth,Treatment)