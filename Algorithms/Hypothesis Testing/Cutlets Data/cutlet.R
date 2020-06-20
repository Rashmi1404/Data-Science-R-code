#Hypothesis Testing

#h0 : = (The Diameter of cutlets in unit A and unit B are same) (Means are equal)
#ha : != (The Diameter of cutlets in unit A and unit B are not same) (Means are not equal)
#Import Cutlet.csv

cutlet <- read.csv("C:/Users/Rashmi/Desktop/Data Science/ASSIGNMENTS/Hypothesis Testing/Cutlets.csv")
cutlet_unit1<- cutlet[,1]
cutlet_unit2 <- cutlet[,2]

summary(cutlet)
boxplot(cutlet)

t.test(cutlet_unit1,cutlet_unit2, alternative = "two.sided") #t_test determine whether the means of two groups are equal to each other
#alternative hypothesis:true difference in means is not equal to 0
#There is a significant difference in the diameter of the cutlets in unit A and unit B