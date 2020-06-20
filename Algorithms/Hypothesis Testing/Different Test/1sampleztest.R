# 1 sample Z test
install.packages("readxl")
library(readxl)
Fabric_data <- read_excel("E:/Day Wise/Day 08 Hypothesis Testing/Data/Fabric data.xlsx")
View(Fabric_data)

#normality test
#Ho:data are normal
#Ha: data are non-normal
attach(Fabric_data)

shapiro.test(Fabric_length)# p-value= 0.1461
#p high Ho fly=> data are normal

#Population std deviation is known= 4

# 1- sample z test
#install.packages("BSDA")
library(BSDA)

#case1
z.test(Fabric_data$Fabric_length, alternative = "two.sided", mu = 150, sigma.x = 4,conf.level = 0.95)

#case2
z.test(Fabric_data$Fabric_length, alternative = "greater", mu = 150, sigma.x = 4,conf.level = 0.95)


