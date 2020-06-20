#1- sample sign test


library(readxl)
Marks_data <- read_excel("E:/Day Wise/Day 08 Hypothesis Testing/Data/Marks-1sample sign test.xlsx")
View(Marks_data)
attach(Marks_data)

#normality test
#ho: Data are normal
#ha: Data are non- normal

shapiro.test(Marks)# p - value =0.0008013, reject Ho, Data are non- normal


#historical median=82

#ho: current median=82
#h1: Current median!=82

#case1
library(BSDA)
SIGN.test(Marks,md = 82, alternative = "two.sided",
          conf.level = 0.95)
#p value=0.1153 , p high Ho fly, fail to reject Ho 
#Current median is equal to historic median
