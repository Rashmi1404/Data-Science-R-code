UN <- read.csv("C:/Users/Rashmi/Desktop/ExcelR/R/DataSets/Universities.csv")
View(Universities)
#####################################

mydata <- scale(UN[,2:7])
d <- dist(mydata,method = "euclidean") #Computing the distance matrix
fit <- hclust(d, method =  "average") #Building the algorithm, try with 'centroid'
plot(fit) #display dendogram
groups <- cutree(fit, k=4) #cutting tree in 4 clusters
 # draw dendogram with red border around the 5 clusters
rect.hclust(fit, k=4, border = "red")
 # Attach the cluster number to Uni
cluster= data.frame('Uni'= UN[,1],'Clusters'=groups)

############################

fit <- hclust(d, method =  "centroid")
plot(fit)
groups <- cutree(fit, k=4)
