PCA<-read.csv("/Volumes/Data/Course Content/DS content/PCA/PCA.csv")

pca<-princomp(PCA[,2:7], cor = TRUE,scores = TRUE, covmat = NULL)
summary(pca)
pca$scores
pca$loadings

plot(pca$scores[,1:2],col="Blue",pch=18,cex = 0.3, lwd = 3)
text(pca$scores[,1:2], labels=c(1:25), cex= 1)
